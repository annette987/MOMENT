#' R6 Class representing a multi-modal mix of experts model
#'
#' @description
#' Creates a multi-modal mix of experts model 
#'
#' @details
#' Trains an expert classifier on each class 
#' then combines the predictions from those experts using a
#' gating function to give a final prediction.
#'
#' @name MM_MoE
#' @docType package
NULL


MM_MoE = R6::R6Class("MM_MoE", 
	inherit = MM_Model,
	public = list(
		models = list(),
		
    #' @description 
		#' Create a new MM_MoE object.
    #' @param config Model parameters (MM_Config).
    #' @return A new `MM_MoE`object.
		#' @examples
		#' mod = MM_MoE$new(config)
		#' @export
		initialize = function(config, model_type = "VOTE", decision = "prob", subset = NULL, balance = FALSE, validate = FALSE, filter_zeroes = FALSE, filter_missings = FALSE, filter_corr = FALSE, filter_var = FALSE) {
			super$initialize(config, model_type, decision, subset, FALSE, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)	
					
			ovr_data = self$create_ovr_tasks(config$dataDir, config, self$task_type, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
			self$tasks = ovr_data$tasks
			self$ovr_classes = ovr_data$classes
			resamp = mlr::makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
			self$ri = mlr::makeResampleInstance(resamp, self$tasks[[1]][[1]])

			if (validate) {
				self$vtasks = create_validation_tasks(self$tasks, config$dataDir, config, self$task_type, ovr_class, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
				vdat = getTaskData(self$vtasks[[1]])
				self$ri_v = makeFixedHoldoutInstance(c(1:nrow(dat)), c((nrow(dat) + 1):nrow(vdat)), nrow(vdat))
				self$vresults = MM_Results$new(self$classes, self$vtasks)
			}
			
			for (cls in names(self$ovr_classes)) {
				if (self$model_type == "VOTE") {
					self$models[[cls]] = MM_Voting$new(NULL)
					self$models[[cls]]$clone_model(self, self$tasks[[cls]], self$ovr_classes[[cls]])
				} else if (self$model_type == "ADA") {
					self$models[[cls]] = MM_Adaboost$new(NULL)	
					self$models[[cls]]$clone_model(self, self$tasks[[cls]], self$ovr_classes[[cls]])		
				}
			}
#			self$results = MM_Results$new(self$classes, self$tasks[[1]], self$measures, self$model_type, self$decision)
		},		

		learn = function(config) 
		{  
			# Classes form the outer loop and a resampling instance is made for each class.
			# These have to be unique to each class as each class can be balanced separately and so may have different indices.
			# Therefore, you have to run all folds of each class and then put them together at the end.
			
			expert_responses = list()
			expert_futures = list()
			
			for (cls_idx in 1:length(self$ovr_classes)) {		
				final_response = list()
				
				expert_futures[[cls_idx]] = future::future({
				
					for (rep in 1:self$ri$desc$reps) {
						fold_responses = list()
						for (fold in 1:self$ri$desc$folds) {
							subset_idx = (rep - 1) * self$ri$desc$folds + fold
							train_subset = self$ri$train.inds[[subset_idx]]
							test_subset = self$ri$test.inds[[subset_idx]]
							
							# Train and predict the individual experts in parallel
							# NB Add futures to make it parallel	
#							dat = mlr::getTaskData(ovr_tasks[[cls_idx]][[1]])
#							sample_ids = data.frame('id' = test_subset, 
#																			'ID' = row.names(dat[test_subset, ]), 
#																			label = dat[test_subset, config$targetVar])
#							rm(dat)
#							predns = sample_ids
#							rownames(predns) <- predns$ID

							if (self$model_type == "VOTE") {
								self$models[[cls_idx]]$train(train_subset, rep, fold)
								predns = self$models[[cls_idx]]$predict(test_subset, self$decision, rep, fold)
							} else if (self$model_type == "ADA") {
								self$models[[cls_idx]]$train(train_subset)
								predns = self$models[[cls_idx]]$predict(test_subset)$data
							} else {
								stop("UNKNOWN model_type")
							}
							predns$fold = subset_idx
							fold_responses[[fold]] = predns
							rm(predns)
						}			
					
						# Combine the responses for each fold for this repeat
						final_response[[rep]] = bind_rows(fold_responses)
						final_response[[rep]] = final_response[[rep]] %>% arrange(id)  # Sort so that all match up
						final_response[[rep]][, "rpt"] = rep
						rm(fold_responses)
					}
					
					# Combine the responses for each repeat into a single data frame
					list("expert_responses" = bind_rows(final_response), "features" = self$results)
#					expert_responses[[cls_idx]] = bind_rows(final_response)
				})	
			}

			future::resolve(expert_futures)	
			for (idx in 1:length(expert_futures)) {
				val = future::value(expert_futures[[idx]])
				expert_responses[[idx]] = val$expert_responses
#				self$results$feats[[idx]] = val$features
			}	

			# Create a summary of the responses from each expert
			if (length(expert_responses) >= 1) {
				summary_responses = expert_responses[[1]][, c('id', 'ID', 'rpt', 'fold')]  # Relies on each expert being in same order
				for (cls_idx in 1:length(expert_responses)) {
					# Don't use match here to match to ID as there are multiple of each ID
					truth = as.character(expert_responses[[cls_idx]][, 'truth'])
					summary_responses[truth != "REST", 'truth'] = truth[truth != "REST"]
					summary_responses[, paste("Expert", cls_idx)] = as.character(expert_responses[[cls_idx]][, 'response'])

				}
				response_posns = as.list(apply(summary_responses[, grepl("Expert", colnames(summary_responses))], 1, function(x) {which(x != "REST")}))
			}
					
			# Aggregate the responses from all the experts
			# If more than one expert gives a valid prediction (i.e. predicts the class it is an expert on, not REST), 
			# then accept the one that predicts with the highest confidence
			prob_res = do.call(cbind, expert_responses)
			for (i in 1:length(response_posns)) {  # For each row
				prob_row = prob_res[i, grepl('prob.', colnames(prob_res)) & !grepl('REST', colnames(prob_res)), drop = FALSE]
				if (length(response_posns[[i]]) > 0) {
					prob_row = prob_row[, unlist(response_posns[[i]]), drop = FALSE]
				} else {
					warning("All predicted REST - choose highest confidence prediction")
				}

				if (ncol(prob_row) > 0) {
					max_posn = as.vector(apply(prob_row, 1, which.max))
					max_predn = strsplit(colnames(prob_row)[max_posn[1]], ".", fixed = TRUE)  # Take the first one if there are more than one
					max_predn = sapply(max_predn, "[[", 2)
					summary_responses[i, 'response'] = max_predn
				} else {
					summary_responses[i, 'response'] = "Unknown"
				}
			}

			# Save and plot results
			for (rpt in 1:self$ri$desc$reps) {
				for (fold in 1:self$ri$desc$folds) {
					idx = (rpt - 1) * self$ri$desc$folds + fold
					roc_rpt = summary_responses[summary_responses$fold == idx, ]
					roc_rpt[roc_rpt == "Unknown"] = "REST"
					self$results$save_responses(roc_rpt, rpt, fold)
				}
			}
			self$results$complete("Mix of Experts")  # Needs summary responses saved
			
			for (cls_idx in 1:length(self$classes)) {
					self$models[[cls_idx]]$results$complete("Mix of Experts")
			}				
			return(self$results)
		}
	)
)