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
		#' @field models (list)
		#' List of the models created in training.
		models = list(),
		
    #' @description 
		#' Create a new MM_MoE object.
		#' @param config (MM_Config)\cr
		#' Configuration object, specifying how the model should be constructed.
    #' @param model_type (character)\cr
		#' Type of model - "CLASSIF" for classification or "SURV" for survival analysis. 
		#' @param decision (character)\cr
		#' Type of prediction - 'response' or 'prob'.
		#' @param subset (integer)\cr
		#' @param concat (logical(1))\cr
		#' Should the tasks be concatenated to form a single, large dataset?
		#' @param balance (logical(1))\cr
		#' Should the tasks be balanced during training?
		#' @param validate (logical(1))\cr
		#' Should the model be validated with validation data provided in the config file.
		#' @param filter_zeroes (double(1))\cr
		#' Features with this percentage of zero values or greater will not be included in the model.
		#' @param filter_missings (double(1))\cr
		#' Features with this percentage of missing values or greater will not be included in the model.
		#' @param filter_corr (double(1))\cr
		#' Should correlated features be included in the model? If FALSE, one feature from each correlated pair is eliminated.
		#' @param filter_var (double(1))\cr
		#' Should low variance features be included in the model?
    #' @return A new [MM_MoE] object.
		#' @export
		initialize = function(config, model_type = "VOTE", decision = "prob", subset = NULL, balance = FALSE, validate = FALSE, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
			super$initialize(config, "CLASSIF", decision, subset, FALSE, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)	
					
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


		#' @description 
		#' Train and test the mix of experts model.
		#' @param config (MM_Config)\cr
		#' Configuration object, specifying how the model should be constructed.
		#' @return An MM_Results object containing the model results.
		#' @export
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
						final_response[[rep]] = dplyr::bind_rows(fold_responses)
						final_response[[rep]] = final_response[[rep]] %>% arrange(id)  # Sort so that all match up
						final_response[[rep]][, "rpt"] = rep
						rm(fold_responses)
					}
					
					# Combine the responses for each repeat into a single data frame
					list("expert_responses" = dplyr::bind_rows(final_response), "features" = self$results)
#					expert_responses[[cls_idx]] = dplyr::bind_rows(final_response)
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