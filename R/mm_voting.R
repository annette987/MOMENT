#' R6 Class representing a multi-modal voting ensemble classifier
#'
#' @description
#' Creates a multi-modal ensemble classifier that uses 
#' hard or soft voting to determine the final outcome of the classifier.
#'
#' @details
#' Trains a classifier on each modality then combines the
#' predictions from those modalities using a hard vote
#' (majority voting) or a soft vote (average of probabilities)
#' to give a final prediction.
#'
#' @name MM_Voting
#' @docType package
NULL

MM_Voting = R6::R6Class("MM_Voting", 
	inherit = MM_Model,
	public = list(
		
    #' @description 
		#' Create a new MM_Voting object.
    #' @param config Model parameters (MM_Config).
    #' @return A new `MM_Voting`object.
		#' @examples
		#' mod = MM_Voting$new(config)
		#' @export
		initialize = function(config, decision = "prob", subset = NULL, balance = FALSE, validate = FALSE, filter_zeroes = FALSE, filter_missings = FALSE, filter_corr = FALSE, filter_var = FALSE) {
			super$initialize(config, "VOTE", decision, subset, FALSE, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)
		},		


		# Calculate the final response, according to the decision_type type and
		# add a response column to the results
		#
		get_final_decision = function(results, classes) 
		{
			results$truth = as.factor(results$truth)
			if (self$decision %in% c('vote', 'hard')) {
				# Calculate final prediction with a majority vote across modalities
				raw_responses = as.data.frame(results[,!colnames(results) %in% c('id', 'ID', 'truth')])
				results$response = as.factor(apply(raw_responses, 1, function(x) names(which.max(table(x)))))	
			} else if (self$decision %in% c('prob', 'soft')) {
				# Calculate sum of probabilities for each class and take max of that as prediction
				for (i in 1:length(classes)) {
					results[, paste0('prob.', classes[[i]])] = rowSums(as.data.frame(results[, grepl(paste0("\\<", classes[[i]], "\\>"), colnames(results))]), na.rm = TRUE)
				}
				results$response = apply(results[, grepl("prob.", colnames(results))], 1, function(x) names(which.max(x)))
				if (!is.null(results$response)) {
					results$response = strsplit(as.character(results$response), ".", fixed = TRUE)
					results$response = as.factor(sapply(results$response, "[[", 2))
				} else {
					message("NULL response")
					message(results)
				}
			}
			
			levels(results$response) = levels(results$truth)
			return(results)
		},

		#' @description 
		#' Train a model for each modality in parallel. 
		#' Save the features selected by each model..
    #' @param training_set
		#' @param rpt
		#' @param fold
    #' @return Nothing
		#' @examples
		#' train(training_set, 1, 2)
		#' @export
		train = function(training_set, rpt, fold) 
		{
			model_futures = list()			
			for (i in 1:length(self$tasks)) {
				model_futures[[i]] = future::future(mlr::train(learner = self$learners[[i]], task = self$tasks[[i]], subset = training_set), seed = TRUE)
			}
			future::resolve(model_futures)
			
			for (i in 1:length(model_futures)) {
				task_id = self$tasks[[i]]$task.desc$id
				private$models[[task_id]] = future::value(model_futures[[i]])
				
				if (mlr::isFailureModel(private$models[[task_id]])) {
					warning(paste0("Model ", task_id, " failed on repeat ", rpt, " fold ", fold))
					warning(mlr::getFailureModelMsg(private$models[[task_id]]))
				} else {
					self$results$save_features(private$models[[task_id]], self$tasks[[i]], "VOTE", fold)
				}
			}
		},
		
  	#' @description 
		#' Predict on new data with fitted models for each modality in parallel. 
		#' Combine the predictions into a data.frame.
    #' @param test_set
		#' @param decision
		#' @param rpt
		#' @param fold
    #' @return Results of the predictions (data.frame)
		#' @examples
		#' predict(training_set, 1, 2)
		#' @export
		predict = function(test_set, decision, rpt, fold) 
		{
			responses = NULL
			predn_futures = list()
			
			for (i in 1:length(self$tasks)) {
				predn_futures[[i]] = future::future(mlr:::predict.WrappedModel(private$models[[i]], self$tasks[[i]], subset = test_set), seed = TRUE)	
			}
			future::resolve(predn_futures)

			for (i in 1:length(predn_futures)) {
				pred = future::value(predn_futures[[i]])
				task_id = self$tasks[[i]]$task.desc$id

				if (is.null(responses)) {
					responses = pred$data[, c('id', 'truth')]
					responses$ID = rownames(pred$data)
				}
				
				if ((decision == 'vote') || (decision == 'hard')) {
					res = pred$data[, 'response', drop = FALSE]
					res$ID = rownames(pred$data)
					responses[, task_id] = res[match(responses$ID, res$ID), 'response']
				} else if ((decision == 'prob') || (decision == 'soft')) {
					probs = pred$data[, grepl("prob.", colnames(pred$data))]
					prob_cols = gsub("prob", task_id, colnames(probs))
					probs$ID = rownames(pred$data)
					responses[, prob_cols] = probs[match(responses$ID, probs$ID), grepl("prob.", colnames(probs)), drop = FALSE]
				}								
			}
			
			responses = self$get_final_decision(responses, self$classes)
			self$results$save_responses(responses, rpt, fold)
			return(responses)
		},
						
  	#' @description 
		#' Validate all modalities for one fold of the test data. 
		#' Combine the validation predictions into a data.frame.
    #' @return Nothing
		#' @examples
		#' @noRd
		validate = function() {
			vroc = ROCMultiClass$new()
			vperf = Performance$new(classfn.measures)
			vfeats = Features$new(vtasks)

			vresponses = train_base_mods(self$vtasks, learners, ri_v, 1, decision, classes, vfeats, vperf)
			vresponses = self$get_final_decision(vresponses, self$ovr_classes)
			self$vresults$save_responses(vresponses, 1, 1)
			vroc$calc_mc_roc(as.factor(vresponses$truth), as.factor(vresponses$response))

			vpred_resp = make_mlr_prediction(vresponses, vtasks[[1]]$task.desc)
			vperf$calculate(vpred_resp$data)
			self$vresults$complete("Voting Ensemble")
			return(self$vresults)
		},
		
  	#' @description 
		#' Training and prediction of a multi-modal Voting ensemble in a cross validated loop. 
		#' Perform validation if a validation set is provided. 
		#' Save the results to a file.
    #' @param validation_set
    #' @return mm_results
		#' @export
		learn = function() 
		{
			for (rpt in 1:self$ri$desc$reps) {
				for (fold in 1:self$ri$desc$folds) {
					subset_idx = (rpt - 1) * self$ri$desc$folds + fold
					training_set = self$ri$train.inds[[subset_idx]]
					test_set = self$ri$test.inds[[subset_idx]]

					self$train(training_set, rpt, fold)
					predns = self$predict(test_set, self$decision, rpt, fold)
				}
			}
			
			if (self$validation) {
				self$validate()
			}
			
			self$results$complete("Voting Ensemble")
			return(self$results)
		},
		
		#' @export
		reset = function() {
			rm(models)
			models = list()
		}
	),
	
	private = list(
		#' @field models (list)
		#' List of the models created in training.
		models   = list()	
	)
)