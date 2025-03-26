#' MM_Voting: R6 Class representing a multi-modal voting ensemble classifier
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
NULL

MM_Voting = R6::R6Class("MM_Voting", 
	inherit = MM_Model,
	public = list(
		
    #' @description 
		#' Create a new MM_Voting object.
		#' @param config (MM_Config)\cr
		#' Configuration object, specifying how the model should be constructed.
		#' @param task_type (character)\cr
		#' Type of model - "classif" for classification, "multilabel" for multilabel classification or "surv" for survival analysis. 
		#' @param decision (character)\cr
		#' Type of decision in combining modalities - "prob" or "soft" for soft voting, "vote" or "hard" for hard voting, "meta for meta learning
		#' @param subset (integer)\cr
		#' A vector of integers specifying the indices of the modalities to be included in the model.
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
		#' @return A new`MM_Voting` object.
		#' @export
		initialize = function(config, task_type, decision = "prob", subset = NULL, balance = FALSE, validate = FALSE, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
			pred_type = ifelse(decision %in% c("prob", "soft"), "prob", "response")
			super$initialize(config, task_type, pred_type, decision, subset, FALSE, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)
		},		


		#' @description 
		#' Calculate the final response, according to the decision_type type and
		#' add a response column to the results.
    #' @param results (data.frame)\cr
		#' The results from the individual models, concatenated column-wise into a data.frame.
		#' @param classes
		#' The classes to be found in the target variable.
    #' @return A data.frame containing the results including the final decision.
		#' @noRd
		get_final_decision = function(results, classes) 
		{
			print("In get_final_decision")
			print(classes)
			print(self$task_type)
#			print(head(results))
#			results$truth = as.factor(results$truth)

			if (self$decision %in% c('vote', 'hard')) {
				# Calculate final prediction with a majority vote across classes/labels
				raw_responses = results[, grepl("^response", colnames(results)), drop = FALSE]
				if (self$task_type != 'multilabel') {
					results$response = as.factor(apply(raw_responses, 1, function(x) names(which.max(table(x)))))	
				} else {
					for (i in 1:length(classes)) {
						raw_responses = results[, grepl(paste0("\\<", classes[i], "\\>"), colnames(results)), drop = FALSE]
						results[, paste0('response.', classes[i])] = as.logical(apply(raw_responses, 1, function(x) names(which.max(table(x)))))
					}
				}
				
			} else if (self$decision %in% c('prob', 'soft')) {
				# Calculate average of probabilities for each class/label 
				# For classification, final response is max probability
				# For multilable classification, apply a threhold to the response fo reach label to get TRUE/FALSE
				
				for (i in 1:length(classes)) {
					tmp = results[, grepl(paste0("\\<", classes[i], "\\>"), colnames(results)), drop = FALSE]
					prob = rowSums(tmp, na.rm = TRUE) / ncol(tmp)
					
					if (self$task_type == 'multilabel') {
						results[, paste0('response.', classes[i])] = ifelse(prob >= 0.5, TRUE, FALSE)
					} else {
						results[, paste0('response.', classes[[i]])] = prob
					}
				}
				if (self$task_type != "multilabel") {
					results$response = apply(results[, grepl("response.", colnames(results))], 1, function(x) names(which.max(x)))
					results$response = strsplit(as.character(results$response), ".", fixed = TRUE)
					results$response = as.factor(sapply(results$response, "[[", 2))
					levels(results$response) = levels(results$truth)
				}
			}
			
			print("After final decision:")
			print(head(results))
			return(results)
		},


		#' @description 
		#' Train a model for each modality in parallel. 
		#' Save the features selected by each model.
    #' @param training_set (integer)\cr
		#' A vector of indices indicating which samples should be used in training.
		#' @param rpt (integer)\cr
		#' Which repeat in the repeated cross-validation is being trained.
		#' @param fold
		#' Which fold in the repeated cross-validation is being trained.
    #' @return Nothing
		#' @export
		train = function(training_set, rpt, fold) 
		{
			print("Training")
			model_futures = list()			
			for (i in 1:length(self$tasks)) {
				model_futures[[i]] = future::future(mlr::train(learner = self$learners[[i]], task = self$tasks[[i]], subset = training_set), seed = TRUE, conditions = character(0))
			}
			future::resolve(model_futures)
			
			for (i in 1:length(model_futures)) {
				task_id = mlr::getTaskId(self$tasks[[i]])
				print(task_id)
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
		#' @param decision (character)\cr
		#' Type of decision to make in combining predictions. 
		#' Can be 'vote' or 'hsrd' for a hard vote or 'prob' or 'soft' for a soft vote.
    #' @param test_set (integer)\cr
		#' A vector of indices indicating which samples should be used in testing.
		#' @param rpt (integer)\cr
		#' Which repeat in the repeated cross-validation is being tested.
		#' @param fold
		#' Which fold in the repeated cross-validation is being tested.
    #' @return Results of the predictions (data.frame)
		#' @export
		predict = function(test_set, decision, rpt, fold) 
		{
			print("Predicting")
			checkmate::assertInteger(test_set)
			checkmate::assertChoice(decision, choices = c('vote', 'hard', 'prob', 'soft'))
			checkmate::assertInteger(rpt)
			checkmate::assertInteger(fold)
			
			responses = NULL
			predn_futures = list()
			
			for (i in 1:length(self$tasks)) {
				print(paste0("i = ", i))
				predn_futures[[i]] = future::future(predict(private$models[[i]], self$tasks[[i]], subset = test_set), seed = TRUE, conditions = character(0))	
			}
			future::resolve(predn_futures)
			print("Futures resolved")

			for (i in 1:length(predn_futures)) {
				print(paste0("i = ", i))
				pred = future::value(predn_futures[[i]])
				print(colnames(pred$data))
				if (is.null(responses)) {
					truth_cols = colnames(pred$data)[grepl("^truth", colnames(pred$data))]
					responses = pred$data[, c('id', truth_cols)]
					responses$ID = rownames(pred$data)
				}
				
				search_str = ifelse((decision == 'vote') || (decision == 'hard'), "^response", "^prob")
				res = pred$data[, grepl(search_str, colnames(pred$data)), drop = FALSE]
				print(colnames(res))
				
				if (((decision == 'vote') || (decision == 'hard')) && (self$task_type != 'multilabel')) {
					responses[, paste("response.", mlr::getTaskId(self$tasks[[i]]))] = res[match(responses$ID, rownames(res)), , drop = FALSE]
				} else {
					res_cols = gsub(search_str, mlr::getTaskId(self$tasks[[i]]), colnames(res))
					res$ID = rownames(pred$data)
					responses[, res_cols] = res[match(responses$ID, res$ID), , drop = FALSE]
				}								
			}
			print(colnames(responses))
			
			responses = self$get_final_decision(responses, self$classes)
			self$results$save_responses(responses, rpt, fold)
			return(responses)
		},

						
  	#' @description 
		#' Validate all modalities for one fold of the test data. 
		#' Combine the validation predictions into a data.frame.
    #' @return Nothing
		#' @noRd
		validate = function() {
			vroc = ROCMultiClass$new()
			vperf = Performance$new(classfn.measures)
			vfeats = Features$new(vtasks)

			vresponses = train_base_mods(self$vtasks, learners, ri_v, 1, decision, classes, vfeats, vperf)
			vresponses = self$get_final_decision(vresponses, self$ovr_classes)
			self$vresults$save_responses(vresponses, 1, 1)
			vroc$calc_mc_roc(as.factor(vresponses$truth), as.factor(vresponses$response))

			vpred_resp = make_mlr_prediction(vresponses, mlr::getTaskDesc(vtasks[[1]]))
			vperf$calculate(vpred_resp$data)
			self$vresults$complete()
			return(self$vresults)
		},
		
  	#' @description 
		#' Training and prediction of a multi-modal Voting ensemble in a cross validated loop. 
		#' Perform validation if a validation set is provided. 
    #' @return An object of type [mm_results] containing the model results.
		#' @export
		learn = function() 
		{
			print("Learning")
			for (rpt in 1:self$ri$desc$reps) {
				for (fold in 1:self$ri$desc$folds) {
					subset_idx = (rpt - 1) * self$ri$desc$folds + fold
					print(paste0("Subset idx = ", subset_idx))
					training_set = self$ri$train.inds[[subset_idx]]
					test_set = self$ri$test.inds[[subset_idx]]

					self$train(training_set, rpt, fold)
					predns = self$predict(test_set, self$decision, rpt, fold)
					print("Finished fold")
				}
			}
			
			if (self$validation) {
				self$validate()
			}
			
			self$results$complete()
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