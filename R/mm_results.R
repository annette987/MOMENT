#' R6 Class to store the results for the multi-modal models
#'
#' @description
#' A container for the different types of results returned by the multi-modal models - \cr
#' performance, roc measures, selected features, feature stability and predictions.
#'
#' @name MM_Results
NULL

MM_Results = R6::R6Class("MM_Results", 
	public = list(
		roc = NULL,
		perf = NULL,
		stab = NULL,
		feats = NULL,
		predn = NULL,
		plots   = NULL,
		classes = NULL,
		task_desc = NULL,
		model_type = NULL,
		decision = NULL,
			
    #' @description 
		#' Create a new MM_Results object.
		#' @param classes (factor)\cr
		#' The classes that can be found in the target variable.
		#' @param tasks (list)\cr
		#' A list of the mlr tasks in the model.
		#' @param measures (list)\cr
		#' A list of the performance measures being used to assess the model's performance.
		#' @param model_type (character)\cr
		#' The type of model - Classification ("CLASSIF") or Survival Analisis ("SURV").
		#' @param decision (character)\cr
		#' The type of prediction made - “response” or “prob.
    #' @return A new `MM_Results` object.
		#' @export
		initialize = function(classes, tasks, measures, model_type = "CLASSIF", decision = "response")
		{
			checkmate::assertChoice(model_type, choices = c("CLASSIF", "SURV"))
			self$classes = classes
			self$task_desc = tasks[[1]]$task.desc
			self$predn = Prediction$new()
			self$roc = ROCMultiClass$new()
			self$perf = Performance$new(measures)
			self$stab = Stability$new(classes)			
			self$feats = Features$new(tasks)
			self$model_type = model_type
			self$decision = decision
		},

		
    #' @description 
		#' Save the predicted responses from training and predicting on one fold of data, in the MM_Results container.
		#' @param responses (data.frame)\cr
		#' The predicted results to be saved.
		#' @param rpt (integer)\cr
		#' The repeat number in the repeated cross-fold validation.
		#' @param fold (integer)\cr
		#' The fold number in the repeated cross-fold validation.
    #' @return Nothing.
		#' @export
		save_responses = function(responses, rpt, fold)
		{
			stopifnot(self$model_type != "SURV")
			stopifnot(inherits(responses, "data.frame") && (all(c("response", "truth") %in% colnames(responses))) && self$model_type != "SURV")
			responses$rpt = rpt
			responses$fold = fold
			private$responses = rbind(private$responses, responses)	
			pred_resp = self$make_mlr_prediction(responses, self$task_desc, self$decision, "PredictionClassif")
			self$perf$calculate(pred_resp, self$tasks[[1]])
			self$roc$calc(responses$truth, responses$response, as.list(self$classes))
		},

		
    #' @description 
		#' Save the predictions made by training and predicting on one fold of data, in the MM_Results container.
		#' @param pred (mlr::Prediction)\cr
		#' An mlr Prediction object containing the predictions to be saved.
		#' @param task (mlr::Task)\cr
		#' The mlr task that produced the predictions.
		#' @param model (object)\cr
		#' The mlr model that produced the predictions.
    #' @return Nothing.
		#' @export		
		save_predictions = function(pred, task = NULL, model = NULL)
		{
			if (self$model_type == "SURV") {
				predn_class = "PredictionSurv"
				check_cols  = c("response", "truth.time", "truth.event")
			} else {
				predn_class = "PredictionClassif"
				check_cols  = c("response", "truth")
			}
			
			stopifnot(all(check_cols %in% colnames(pred$data)))
			private$responses = rbind(private$responses, pred$data)			
			self$perf$calculate(pred, task, model)
			if (self$model_type != "SURV") {
				self$roc$calc(pred$data$truth, pred$data$response, as.list(self$classes))
			}
		},

		
    #' @description 
		#' Save the features selected in training on one fold of data, in the MM_Results container.
		#' @param model (object)\cr
		#' The mlr model that produced the predictions.
		#' @param task (mlr::Task)\cr
		#' The mlr task that produced the predictions.
		#' @param method (character)\cr
		#' The name of the method used. Forms part of the column name in the output.
		#' @param fold_num (integer)\cr
		#' The fold number in the repeated cross-fold validation.
    #' @return Nothing.
		#' @export				
		save_features = function(model, task, method, fold_num)
		{
			self$feats$save(model, task, self$classes, method, fold_num)
		},

		
    #' @description 
		#' Wrap the supplied results in an mlr Prediction object, for use by other mlr functions.
		#' @param results (data.frame)\cr
		#' A data.frame containing the results.
		#' @param task_desc (mlr::TaskDesc)\cr
		#' An mlr task description object.
		#' @param decision (character)\cr
		#' The type of prediction - "prob" or "response".
		#' @param predn_class (character)\cr
		#' The class of the results.
    #' @return An mlr Prediction object.
		#' @noRd				
		make_mlr_prediction = function(results, task_desc, decision = "response", predn_class = "PredictionClassif") {
			p = BBmisc::makeS3Obj(c(predn_class, "Prediction"),
				predict.type = decision,
				data = results,
				hv_threshold = NA_real_,
				task.desc = task_desc,
				time = NULL,
				error = NULL,
				dump = NULL
			)
			return(p)
		},		

		
    #' @description 
		#' Finalise the results, ready for inspection by the user.
		#' This function should only be called once training and testing is complete on all folds.
		#' The overall area under the receiver operating curve is calculated. 
		#' The features are saved and the stability metrcs are also calculated.
    #' @return Nothing.
		#' @export				
		complete = function()
		{
			if (self$model_type != "SURV") {
				self$roc$calc_mc_roc(as.factor(private$responses$truth), as.factor(private$responses$response))
			}
			self$stab$save_all(self$model_type, self$feats$featsel)
#			self$predn$save(private$responses)
		},	

		
    #' @description 
		#' Write all results to disk, in separate files.
		#' @param result_file_prefix (character)\cr
		#' 	The prefix of the name of the files to which the results will be written.
		#' @param suffix (character)\cr
		#' 	The suffix will be appended to the prefix of the output file name.
    #' @return Nothing.
		#' @export				
		write = function(result_file_prefix, suffix = NULL)
		{
			if (self$model_type != "SURV") {
				self$roc$write(result_file_prefix, suffix)
				self$roc$plot("TITLE", result_file_prefix)
			}
			self$perf$write(result_file_prefix, suffix)
			self$feats$complete()
			self$feats$write(result_file_prefix, suffix)
			self$stab$write(result_file_prefix, suffix)
#			self$predn$write(result_file_prefix, suffix)
		},

		
		#' @description Return the stability results.
		#' @return data.frame
		#' @export	
		get_stability = function() {
			return(self$stab$get_results())
		},
		
		
		#' @description Return the performance results.
		#' @return data.frame
		#' @export	
		get_performance = function() {
			return(self$perf$get_results())
		},
		
		
		#' @description Return the predictions made.
		#' @return data.frame
		#' @export			
		get_predictions = function() {
			return(self$predn$get_results())
		},
		
		
		#' @description Return the features selected.
		#' @return A character vector
		#' @export			
		get_features = function() {
			return(self$feats$get_results())
		},
		
		
		#' @description 
		#' Return the ROC measures - Accuracy, F1 score, Sensitivity, Specificity, Overall AUROC.
		#' @return list
		#' @export			
		get_roc_measures = function() {
			return(list("roc" = self$roc$get_results(), "auroc" = self$roc_get_auroc()))
		}
	),
	
	private = list(	
		responses = NULL
	)

)