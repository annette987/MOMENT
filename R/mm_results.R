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
		#' Type of model - "classif" for classification, "multilabel" for multilabel classification or "surv" for survival analysis. 
		#' @param decision (character)\cr
		#' The type of prediction made - “response” or “prob.
    #' @return A new `MM_Results` object.
		#' @export
		initialize = function(classes, tasks, measures, model_type = "classif", decision = "response")
		{
			checkmate::assertChoice(model_type, choices = c("classif", "multilabel", "surv"))
			self$model_type = model_type
			self$decision = decision
			self$classes = classes
			self$task_desc = mlr::getTaskDesc(tasks[[1]])
			self$predn = Prediction$new()
			self$roc = ROCMultiClass$new()
			self$perf = Performance$new(measures)
			
			if (model_type == 'multilabel') {
				self$feats = list()
				self$stab = list()
				labels = mlr::getTaskTargetNames(tasks[[1]])
				for (lbl_idx in labels) {
					self$feats[[lbl_idx]] = Features$new(tasks)
					self$stab[[lbl_idx]]  = Stability$new(classes)
				}
			} else {
				self$feats = Features$new(tasks)
				self$stab  = Stability$new(classes)			
			}
		},


		#' @description 
		#' Extract the feature importance scores from an mlr model.
		#' Convert the custom importance data from each model into a uniform format -
		#' a data.frame with one row per feature and one column per class, 
		#' plus a column labelled 'all', which gives the feature importance for the model as a whole.
		#' The 'all' column may contain zeroes if this data is not available.
		#' @param mod (???)\cr
		#' 	The mlr model
		#' @param class_names (character vector)
		#' 	Names of the classes in the data
		#' @return A data.frame containing feature importance scores
		#' @export
		getFeatImpScores = function(mod, class_names = NULL)
		{	
			imp_data = NULL
			if (is.null(mod)) {
				cat(paste("=======> Model is null!!\n"))		
			} else if (inherits(mod, "WrappedModel") && isFailureModel(mod)) {
				cat(paste("=======> Model is a FailureModel!!!\n"))
				cat(mlr::getFailureModelMsg(mod))		
			} else if (inherits(mod, "coxph")) {
				pvalues = summary(mod)$coefficients[,5]
				imp_data = as.data.frame(pvalues)
				colnames(imp_data) = c('all')
			} else if (inherits(mod, "CoxBoost")) {
				coef.nonzero = CoxBoost::coef(mod)
				imp_data = coef.nonzero
			} else if (inherits(mod, "glmboost")) {
				coef.nonzero = glmboost::coef(mod)
				imp_data  = coef.nonzero[2:length(coef.nonzero)]						# 1st column is Intercept - ignore this
			} else if (inherits(mod, "cv.glmnet")) {
				coef.min = glmnet::coef(mod, s = mod$lambda.min)
				# For binary class problems coef.min is a single dgCMatrix
				# For multi-class problems coef.min is a list of dgCMatrix, one per class
				if (inherits(coef.min, "dgCMatrix")) {
					coef.min = coef.min[!rownames(coef.min) %in% c("(Intercept)"), ]
					imp_data = data.frame('all' = coef.min)
				} else {
					imp_data = as.data.frame(do.call(cbind, lapply(coef.min, as.matrix)))
					imp_data = imp_data[!rownames(imp_data) %in% c("(Intercept)"), ]
					colnames(imp_data) = class_names[1:ncol(imp_data)]
					imp_data['all'] = rep(0, nrow(imp_data))
				}
				
			} else if (inherits(mod, "rfsrc")) {
				if (!is.null(mod$importance)) {
					imp_data = mod$importance
				} else {
					imp = randomForestSRC::vimp.rfsrc(mod)
					imp_data = imp$classOutput
				}
			} else if (inherits(mod, "randomForest")) {
				imp_data = mod$importance
				colnames(imp_data) = c('all')
			} else if (inherits(mod, "ranger")) {
				# N.B. This one needs to use local.importance but mlr doesn't allow it!
				ind = which(mod$variable.importance != 0)
				imp_data = as.data.frame(mod$variable.importance)
				rownames(imp_data) =  names(mod$variable.importance)
				imp_data = imp_data[ind, , drop = FALSE]
				colnames(imp_data) = c('all')
			} else if (inherits(mod, "xgb.Booster")) {
				if (mod$params$booster == BOOSTER_LINEAR) {
					imp_col = "Weight"
				} else {
					imp_col = "Gain"
				}
				if (mod$params$booster == BOOSTER_LINEAR) {
					imp = xgboost::xgb.importance(model = mod)
					if ('Class' %in% colnames(imp)) {
						imp_data = as.data.frame(imp %>% 
												tidyr::pivot_wider(names_from = "Class", 
																					 values_from = "Weight", 
																						names_prefix = "Class"))
						colnames(imp_data) = c("Feature", class_names[1:(length(class_names))])
					} else {
						imp_data = as.data.frame(imp)
					}
					
					rownames(imp_data) = imp_data$Feature
					imp_data$Feature = NULL
					imp_data['all'] = 0		# Not provided - no way to calculate
				} else {
					imp = xgboost::xgb.importance(model = mod)
					imp_data = as.data.frame(imp)[, c("Feature", imp_col)]
					colnames(imp_data) = c("Feature", "all")
					rownames(imp_data) = imp_data$Feature
					imp_data$Feature = NULL
				}
			} else if (inherits(mod, "svm")) {
					imp <- t(mod$coefs) %*% mod$SV                   				# weight vectors
					imp <- apply(imp, 2, function(v){sqrt(sum(v^2))})  	# weight
					imp <- sort(imp, decreasing = T)
					imp_data = data.frame("all" = imp)
			} else if (inherits(mod, "gbm")) {
					imp = gbm::summary.gbm(mod, method = gbm::relative.influence)
					colnames(imp) = c("var", "all")
					imp_data = data.frame('all' = imp[, "all"], row.names = imp[, "var"])
			} else if (inherits(mod, "rpart")) {
					imp_data = data.frame('all' = mod$variable.importance)
			} else if (inherits(mod, "naiveBayes")) {
			} else {
				warning(paste0("Unknown model class: ", class(mod)))
			}

			return(imp_data)
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
			stopifnot(inherits(responses, "data.frame") && self$model_type != "surv")
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
			if (self$model_type == "surv") {
				predn_class = "PredictionSurv"
				check_cols  = c("response", "truth.time", "truth.event")
			} else if (self$model_type == "multilabel") {
				predn_class = "PredictionMultilabel"
				check_cols  = c("response", "truth")  # WRONG - Need to look for truth.*, label.*? or response.*?
			} else {
				predn_class = "PredictionClassif"
				check_cols  = c("response", "truth")
			}
			
			stopifnot(all(check_cols %in% colnames(pred$data)))
			private$responses = rbind(private$responses, pred$data)			
			self$perf$calculate(pred, task, model)
			if (self$model_type == "classif") {
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
			scores = self$getFeatImpScores(mlr::getLearnerModel(model, more.unwrap = TRUE), self$classes)
			if (self$model_type == 'multilabel') {
				labels = mlr::getTaskTargetNames(task)
				for (lbl_idx in labels) {
					self$feats[[lbl_idx]]$save(scores[[lbl_idx]]$importance, model, task, method, fold_num)
				}
			} else {
				self$feats$save(scores, model, task, method, fold_num)
			}
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
			if (self$model_type == "classif") {
				self$roc$calc_mc_roc(as.factor(private$responses$truth), as.factor(private$responses$response))
			}
			
			if (self$model_type == 'multilabel') {
				labels = mlr::getTaskTargetNames(self$task_desc)
				for (lbl_idx in labels) {
					self$stab[[lbl_idx]]$save_all(self$model_type, self$feats[[lbl_idx]]$featsel)
				}
			} else {
				self$stab$save_all(self$model_type, self$feats$featsel)
			}
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
			print(paste0("Writing out results to ", result_file_prefix))
			self$perf$write(result_file_prefix, suffix)
			self$perf$plot(result_file_prefix)
#			self$predn$write(result_file_prefix, suffix)

			if (self$model_type == "classif") {
				self$roc$write(result_file_prefix, suffix)
				self$roc$plot("TITLE", result_file_prefix)
			}
			
			if (self$model_type == 'multilabel') {
				for (i in 1:length(self$feats)) {
					self$feats[[i]]$complete()
					self$feats[[i]]$write(paste(result_file_prefix, self$classes[[i]], sep = "_"), suffix)
					self$stab[[i]]$write(paste(result_file_prefix, self$classes[[i]], sep = "_"), suffix)
				}
			}
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