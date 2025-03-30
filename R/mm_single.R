#' R6 Class representing a classifier or survival analysis model for single modalities
#'
#' @description
#' Creates a classifier or or survival analysis model
#' for each individual modality.
#'
#' @details
#' Trains a classifier or survival analysis model on each modality.
#" Used as a comparison to the performance of the multi-modal methods 
#'
#' @name MM_Single
#' @docType package
NULL

MM_Single = R6::R6Class("MM_Single", 
	inherit = MM_Model,
	public = list(
		single_results = list(),
		
    #' @description 
		#' Create a new MM_Single object.
		#' @param config (MM_Config)\cr
		#' Configuration object, specifying how the model should be constructed.
		#' @param task_type (character)\cr
		#' Type of model - "classif" for classification, "multilabel" for multilabel classification or "surv" for survival analysis. 
		#' @param predict_type (character)\cr
		#' Type of prediction - "response" or "prob"
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
    #' @return A new `MM_Single`object.
		#' @export
		initialize = function(config, task_type = "classif", predict_type = "response", concat = FALSE, balance = FALSE, validate = FALSE, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
			super$initialize(config, task_type, predict_type, "hard", NULL, concat, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)
		},		

						
  	#' @description 
		#' Validate all modalities for one fold of the test data. 
		#' Combine the validation predictions into a data.frame.
    #' @return Nothing
		#' @noRd
		validate = function() {
			vroc = ROCMultiClass$new()
			vperf = Performance$new(classfn.measures)
			vfeats = MM_Features$new(vtasks)

			vresponses = train_base_mods(self$vtasks, learners, ri_v, 1, decision, classes, vfeats, vperf)
			vresponses = self$get_final_decision(vresponses, self$ovr_classes)
			self$vresults$save_predictions(vresponses$truth, vresponses$response, as.list(self$classes))
#			vroc$calc_mc_roc(as.factor(vresponses$truth), as.factor(vresponses$response))

#			vpred_resp = make_mlr_prediction(vresponses, mlr::getTaskDesc(vtasks[[1]]))
#			vperf$calculate(vpred_resp)
			self$vresults$complete()
			return(self$vresults)
		},
		
		get_model_results = function(res, task)
		{
			print("Getting results")
			results = MM_Results$new(self$classes, self$tasks, self$measures, self$task_type, self$decision)
			print("MM_Results initialised")
			
			for (i in 1:length(res$models)) 
			{
				print(paste0("i = ", i))
				results$save_predictions(res$pred, task, res$models[[i]])
				results$save_features(res$models[[i]], task, self$task_type, i)
			}
			results$complete()
			return(results)
		},

		
  	#' @description 
		#' Training and prediction of a multi-modal Voting ensemble in a cross validated loop. 
		#' Perform validation if a validation set is provided. 
		#' Save the results to a file.
    #' @param active_learners (integer)
    #' @return mm_results
		#' @export
		learn = function(active_learners, config) 
		{
			print("Learning")
			learners = Learners$new(self$task_type)
			base_filters = learners$base_filters
			base_learners = learners$base_learners

			result_idx = 1
			for (i in 1:length(self$tasks)) {
				dat = getTaskData(self$tasks[[i]], target.extra = TRUE)
				if (active_learners <= LRN_LAST || active_learners == LRN_ALL_MODELS) {
					for (baselrn in base_learners) {
						if (bitwAnd(active_learners, baselrn$code) || bitwAnd(active_learners, LRN_ALL_MODELS)) {
							print(paste0("result_idx = ", result_idx))
							targets = getTaskTargetNames(self$tasks[[i]])
							lrn = learners$create_learner(targets, baselrn, NULL, self$predict_type, TRUE, config$baseModels[[i]]$norm, config$baseModels[[i]]$imputation)
							res = mlr::resample(learner = lrn, task = self$tasks[[i]], measures = self$measures, resampling = self$ri, models = TRUE, extract = getFilteredFeatures)
							print("Resampling done")
							self$single_results[[result_idx]] = self$get_model_results(res, self$tasks[[i]])
							result_idx = result_idx + 1
						}
					}
				} else {
					for (filt in base_filters) {
						if (bitwAnd(active_learners, filt$code) || bitwAnd(active_learners, LRN_ALL_FS)) {
							for (baselrn in base_learners) {
								if (bitwAnd(active_learners, baselrn$code) || bitwAnd(active_learners, LRN_ALL_MODELS)) {
									targets = getTaskTargetNames(self$tasks[[i]])
									lrn = learners$create_learner(targets, baselrn, filt,  self$predict_type, TRUE, config$baseModels[[i]]$norm, config$baseModels[[i]]$imputation)
									res = mlr::resample(learner = lrn, task = self$tasks[[i]], measures = self$measures, resampling = self$ri, models = TRUE, extract = getFilteredFeatures)
									self$single_results[[result_idx]] = self$get_model_results(res, self$tasks[[i]])
									result_idx = result_idx + 1
									print(paste0("result_idx = ", result_idx))
								}
							}
						}
					}
				}
			}	
			
			if (self$validation) {
				self$validate()
			}	

			return(self$single_results)
		}
	)
)