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
		model_type = "CLASSIF",
		
    #' @description 
		#' Create a new MM_Single object.
    #' @param config Model parameters (MM_Config).
    #' @return A new `MM_Single`object.
		#' @export
		initialize = function(config, model_type = "CLASSIF", predict_type = "response", concat = FALSE, balance = FALSE, validate = FALSE, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
			super$initialize(config, model_type, predict_type, NULL, concat, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)
			self$model_type = model_type
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

#			vpred_resp = make_mlr_prediction(vresponses, vtasks[[1]]$task.desc)
#			vperf$calculate(vpred_resp)
			self$vresults$complete()
			return(self$vresults)
		},
		
		get_model_results = function(res, task)
		{
			results = MM_Results$new(self$classes, self$tasks, self$measures, self$model_type, self$decision)
			
			for (i in 1:length(res$models)) 
			{
				results$save_predictions(res$pred, task, res$models[[i]])
				results$save_features(res$models[[i]], task, self$model_type, i)
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
		learn = function(active_learners) 
		{
			learners = Learners$new(self$model_type)
			base_filters = learners$base_filters
			base_learners = learners$base_learners

			result_idx = 1
			for (i in 1:length(self$tasks)) {
				dat = getTaskData(self$tasks[[i]], target.extra = TRUE)
				if (active_learners <= LRN_LAST || active_learners == LRN_ALL_MODELS) {
					for (baselrn in base_learners) {
						if (bitwAnd(active_learners, baselrn$code) || bitwAnd(active_learners, LRN_ALL_MODELS)) {
							targets = getTaskTargetNames(self$tasks[[i]])
							lrn = learners$create_learner(targets, baselrn, NULL, self$model_type, self$decision, TRUE)
							res = mlr::resample(learner = lrn, task = self$tasks[[i]], measures = self$measures, resampling = self$ri, models = TRUE, extract = getFilteredFeatures)
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
									lrn = learners$create_learner(targets, baselrn, filt, self$model_type, self$decision, TRUE)
									res = mlr::resample(learner = lrn, task = self$tasks[[i]], measures = self$measures, resampling = self$ri, models = TRUE, extract = getFilteredFeatures)
									self$single_results[[result_idx]] = self$get_model_results(res, self$tasks[[i]])
									result_idx = result_idx + 1
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