#' @title R6 Class representing a multi-modal meta learner classifier
#'
#' @description
#' Creates a multi-modal meta learner classifier that uses
#' soft voting to determine the final outcome of the classifier.
#'
#' @details
#' Still to come
#'
#' @name MM_Meta_Learner
NULL

MM_Meta_Learner = R6::R6Class("MM_Meta_Learner", 
	inherit = MM_Model,
	public = list(
		inner 			 = NULL,
		meta_learner = NULL,
		meta_results = NULL,
		meta_model 	 = NULL,
		
    #' @description 
		#' Create a new MM_Meta_Learner object, based on the parameters specified in the config object.
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
### NB explain how these are selected
		#' @return A new `MM_Meta_Learner`object
		#' @export
		#' 
		initialize = function(config, task_type, decision = "prob", subset = NULL, balance = FALSE, validate = FALSE, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
			pred_type = ifelse(decision %in% c("prob", "soft"), "prob", "response")
			super$initialize(config, "classif", pred_type, decision, subset, FALSE, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)	
			self$inner = mlr::makeResampleDesc("CV", iters = config$foldsInner, stratify = TRUE)
			self$meta_model = MM_Model$new(NULL)
			learner = Learners$new(self$task_type)$base_learners[[config$metaLearner]]
			self$meta_learner = do.call(mlr::makeLearner, args = append(list("cl" = learner$class, "id" = learner$name, "predict.type" = "prob"), learner$args))
		},		


		#' @description 
		#' Train the base models on the inner training set and predict on the inner test set.
		#' The inner training/test set is created from the outer training data, for each CV split.
		#' These predictions form the training meta data.
    #' @param training_set (integer)\cr
		#' A vector of indices indicating which samples should be used in training.
    #' @return The meta data
		#' @noRd
		get_out_of_fold_predictions = function(training_set) 
		{
				meta_data = list()		
				sub_task = subsetTask(self$tasks[[1]], subset = training_set)
				ri_inner = mlr::makeResampleInstance(self$inner, sub_task)
							
				for (rep in 1:1) {
					fold_responses = list()
					
					for (fold in 1:ri_inner$desc$iters) {
						subset_idx = (rep - 1) * ri_inner$desc$iters + fold
						predn_results = list()
						truth = NULL
					
						# Fire off training of each task in parallel
						model_futures = list()
						for (j in 1:length(self$tasks)) {
							task_idx = ifelse(length(self$tasks) == length(self$learners), j, 1L)	
							sub_task = subsetTask(self$tasks[[task_idx]], subset = training_set)
							model_futures[[j]] = future::future(mlr::train(learner = self$learners[[j]], task = sub_task, subset = ri_inner$train.inds[[subset_idx]]), conditions = character(0))
						}
						
						# Wait for results
						future::resolve(model_futures)
						for (j in 1:length(model_futures)) {
							mod = future::value(model_futures[[j]])

							if (!is.null(mod)) {			
		#						raw_mod = mlr::getLearnerModel(mods, more.unwrap = TRUE)
		#						feat_base[[j]]$save_multiclass(mlr::getTaskID(self$tasks[[j]]), getFeatImpScores(raw_mod, classes), i, subset_idx)
								
								sub_task = subsetTask(self$tasks[[j]], subset = training_set)						
								pred = predict(mod, task = sub_task, subset = ri_inner$test.inds[[subset_idx]])
#								pred_df = as.data.frame(pred$data[, grepl("prob.", colnames(pred$data))])

								# EXPECTING PROBABILITIES. DO THEY NEED TO BE? COULD BE RESPONSES? 
								# SOMETHING META LEARNER CAN LEARN FROM
								pred_df = mlr::getPredictionProbabilities(pred)
								colnames(pred_df) = paste0(names(self$tasks)[[j]], ".", colnames(pred_df))
								pred_df[is.na(pred_df)] = 0
								predn_results[[length(predn_results) + 1]] = pred_df
								if (is.null(truth))
								{
									truth = factor(mlr::getPredictionTruth(pred))
								}
							}
						}

						df = data.frame(predn_results)
						df[self$targetVar] = truth
						fold_responses[[length(fold_responses) + 1]] = df
					}
					
					meta_data[[rep]] = dplyr::bind_rows(fold_responses)
					meta_data[[rep]] = meta_data[[rep]] %>% arrange(self$targetVar)
				}
			
				meta = dplyr::bind_rows(meta_data)
				return(meta)
		},
		

		#' @description 
		#' Train the base models on the outer training set and predict on the outer test set.
		#' These predictions form the meta test data
    #' @param training_set (integer)\cr
		#' A vector of indices indicating which samples should be used in training.
    #' @return A named list containing the trained models
		#' @noRd
		train_base_models = function(training_set) 
		{
			# Fire off training of each task in parallel
			model_futures = list()
			base_model_names = list()
			
			for (i in 1:length(self$learners)) {
				task_idx = ifelse(length(self$tasks) == length(self$learners), i, 1L)
				model_futures[[task_idx]] = future::future(mlr::train(learner = self$learners[[i]], task = self$tasks[[task_idx]], subset = training_set), conditions = character(0))
			}			
			resolve(model_futures)
			
			for (i in 1:length(model_futures)) {
				task_id = mlr::getTaskID(self$tasks[[i]])
				private$models[[task_id]] = future::value(model_futures[[i]])

				if (mlr::isFailureModel(private$models[[task_id]])) {
					warning(paste0("Model ", task_id, " failed on repeat ", rpt, " fold ", fold))
					warning(mlr::getFailureModelMsg(private$models[[task_id]]))
				} else {
					parts = strsplit(self$learners[[i]]$id, ".", fixed=TRUE)
					base_model_names = append(base_model_names, paste0(parts[[1]][1], "_", task_id, "_", i))
				}
			}
			return(setNames(private$models, base_model_names))
		},


		#' @description 
		#' Train a learner on the results of the base learners i.e. the meta data
    #' @param meta_data (data.frame)\cr
		#' A data.frame containing the meta_data i.e. the results of training the base models.
    #' @return A  list containing the trained meta-model and the meta task.
		#' @noRd
		train_meta_learner = function(meta_data) 
		{
			meta_task = mlr::makeClassifTask(id = "MetaLearner", data = meta_data, target = self$targetVar)
			self$meta_model$clone_model(self, list("meta" = meta_task), self$classes)
			mod = mlr::train(self$meta_learner, meta_task)
			return(list("mod" = mod, "task" = meta_task))
		},
		

		#' @description 
		#' Train the meta learner on one fold of data and save the selected features.
    #' @param subset_idx (integer)\cr
		#' An index indicating which subset of data should be used for training..
    #' @return A  list containing the trained meta-model and the meta task.
		#' @export		
		train = function(subset_idx)
		{
			self$train_base_models(self$ri$train.inds[[subset_idx]])
			meta_data_train = self$get_out_of_fold_predictions(self$ri$train.inds[[subset_idx]])
			meta_task = mlr::makeClassifTask(id = "MetaLearner", data = meta_data_train, target = self$targetVar)
			self$meta_model$clone_model(self, list(meta_task), self$classes, self$meta_learner)
			mod = mlr::train(self$meta_learner, meta_task)
			
			self$meta_results = MM_Results$new(self$classes, meta_task)
			self$meta_results$save_features(mod, meta_task, "meta", subset_idx)
			return(meta_res$mod)
		},


		#' @description 
		#' Predict all base models on test data and save predictions as meta test data
		#' Predict meta learner on those predictions
    #' @param meta_model\cr
		#' The trained meta learner model.
		#' @param test_set (integer)\cr
		#' A vector of indices indicating which samples should be used in testing.
    #' @return An object of type mlr::Prediction containing the predictions from the meta model.
		#' @export		
		predict = function(meta_model, test_set)
		{
			# Get predictions from base learners on each fold of validation data
			meta_data = list()
			for (i in 1:length(self$learners)) {
				task_idx = ifelse(length(self$tasks) == length(self$learners), i, 1L)
				pred = predict(private$models[[i]], task = self$tasks[[task_idx]], subset = test_set)

#				pred_df = as.data.frame(pred$data[, grepl("prob.", colnames(pred$data))])
				pred_df = mlr::getPredictionProbabilities(pred)
				colnames(pred_df) = paste0(mlr::getTaskID(self$tasks[[task_idx]]), ".", colnames(pred_df))
				meta_data[[length(meta_data) + 1]] = pred_df
			}
			
			meta = data.frame(meta_data)
			meta[self$targetVar] = factor(mlr::getPredictionTruth(pred))
			pred = predict(meta_model, newdata = meta)
			return(pred)
		},
				

		#' @description 
		#' Train the model and predict in a cross validated loop, optionally with validation.   
		#' @return An object of type MM_Results containing the model results.
		#' @export		
		learn = function()
		{
			for (rep in 1:self$ri$desc$reps) {
				for (fold in 1:self$ri$desc$folds) {
					subset_idx = (rep - 1) * self$ri$desc$folds + fold
					training_set = self$ri$train.inds[[subset_idx]]
					test_set = self$ri$test.inds[[subset_idx]]
			
					mods = self$train_base_models(training_set)
					meta_data = self$get_out_of_fold_predictions(training_set)
					meta_res = self$train_meta_learner(meta_data)

					if (!is.null(meta_res$mod)) {
						pred = self$predict(meta_res$mod, self$ri$test.inds[[subset_idx]])
						self$results$save_responses(pred$data, rep, fold)
					}
				}
			}

			if (self$validation) {
				self$validate()
			}
			
			self$results$complete()
			return(self$results)
		}
	),	
		
	private = list(
		#' @field models (list)
		#' List of the models created in training.
		models   = list()	
	)

)