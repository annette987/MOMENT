#--------------------------------------------------------
# CLASS MM_Meta_Learner
# Multi-modal meta learner classifier
#--------------------------------------------------------

MM_Meta_Learner = R6::R6Class("MM_Meta_Learner", 
	inherit = MM_Model,
	public = list(
		inner 			 = NULL,
		meta_learner = NULL,
		meta_results = NULL,
		meta_model 	 = NULL,
		
		initialize = function(config, decision = "prob", subset = NULL, balance = FALSE, validate = FALSE, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
			super$initialize(config, "META", decision, subset, FALSE, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)	
			self$inner = mlr::makeResampleDesc("CV", iters = config$foldsInner, stratify = TRUE)
			self$meta_model = MM_Model$new(NULL)
			learner = Learners$new(self$task_type)$base_learners[[config$metaLearner]]
			self$meta_learner = do.call(mlr::makeLearner, args = append(list("cl" = learner$class, "id" = learner$name, "predict.type" = "prob"), learner$args))
		},		

		#
		# Train the base models on the inner training set and predict on the inner test set.
		# The inner training/test set is created from the outer training data, for each CV split.
		# These predictions form the training meta data.
		# 
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
							task_name = self$tasks[[task_idx]]$task.desc$id				
							sub_task = subsetTask(self$tasks[[task_idx]], subset = training_set)
							model_futures[[j]] = future::future(mlr::train(learner = self$learners[[j]], task = sub_task, subset = ri_inner$train.inds[[subset_idx]]))
						}
						
						# Wait for results
						future::resolve(model_futures)
						for (j in 1:length(model_futures)) {
							mod = future::value(model_futures[[j]])

							if (!is.null(mod)) {
								task_name = self$tasks[[j]]$task.desc$id				
		#						raw_mod = mlr::getLearnerModel(mods, more.unwrap = TRUE)
		#						feat_base[[j]]$save_multiclass(task_name, getFeatImpScores(raw_mod, classes), i, subset_idx)
								
								sub_task = subsetTask(self$tasks[[j]], subset = training_set)						
								pred = mlr:::predict.WrappedModel(mod, task = sub_task, subset = ri_inner$test.inds[[subset_idx]])
								pred_df = as.data.frame(pred$data[, grepl("prob.", colnames(pred$data))])
								colnames(pred_df) = paste0(names(self$tasks)[[j]], ".", colnames(pred_df))
								pred_df[is.na(pred_df)] = 0
								predn_results[[length(predn_results) + 1]] = pred_df
								if (is.null(truth))
								{
									truth = factor(pred$data$truth)
								}
							}
						}

						df = data.frame(predn_results)
						df[self$targetVar] = truth
						fold_responses[[length(fold_responses) + 1]] = df
					}
					
					meta_data[[rep]] = bind_rows(fold_responses)
					meta_data[[rep]] = meta_data[[rep]] %>% arrange(self$targetVar)
				}
			
				meta = bind_rows(meta_data)
				return(meta)
		},
		
		#
		# Train the base models on the outer training set and predict on the outer test set.
		# These predictions form the meta test data
		#
		train_base_models = function(training_set) 
		{
			# Fire off training of each task in parallel
			model_futures = list()
			base_model_names = list()
			
			for (i in 1:length(self$learners)) {
				task_idx = ifelse(length(self$tasks) == length(self$learners), i, 1L)
				model_futures[[task_idx]] = future::future(mlr::train(learner = self$learners[[i]], task = self$tasks[[task_idx]], subset = training_set))
			}			
			resolve(model_futures)
			
			for (i in 1:length(model_futures)) {
				task_id = self$tasks[[i]]$task.desc$id
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

		#
		# Train a learner on the results of the base learners i.e. the meta data
		#
		train_meta_learner = function(meta_data) 
		{
			meta_task = mlr::makeClassifTask(id = "MetaLearner", data = meta_data, target = self$targetVar)
			self$meta_model$clone_model(self, list("meta" = meta_task), self$classes)
			mod = mlr::train(self$meta_learner, meta_task)
			return(list("mod" = mod, "task" = meta_task))
		},
		
		#
		# Train the meta learner on one fold of data
		# Save the features
		#
		train = function(training_set, subset_idx)
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

		#
		# Predict all base models on test data and save predictions as meta test data
		# Predict meta learner on those predictions
		#
		predict = function(meta_model, test_set)
		{
			# Get predictions from base learners on each fold of validation data
			meta_data = list()
			for (i in 1:length(self$learners)) {
				task_idx = ifelse(length(self$tasks) == length(self$learners), i, 1L)
				task_name = self$tasks[[task_idx]]$task.desc$id				
				pred = mlr:::predict.WrappedModel(private$models[[i]], task = self$tasks[[task_idx]], subset = test_set)

				pred_df = as.data.frame(pred$data[, grepl("prob.", colnames(pred$data))])
				colnames(pred_df) = paste0(task_name, ".", colnames(pred_df))
				meta_data[[length(meta_data) + 1]] = pred_df
			}
			
			meta = data.frame(meta_data)
			meta[self$targetVar] = factor(pred$data$truth)
			pred = mlr:::predict.WrappedModel(meta_model, newdata = meta)
			return(pred)
		},
				
		#
		# Train the model and predict in a cross validated loop  
		# Save the results
		#	
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
			
			self$results$complete("Meta Learner")
			return(self$results)
		}
	),	
		
	private = list(
		#' @field models (list)
		#' List of the models created in training.
		models   = list()	
	)

)