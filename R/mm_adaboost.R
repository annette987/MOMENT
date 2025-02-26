#' R6 Class representing a multi-modal Adaboost classifier
#'
#' @description
#' Creates a multi-modal Adaboost classifier that uses hard or 
#' soft voting or a meta learner to determine the final outcome of the classifier.
#'
#' @details
#' For each step in the boosting process, this model
#' trains a classifier on each modality then combines the
#' predictions from those modalities using a hard vote
#' (majority voting), soft vote (average of probabilities) 
#' or a meta learner to give the final prediction.
#' After each boosting step, the weights of the samples are updated
#' to ensure that the samples that were misclassified in the last step
#' are given more weight in subsequent boosting steps.
#'
#' @name MM_Adaboost
#' @docType package
NULL

MM_Adaboost = R6::R6Class("MM_Adaboost", 
	inherit = MM_Model,
	public = list(
		nrounds = 10,
		meta_learner = "RF",
		alphas   = list(),
		feats		 = list(),
		models	 = list(),
		meta_models   = list(),
		
    #' @description 
		#' Create a new MM_Adaboost object.
    #' @param env Set of environment variables (MM_Environment).
    #' @param nrounds Number of rounds of boosting to perform (integer).
    #' @param target Target variable name (character).
    #' @param meta_lrn Name of meta learner. Used only if meta learning is the combination method.
    #' @return A new`MM_Adaboost` object.
		#' @export
		initialize = function(config, nrounds = 10, meta_lrn = "RF", decision = "prob", subset = NULL, balance = FALSE, validate = FALSE, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
			super$initialize(config, "CLASSIF", decision, subset, FALSE, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)
			self$nrounds = nrounds					# Number of boosting iterations
			if (decision == "meta") {
				learner = Learners$new(self$task_type)$base_learners[[meta_lrn]]
				self$meta_learner = do.call(mlr::makeLearner, args = append(list("cl" = learner$class, "id" = learner$name, "predict.type" = "response"), learner$args))
			}
		},
		
		
    #' @description 
		#' Calculate the final response of the classifier, according to the decision type given.
		#' Add a response column to the results
		#' For self$decision = 'prob' the probability must also pass a threshold.
    #' @param results (data.frame)\cr
		#' The raw results from each modality. One column per modality. 
		#' Column names are modality names and row names are sample identifiers.
    #' @param classes (character vector)\cr
		#' The classes to which data samples can belong.
    #' @param self$decision (character)\cr
		#' Type of decision to be made in determining final response.
    #' @param iter 
    #' @return A data.frame containing the raw results for each modality and a new column,
		#' labelled 'response', containing the final response for the classifier.
		#' @noRd
		get_final_decision = function(results, classes, iter) {
			print("In get_final_decision")
			if (self$decision %in% c('vote', 'hard')) {
				# Calculate final prediction with a majority vote across modalities
				raw_responses = as.data.frame(results[,!colnames(results) %in% c('id', 'ID', 'truth')])
				if (all(is.na(raw_responses))) {
					results$response = rep(0, nrow(raw_responses))
				} else {
					results$response = as.factor(apply(raw_responses, 1, function(x) names(which.max(table(x)))))	
				}
			} else if (self$decision %in% c('prob', 'soft')) {
				# Calculate sum of probabilities for each class and take max of that as prediction, but only if it passes a threshold
				for (i in 1:length(classes)) {
					results[, paste0('prob.', levels(classes)[i])] = rowSums(as.data.frame(results[, grepl(paste0("\\<", levels(classes)[i], "\\>"), colnames(results))]), na.rm = TRUE) / 
																												length(levels(classes[i]))
				}
				results$response = apply(results[, grepl("prob.", colnames(results))], 1, function(x) names(which.max(x)))
				if (!is.null(results$response)) {
					results$response = strsplit(as.character(results$response), ".", fixed = TRUE)
					results$response = as.factor(sapply(results$response, "[[", 2))
				} else {
					warning("Response is NULL!")
				}

			} else if (self$decision == "meta") {
				print("Training meta model")
				print(iter)
				# Train a meta learner on the results of the base learners or predict using meta model
				meta_data = as.data.frame(results[,!colnames(results) %in% c('id', 'ID')])   # Should we match?
				meta_task = makeClassifTask(id = "MetaLearner", data = meta_data, target = 'truth')
				
				if (length(self$meta_models) < iter) {
					self$meta_models[[iter]] = mlr::train(self$meta_learner, meta_task)
					if (isFailureModel(self$meta_models[[iter]])) {
						warning(paste0("Meta learner failed"))
						warning(getFailureModelMsg(self$meta_models[[iter]]))
					}
					
					print(self$meta_models[[iter]])
					mod = mlr::getLearnerModel(self$meta_models[[iter]], more.unwrap = TRUE)
					results$response = mod$class.oob
					
					if (inherits(mod, "cv.glmnet")) {
						coef.min = coef(mod, s = mod$lambda.min)
					}
				} else {
					pred = mlr::predictLearner(self$meta_models[[iter]], newdata = meta_data)
					results$response = pred$data$response
				}
			}
			return(results)
		},
		
		
    #' @description 
		#' Get the predictions from all modalities for one round of boosting.
		#' Trains a model on each task then predicts from those models in parallel.
		#' Combines the predictions into a single data.frame and determines the final response.
		#' @param train_subset (integer vector)\cr
		#'  The indices of samples in the training subset 
		#' @param test_subset (integer vector)\cr
		#'  The indices of samples in the test subset 
    #' @param classes (character vector)\cr
		#'  The classes to which data samples can belong.
    #' @param self$decision (character)\cr
		#'  Type of decision to be made in determining final response.
    #' @param iter (integer)\cr
		#'  The iteration number in the cross-validation loop.
    #' @return A data.frame containing the raw results for each modality and a new column,
		#' labelled 'response', containing the final response for the classifier.
		#' @noRd
		get_predictions = function(train_subset, test_subset, classes, iter) 
		{
			predns = NULL
			model_futures = list()
			predn_futures = list()
			self$models[[iter]] = list()

			# Train a model on each task (modality) in parallel and wait for the results
			for (i in 1:length(self$tasks)) {
				lrn_idx = ifelse(length(self$tasks) == length(self$learners), i, 1L)
				task_id = self$tasks[[i]]$task.desc$id
				model_futures[[i]] = future::future(mlr::train(learner = self$learners[[lrn_idx]], task = self$tasks[[i]], subset = train_subset))
			}
			future::resolve(model_futures)
		
			# Predict from each model in parallel and wait for the results
			for (i in 1:length(model_futures)) {
				task_id = self$tasks[[i]]$task.desc$id
				self$models[[iter]][[task_id]] = value(model_futures[[i]])
				if (mlr::isFailureModel(self$models[[iter]][[task_id]])) {
					warning(paste0("Model ", task_id, " failed"))
					warning(mlr::getFailureModelMsg(self$models[[iter]][[task_id]]))
				}
				predn_futures[[i]] = future::future(mlr::predictLearner(self$models[[iter]][[task_id]], task = self$tasks[[i]], subset = test_subset))
			}
			future::resolve(predn_futures)
			
			# Combine the responses from each task into a single data.frame and add the response
			for (i in 1:length(predn_futures)) {
				pred = value(predn_futures[[i]])
				task_id = self$tasks[[i]]$task.desc$id
				
				# Set up predns first time through
				if (is.null(predns)) { 
					predns = pred$data[, c('id', 'truth')]
					predns$ID = rownames(pred$data)
				}

				if (self$decision %in% c("vote", "hard") || self$decision == "meta") {
					if (!any(is.na(pred$data$response))) {
						predns[, task_id] = pred$data[match(predns$ID, rownames(pred$data)), 'response']
					}
				} else {
						probs = pred$data[, grepl("prob.", colnames(pred$data))]
						probs$ID = rownames(pred$data)
						prob_cols = paste0("prob.", levels(classes))
						predns[, paste0(task_id, ".", levels(classes))] = probs[match(predns$ID, probs$ID), prob_cols, drop = FALSE]
				}
			}			
			return(as.data.frame(predns))
		},
		

		#' @description 
		#' Train a multi-modal adaboost model - on one fold of data. 
 		#' @param train_subset (integer vector)\cr
		#'  The indices of samples in the training subset .
		#' @param self$decision (character)\cr
		#'  Type of decision to be made in determining final response.
		#' @param target_var (character)\cr
		#' The target variable in the data.
    #' @return Nothing
		#' @export
		train = function(train_subset) {
			boost_iter = 1
			correct = rep(0, length(train_subset))
				
			# Initialise weights to be equal and take first sample
			weights = rep(1 / length(train_subset), length(train_subset))
			wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = weights)
			
			# Boosting loop: stop if maximum number of iterations is reached or if all samples predicted correctly.
			# If all predicted correctly, then weights won't be updated, so no point continuing
			while (boost_iter <= self$nrounds && any(correct == 0)) {
				print("Getting predictions")
				predictions = self$get_predictions(wght_sample, train_subset, self$classes, boost_iter)
				if (all(is.na(predictions[, !names(predictions) %in% c('id', 'ID', 'truth', 'response')]))) { 
					warning("All models failed!")
					correct = rep(FALSE, length(train_subset))
				} else {
					predictions = self$get_final_decision(predictions, self$classes, boost_iter)
					
					# Record a correct prediction only if it was made with high confidence:
					# i.e. a clear majority or a probability >= twice that of the next highest class probability.
					# Otherwise upweight.
					if (self$decision == 'prob') {
						high_conf = t(apply(predictions[, grepl('prob.', colnames(predictions))], 1, function(x) sort(x, TRUE)))
						high_conf = ((high_conf[, 1] / high_conf[, 2]) >= 2.0)
						correct = (as.character(predictions$response) == as.character(predictions$truth)) & high_conf
					} else {
						num_correct = rowSums(as.matrix(predictions[, !names(predictions) %in% c('id', 'ID', 'truth', 'response')]) == predictions$truth, na.rm = TRUE)		
						correct = ifelse(num_correct >= length(self$tasks)/2, 1, 0)
					}
				}

			# Calculate the error
				err = sum(as.numeric(!correct) * weights, na.rm = TRUE)
				if (err == 0) {
					alpha = 100  # Large positive
				} else if (err == 1) {
					alpha = -100  # Large negative
				} else {
					alpha = 0.5 * log((1 - err) / (err)) + log(length(self$classes)-1)  # multiclass
				}
				self$alphas[[length(self$alphas) + 1]] = alpha
				
			# Update weights and normalise
				predn = ifelse(correct, 1, -1)
				weights = weights * exp(-1 * alpha * predn)
				weights = weights/sum(weights)
				if (any(is.na(weights))) {
					weights[is.na(weights)] = 0
				}
				
				# Resample with updated weights 
				wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = weights)
				while (length(unique(predictions[wght_sample, self$targetVar])) != length(unique(predictions[train_subset, self$targetVar]))) {
					warning("Not all classes represented in sample - trying again")
					wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = weights)
				}
				boost_iter = boost_iter + 1
			}
			if (boost_iter < self$nrounds) {
				warning(paste0("Stopped early after ", boost_iter, " rounds"))
			}
		},
		
		
		# Get prediction from each modality and use that to create final prediction,based on decsion type:
		# self$decision can be one of
		#		"vote" - simple majority vote
		#		"prob" - sum the probabilities across each class and take the maximum
		#		"meta" - train a learner on the results of the base models
		# Format this into an mlr prediction object so that we can apply other mlr  functions to it
		#
		
		#' @description 
		#' Predict on new data with a fitted multi-modal adaboost model. 
 		#' @param test_subset (integer vector)\cr
		#'  The indices of samples in the test subset .
		#' @param self$decision (character)\cr
		#'  Type of decision to be made in determining final response.
    #' @return Nothing
		#' @export
		predict = function(test_subset) {
			k = length(self$classes)
			y_pred = 0
			predn_futures = list()
			
			# Get predictions from each boosting step and modality.
			# At each step, get the final prediction probabilities using all modalities.
			# Then multiply these by alpha and keep a cumuative sum
			for (i in 1:length(self$models)) { #Iteration
				# Get prediction for each modality and store in a data.frame
				# Also get feature importance scores for each modality
				for (j in 1:length(self$tasks)) { # Modality
					task_id = self$tasks[[j]]$task.desc$id
					predn_futures[[j]] = future::future(mlr::predictLearner(self$models[[i]][[task_id]], task = self$tasks[[j]], subset = test_subset))
				}
			
				# Wait for results
				future::resolve(predn_futures)			
				for (j in 1:length(predn_futures)) {
					pred = value(predn_futures[[j]])
					task_id = self$tasks[[j]]$task.desc$id
					if (j == 1) {
						results = data.frame("id" = pred$data$id, "ID" = row.names(pred$data), "truth" = pred$data$truth)
					}
					if (self$decision %in% c("prob", "soft")) {
						probs = pred$data[, !colnames(pred$data) %in% c('id', 'truth', 'response')]
						colnames(probs) = gsub("prob", task_id, colnames(probs))
						results = cbind(results, probs)
						row.names(results) = NULL
					} else {
						results[, task_id] = pred$data$response
					}
				}
				
				# Get the final decision using the results from each modality
				results = self$get_final_decision(results, self$classes, i)
				
				# Update the weighted linear sum of models
				m = matrix(0, nrow = length(test_subset), ncol = length(self$classes), dimnames = list(NULL, paste0('prob.', self$classes)))
				if (self$decision == "prob") {
					for (cls in self$classes) {
						m[, paste0('prob.', cls)] = results[, paste0('prob.', cls)]
					}
				} else {
					m[cbind(1:nrow(m), as.factor(results$response))] = 1
				}
				
				y_pred = y_pred + (self$alphas[[i]] * m)
			}
			
			y_pred_max = apply(y_pred, 1, which.max)
			final = data.frame('id' = results$id, 'ID' = results$ID, 'truth' = results$truth, 'response' = levels(self$classes)[y_pred_max], y_pred[, grepl('prob.', colnames(y_pred))]) 
			return(self$results$make_mlr_prediction(final, self$tasks[[1]]$task.desc, self$decision))
		},
		
		
  	#' @description 
		#' Training and prediction of a multi-modal Adaboost model in a cross validated loop. 
    #' @return Nothing
		#' @export
		learn = function() {
			for (rpt in 1:self$ri$desc$reps) {
				for (fold in 1:self$ri$desc$folds) {
					subset_idx = (rpt - 1) * self$ri$desc$folds + fold
					message(paste0("Subset Index = ", subset_idx))

					training_set = self$ri$train.inds[[subset_idx]]
					test_set = self$ri$test.inds[[subset_idx]]					
					print("Training")
					self$train(training_set)
					print("Predicting")
					pred = self$predict(test_set)
					self$results$save_responses(pred$data, rpt, fold)			
				}
			}
			
			if (self$validation) {
				self$validate()
			}
			self$results$complete("Adaboost")
			return(self$results)
		},		

		#' @description 
		#' Extract the feature importance scores from each model in each iteration on one fold of data. 
		#' Then multiply these by the weights for each model. 
		#' @param classes (character vector)\cr
		#'  The classes to which data samples can belong.
    #' @return Feature importance scores for the model.
		#' @export
		get_feature_importance = function(classes) {
			print("Getting feature importance scores")
			feat_scores = list()
			
			# First extract the feature importance scores from the saved models for each task
			for (j in 1:length(self$tasks)) {
				task_id = self$tasks[[j]]$task.desc$id
				self$feats[[task_id]] = list()
		
				for (i in 1:length(self$models)) {
					if (mlr::isFailureModel(self$models[[i]][[task_id]])) {
						warning(paste0("Model ", task_id, " failed on iteration ", i))
						warning(mlr::getFailureModelMsg(self$models[[i]][[task_id]]))
					} else {
						scores = getFeatImpScores(mlr::getLearnerModel(self$models[[i]][[task_id]], more.unwrap = TRUE), classes)
						selected = mlr::getFilteredFeatures(mlr::getLearnerModel(self$models[[i]][[task_id]], more.unwrap = FALSE))
						not_selected = setdiff(mlr::getTaskFeatureNames(self$tasks[[j]]), selected)
						self$feats[[task_id]][[i]] = scores[, "all"]
						names(self$feats[[task_id]][[i]]) = rownames(scores)
						if (length(not_selected) > 0) {
							self$feats[[task_id]][[i]][not_selected] = 0
							names(self$feats[[task_id]][[i]][not_selected]) = not_selected
						}
					}
				}

				# Calculate the weighted scores using the model weights
				df = as.data.frame(dplyr::bind_rows(self$feats[[task_id]]))
				seln_counts = colSums(df != 0)
				if (length(self$alphas) > 1) {
						feat_scores[[task_id]] = colSums(sapply(df, function(x) {unlist(self$alphas) * unlist(x)}), na.rm = TRUE) / sum(unlist(self$alphas))
				}
				feat_scores[[task_id]][seln_counts <= nrow(df)/2] = 0
			}		
			return(feat_scores)
		}
	)
)
