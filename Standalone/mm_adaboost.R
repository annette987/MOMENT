#--------------------------------------------------------
# CLASS MM_ADABOOST
# Multi-modal adaboost classifier
#--------------------------------------------------------

library(BBmisc)
library(R6)
library(testthat)
library(mice)
library(mlr)
library(future)

source("imputation.R")
source("normalisation.R")


MM_Adaboost = R6Class("MM_Adaboost", list(
	n_rounds = 0,
	tasks    = NULL,
	learners = NULL,
	models   = list(),
	alphas   = list(),
	feats		 = list(),
	target_var   = "",
	meta_learner = "RF",
	meta_models   = list(),
	
	initialize = function(tasks, learners, nrounds, target, meta_lrn) {
		self$n_rounds = nrounds					# Number of boosting iterations
		self$tasks = tasks
		self$learners = learners
		self$target_var = target
		self$meta_learner = meta_lrn
	},
	
	# Calculate the final response, according to the decision_type type and add a response column to the results
	# For decision_type = 'prob' the probability must also pass a threshold
	#
	get_final_decision = function(results, classes, decision_type, iter) {
		print(paste0("In get_final_decision: ", decision_type))
		if (decision_type == 'vote') {
			# Calculate final prediction with a majority vote across modalities
			raw_responses = as.data.frame(results[,!colnames(results) %in% c('id', 'ID', 'truth')])
			if (all(is.na(raw_responses))) {
				results$response = rep(0, nrow(raw_responses))
			} else {
				results$response = as.factor(apply(raw_responses, 1, function(x) names(which.max(table(x)))))	
			}
		} else if (decision_type == 'prob') {
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
				print("Response is NULL!")
			}

		} else if (decision_type == "meta") {
			# Train a meta learner on the results of the base learners or predict using meta model
			meta_data = as.data.frame(results[,!colnames(results) %in% c('id', 'ID')])   # Should we match?
			print("META DATA:")
			print(head(meta_data))
			meta_task = makeClassifTask(id = "MetaLearner", data = meta_data, target = 'truth')
			if (length(self$meta_models) < iter) {
				metalrn = base_learners[[self$meta_learner]]
				lrn = do.call(makeLearner, args = append(list("cl" = metalrn$class, "id" = metalrn$name, "predict.type" = "response"), metalrn$args))
#				lrn = do.call(makeLearner, args = append(list("cl" = "classif.randomForest", "id" = "rfsrc", "predict.type" = "response"), list(importance = "permute")))
				self$meta_models[[iter]] = train(lrn, meta_task)
				mod = getLearnerModel(self$meta_models[[iter]], more.unwrap = TRUE)
				results$response = mod$class.oob
				
				if (inherits(mod, "cv.glmnet")) {
					coef.min = coef(mod, s = mod$lambda.min)
					print("Lasso MetaLearner coefficients:")
					print(coef.min)
				}
			} else {
				pred = predict(self$meta_models[[iter]], newdata = meta_data)
				results$response = pred$data$response
			}
		}
		rm(mod)
		rm(meta_data)
		rm(meta_task)
		print("RESULTS:")
		print(results)
		return(results)
	},
	
	# Get the predictions from all modalities for one round of boosting
	#
	get_predictions = function(train_subset, test_subset, sample_ids, classes, decision_type, iter) 
	{
		print("In get_predictions")
		print(classes)
		predns = NULL
		model_futures = list()
		predn_futures = list()
		self$models[[iter]] = list()

		# Train a model on each task (modality) in parallel and wait for the results
		for (i in 1:length(self$tasks)) {
			lrn_idx = ifelse(length(self$tasks) == length(self$learners), i, 1L)
			task_id = self$tasks[[i]]$task.desc$id
			print(paste0("Training ", task_id, ", ", Sys.time()))
			model_futures[[i]] = future(train(learner = self$learners[[lrn_idx]], task = self$tasks[[i]], subset = train_subset))
#			model_futures[[i]] = train(learner = self$learners[[lrn_idx]], task = self$tasks[[i]], subset = train_subset)
		}
		print(paste0("Waiting for training futures to resolve ... ", Sys.time()))
		resolve(model_futures)
		print(paste0("Futures resolved at", Sys.time()))
	
		# Predict from each model in parallel and wait for the results
		for (i in 1:length(model_futures)) {
			task_id = self$tasks[[i]]$task.desc$id
			print(task_id)
			self$models[[iter]][[task_id]] = value(model_futures[[i]])
#			self$models[[iter]][[task_id]] = model_futures[[i]]
#			print(self$models[[iter]][[task_id]]$task.desc$class.levels)
			if (isFailureModel(self$models[[iter]][[task_id]])) {
				print(paste0("Model ", task_id, " failed"))
				print(getFailureModelMsg(self$models[[iter]][[task_id]]))
			}
			predn_futures[[i]] = future(predict(self$models[[iter]][[task_id]], task = self$tasks[[i]], subset = test_subset))
#			predn_futures[[i]] = predict(self$models[[iter]][[task_id]], task = self$tasks[[i]], subset = test_subset)
		}
		print(paste0("Waiting for prediction futures to resolve ... ", Sys.time()))
		resolve(predn_futures)
		print(paste0("Prediction futures  resolveD at ", Sys.time()))

		
		# Combine the responses from each task into a single data.frame and add the response
		print("Combining responses ...")
		for (i in 1:length(predn_futures)) {
			print(paste0("Task ", i))
			pred = value(predn_futures[[i]])
			task_id = self$tasks[[i]]$task.desc$id
			print(task_id)
			
			# Set up predns first time through
			if (is.null(predns)) { 
				predns = pred$data[, c('id', 'truth')]
				predns$ID = rownames(pred$data)
			}

			if (decision_type == "vote" || decision_type == "meta") {
				if (!any(is.na(pred$data$response))) {
					predns[, task_id] = pred$data[match(predns$ID, rownames(pred$data)), 'response']
				}
			} else {
					probs = pred$data[, grepl("prob.", colnames(pred$data))]
					probs$ID = rownames(pred$data)
#					if (!is.factor(classes)) {
#						classes = factor(classes)
#					}
					prob_cols = paste0("prob.", levels(classes))
					print(prob_cols)
					print(head(probs))
					predns[, paste0(task_id, ".", levels(classes))] = probs[match(predns$ID, probs$ID), prob_cols, drop = FALSE]
			}
		}
		
		rm(pred)
		rm(model_futures)
		rm(predn_futures)
		return(as.data.frame(predns))
	},
	
	# Train a multi-modal adaboost model - on one fold of data
	#
	train = function(train_subset, classes, sample_ids, decision_type, target_var) {
		print("Training adaboost...")
		print(classes)
		boost_iter = 1
		correct = rep(0, length(train_subset))
			
		# Initialise weights to be equal and take first sample
		print("Initialising weights...")
		weights = rep(1 / length(train_subset), length(train_subset))
		wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = weights)
		
		# Boosting loop: stop if maximum number of iterations is reached or if all samples predicted correctly.
		# If all predicted correctly, then weights won't be updated, so no point continuing
		while (boost_iter <= self$n_rounds && any(correct == 0)) { 
			print(paste0("boost_iter = ", boost_iter, ", ", Sys.time()))
			predictions = self$get_predictions(wght_sample, train_subset, sample_ids, classes, decision_type, boost_iter)
			if (all(is.na(predictions[, !names(predictions) %in% c('id', 'ID', 'truth', 'response')]))) { 
				print("All models failed!")
				correct = rep(FALSE, length(train_subset))
			} else {
				predictions = self$get_final_decision(predictions, classes, decision_type, boost_iter)
				
				# Record a correct prediction only if it was made with high confidence:
				# i.e. a clear majority or a probability >= twice that of the next highest class probability.
				# Otherwise upweight.
				if (decision_type == 'prob') {
					high_conf = t(apply(predictions[, grepl('prob.', colnames(predictions))], 1, function(x) sort(x, TRUE)))
					high_conf = ((high_conf[, 1] / high_conf[, 2]) >= 2.0)
#					print(high_conf)
					correct = (as.character(predictions$response) == as.character(predictions$truth)) & high_conf
					rm(high_conf)
				} else {
					num_correct = rowSums(as.matrix(predictions[, !names(predictions) %in% c('id', 'ID', 'truth', 'response')]) == predictions$truth, na.rm = TRUE)		
					correct = ifelse(num_correct >= length(self$tasks)/2, 1, 0)
				}
			}

		# Calculate the error
			err = sum(as.numeric(!correct) * weights, na.rm = TRUE)
			print(paste0("Error = ", err))
			if (err == 0) {
				alpha = 100  # Large positive
			} else if (err == 1) {
				alpha = -100  # Large negative
			} else {
				alpha = 0.5 * log((1 - err) / (err)) + log(length(classes)-1)  # multiclass
			}
			print(paste0("alpha = ", alpha))
			self$alphas[[length(self$alphas) + 1]] = alpha
#			print("Alphas:")
#			print(self$alphas)
			
		# Update weights and normalise
			print("Updating weights")
			predn = ifelse(correct, 1, -1)
			weights = weights * exp(-1 * alpha * predn)
			weights = weights/sum(weights)
			if (any(is.na(weights))) {
				weights[is.na(weights)] = 0
			}
			rm(predn)
			
			# Resample with updated weights 
			wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = weights)
			while (length(unique(sample_ids[wght_sample, target_var])) != length(unique(sample_ids[train_subset, target_var]))) {
				print("Not all classes represented in sample - trying again")
				wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = weights)
			}
			boost_iter = boost_iter + 1
		}
		if (boost_iter < self$n_rounds) {
			print(paste0("Stopped early after ", boost_iter, " rounds"))
		}
	},
	
	# Get prediction from each modality and use that to create final prediction,based on decsion type:
	# decision_type can be one of
	#		"vote" - simple majority vote
	#		"prob" - sum the probabilities across each class and take the maximum
	#		"meta" - train a learner on the results of the base models
	# Format this into an mlr prediction object so that we can apply other mlr  functions to it
	#
	predict = function(test_subset, classes, decision_type) {
		print("Predicting...")
		print(classes)
		k = length(classes)
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
				print("Predicting ...")
				print(task_id)
#				self$models[[i]][[task_id]] = update_task_classes(self$models[[i]][[task_id]], classes, getTaskTargets(self$tasks[[j]]))				
				predn_futures[[j]] = future(predict(self$models[[i]][[task_id]], task = self$tasks[[j]], subset = test_subset))
#				predn_futures[[j]] = predict(self$models[[i]][[task_id]], task = self$tasks[[j]], subset = test_subset)
			}
		
			# Wait for results
			resolve(predn_futures)
			
			for (j in 1:length(predn_futures)) {
				pred = value(predn_futures[[j]])
				task_id = self$tasks[[j]]$task.desc$id
				print(task_id)
				if (j == 1) {
					results = data.frame("id" = pred$data$id, "ID" = row.names(pred$data), "truth" = pred$data$truth)
				}
				if (decision_type == "prob") {
					probs = pred$data[, !colnames(pred$data) %in% c('id', 'truth', 'response')]
					colnames(probs) = gsub("prob", task_id, colnames(probs))
					results = cbind(results, probs)
					row.names(results) = NULL
					rm(probs)
				} else {
					results[, task_id] = pred$data$response
				}
				rm(pred)
			}
			
			# Get the final decision using the results from each modality
			results = self$get_final_decision(results, classes, decision_type, i)
#			
			# Update the weighted linear sum of models
			m = matrix(0, nrow = length(test_subset), ncol = length(classes), dimnames = list(NULL, paste0('prob.', classes)))
			if (decision_type == "prob") {
				for (cls in classes) {
					m[, paste0('prob.', cls)] = results[, paste0('prob.', cls)]
				}
			} else {
				m[cbind(1:nrow(m), as.factor(results$response))] = 1
			}
			
			y_pred = y_pred + (self$alphas[[i]] * m)
			rm(m)
		}
#		print("Probabilities:")
#		print(head(y_pred))

		y_pred_max = apply(y_pred, 1, which.max)
		final = data.frame('id' = results$id, 'ID' = results$ID, 'truth' = results$truth, 'response' = levels(classes)[y_pred_max], y_pred[, grepl('prob.', colnames(y_pred))]) 
#		print("FINAL:")
#		print(final)
		
		return(make_mlr_prediction(final, self$tasks[[1]]))
	},
	

	# Extract the feature importance scores from each model on each iteration on one fold of data.
	# Then multiple these by the weights for each model.
	get_feature_importances = function(classes) {
		print("In get_feature_importances")
		feat_scores = list()
		
		# First extract the feature importance scores from the saved models for each task
		for (j in 1:length(self$tasks)) {
			task_id = self$tasks[[j]]$task.desc$id
			print(task_id)
			self$feats[[task_id]] = list()
	
			for (i in 1:length(self$models)) {
				if (isFailureModel(self$models[[i]][[task_id]])) {
					print(paste0("Model ", task_id, " failed on iteration ", i))
					print(getFailureModelMsg(self$models[[i]][[task_id]]))
				} else {
					scores = getFeatImpScores(getLearnerModel(self$models[[i]][[task_id]], more.unwrap = TRUE), classes)
					selected = getFilteredFeatures(getLearnerModel(self$models[[i]][[task_id]], more.unwrap = FALSE))
					not_selected = setdiff(getTaskFeatureNames(self$tasks[[j]]), selected)
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
#			print(self$alphas)
			if (length(self$alphas) > 1) {
#					print(head(df))
#					print(sapply(df, function(x) {unlist(self$alphas) * unlist(x)}), na.rm = TRUE)
					feat_scores[[task_id]] = colSums(sapply(df, function(x) {unlist(self$alphas) * unlist(x)}), na.rm = TRUE) / sum(unlist(self$alphas))
			}
			feat_scores[[task_id]][seln_counts <= nrow(df)/2] = 0
		}		
		return(feat_scores)
	})
)
