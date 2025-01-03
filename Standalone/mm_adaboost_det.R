#--------------------------------------------------------
# CLASS MM_ADABOOST DETACHED
# Multi-modal adaboost classifier
#--------------------------------------------------------

library(BBmisc)
library(R6)
library(testthat)
library(mice)
library(mlr)


MM_Adaboost = R6Class("MM_Adaboost", list(
	n_rounds = 0,
	n_mods   = 0,
	tasks    = NULL,
	learners = NULL,
	models   = list(),
	alphas   = list(),
	feats		 = list(),
	feat_scores = list(),
	predictions  = list(),
	target_var   = "",
	meta_learner = "RF",
	meta_models   = list(),
	
	initialize = function(tasks, learners, nrounds, target, meta_lrn) {
		self$n_rounds = nrounds					# Number of boosting iterations
		self$n_mods = length(tasks)			# Number of modalities
		self$tasks = tasks
		self$learners = learners
		self$target_var = target
		self$meta_learner = meta_lrn
	},
	
	# Calculate the final response, according to the decision_type type and
	# add a response column to the results
	#
	get_final_decision = function(results, classes, decision_type, iter) {
		print(paste0("In get_final_decision, iter = ", iter))
		if (decision_type == 'vote') {
			# Calculate final prediction with a majority vote across modalities
			raw_responses = as.data.frame(results[,!colnames(results) %in% c('id', 'ID', 'truth')])
			results$response = as.factor(apply(raw_responses, 1, function(x) names(which.max(table(x)))))
			
		} else if (decision_type == 'prob') {
			# Calculate sum of probabilities for each class and take max of that as prediction
			for (i in 1:length(classes)) {
				results[, paste0('prob.', classes[i])] = rowSums(as.data.frame(results[, grepl(classes[i], colnames(results))]), na.rm = TRUE)
			}
			results$response = apply(results[, grepl("prob.", colnames(results))], 1, function(x) names(which.max(x)))
			if (!is.null(results$response)) {
				results$response = strsplit(as.character(results$response), ".", fixed = TRUE)
				results$response = as.factor(sapply(results$response, "[[", 2))
			} else {
				print("Response is NULL!")
				print(results)
			}

		} else if (decision_type == "meta") {
			# Train a meta learner on the results of the base learners or predict using meta model
			meta_data = as.data.frame(results[,!colnames(results) %in% c('id', 'ID')])
			meta_task = makeClassifTask(id = "MetaLearner", data = meta_data, target = 'truth')
			if (length(self$meta_models) < iter) {
#				metalrn = base_learners[[self$meta_learner]]
				lrn = do.call(makeLearner, args = append(list("cl" = "classif.randomForestSRC", "id" = "rfsrc", "predict.type" = "response"), list(importance = "permute")))
				self$meta_models[[iter]] = train(lrn, meta_task)
				mod = getLearnerModel(self$meta_models[[iter]], more.unwrap = TRUE)
				results$response = mod$class.oob
			} else {
				pred = predict(self$meta_models[[iter]], meta_task)
				results$response = pred$data$response
			}
		}
		return(results)
	},
	
	# Get the predictions from all modalities for one round of boosting
	#
	get_predictions = function(train_subset, test_subset, sample_ids, classes, decision_type, iter) 
	{
		print(paste0("In get_predictions: ", decision_type))
		models = list()	
		predns = list()
		feats  = list()

		# Train a model on each task (modality) and record the responses in a data.frame, one per column
		for (i in 1:length(self$tasks)) {
			lrn_idx = ifelse(length(self$tasks) == length(self$learners), i, 1L)
			task_id = self$tasks[[i]]$task.desc$id
			dat = getTaskData(self$tasks[[i]])
			
			# Train the learner and record the class predictions and selected features in a list
#			print(paste0("Training ", task_id))
			models[[task_id]] = train(learner = self$learners[[lrn_idx]], task = self$tasks[[i]], subset = train_subset)

			if (!is.null(models[[task_id]])) {
#				print(paste0("Predicting ", task_id))
#				pred = predict(models[[task_id]], task = self$tasks[[i]], subset = test_subset)
				pred = tryCatch({
						predict(models[[task_id]], task = self$tasks[[i]], subset = test_subset)
				 }, 
				 error = function(cond) {
						print(paste("Model ", task_id, " returned error ", cond))
						print("*** Prediction failed ***")
						pred = NULL
					})

				if (length(predns) == 0) {
					predns[['id']] = test_subset
					predns[['ID']] = sample_ids[sample_ids$id %in% test_subset, 'ID']
					predns[['truth']] = as.factor(pred$data$truth)
				}
				
				if (decision_type == "vote" || decision_type == "meta") {
					predns[[task_id]] = pred$data$response
				} else {
					probs = pred$data[, grepl("prob.", colnames(pred$data))]
					colnames(probs) = paste0(task_id, ".", classes)
					predns = cbind(predns, probs)
				}
				mod = getLearnerModel(models[[task_id]], more.unwrap = TRUE)
				feats[[task_id]] = getFeatImpScores(mod, as.factor(as.character(unique(pred$data$truth))))
			}			
		}
		
		# Combine the responses from each task into a single data.frame and add the response
		predictions = as.data.frame(predns)
#		print("All predictions:")
#		print(predictions)
		
		self$models[[length(self$models) + 1]] = models
		self$feats[[length(self$feats) + 1]] = feats
		self$predictions[[length(self$predictions) + 1]] = predictions
		return(predictions)
	},
	
	# Train a multi-modal adaboost model
	#
	train = function(train_subset, classes, sample_ids, decision_type, target_var) {
		print("Training...")
		boost_iter = 1
		correct = rep(0, length(train_subset))			
		predns = list()

		# Task loop: Train an adaboost model on each task (modality) separately 
		# and record the responses in a data.frame, one per column
		for (i in 1:length(self$tasks)) {
			lrn_idx = ifelse(length(self$tasks) == length(self$learners), i, 1L)
			task_id = self$tasks[[i]]$task.desc$id
			print(paste0("Training ", task_id))
			dat = getTaskData(self$tasks[[i]])
			predictions = NULL
			
			# Initialise weights to be equal and take first sample
			weights = rep(1 / length(train_subset), length(train_subset))
			wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = weights)
		
			# Boosting loop: stop if maximum number of iterations is reached or if all samples predicted correctly.
			# If all predicted correctly, then weights won't be updated, so no point continuing
			while (boost_iter <= self$n_rounds && any(correct == 0)) { 
				print(paste0("boost_iter = ", boost_iter))
				
				# Train the learner and get the predictions
				models[[boost_iter]] = train(learner = self$learners[[lrn_idx]], task = self$tasks[[i]], subset = wght_sample)

				if (!is.null(models[[boost_iter]])) {
					pred = tryCatch({
							predict(models[[boost_iter]], task = self$tasks[[i]], subset = train_subset)
					 }, 
					 error = function(cond) {
							print(paste0("Model ", task_id, "on iteration ", boost_iter, " returned error ", cond))
							print("*** Prediction failed ***")
							pred = NULL
						})
						
					if (predictions == NULL) {
						predictions = data.frame('id' = train_subset, 
																		 'ID' = sample_ids, 
																		 'truth' = as.factor(pred$data$truth))
					}
					
					if (decision_type == "vote" || decision_type == "meta") {
						predictions[boost_iter] = pred$data$response
					} else {
						probs = pred$data[, grepl("prob.", colnames(pred$data))]
						colnames(probs) = paste0(task_id, ".", classes)
						predictions = cbind(predictions, probs)
					}
					mod = getLearnerModel(models[[boost_iter]], more.unwrap = TRUE)
					feats[[boost_iter]] = getFeatImpScores(mod, as.factor(as.character(unique(pred$data$truth))))
					
					correct = pred$data$response == pred$data$truth
					# Could calculate high confidence here with probabilities
		#			print("Correct:")
		#			print(correct)

				# Calculate the error
					err = sum(as.numeric(!correct) * weights, na.rm = TRUE)
					print(paste0("Error = ", err))
					if (err == 0) {
						alpha = 10  # Large positive
					} else if (err == 1) {
						alpha = -10  # Large negative
					} else {
						alpha = 0.5 * log((1 - err) / (err)) + log(length(classes)-1)  # multiclass
					}
					print(paste0("alpha = ", alpha))
					self$alphas[[length(self$alphas) + 1]] = alpha
					
				# Update weights and normalise
					predn = ifelse(correct, 1, -1)
					weights = weights * exp(-1 * alpha * predn)
					weights = weights/sum(weights)
		#			print("Updated weights:")
		#			print(weights)
					
					# Resample with updated weights 
					wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = weights)
					while (length(unique(sample_ids[wght_sample, target_var])) != length(unique(sample_ids[train_subset, target_var]))) {
						print("Not all classes represented in sample - trying again")
						wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = weights)
					}
				}
				boost_iter = boost_iter + 1
			}
			if (boost_iter < self$n_rounds) {
				print(paste0("Stopped early after ", boost_iter, " rounds"))
			}
			print("Predictions for each boosting round:")
			print(predictions)
			predns[task_id] = 
		}
	},
	
	# Create an mlr prediction object using the specified results
	make_mlr_prediction = function(results, task) {
		p = makeS3Obj(c("PredictionClassif", "Prediction"),
			predict.type = "response",
			data = results,
			threshold = NA_real_,
			task.desc = task$task.desc,
			time = NULL,
			error = NULL,
			dump = NULL
		)
		return(p)
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
		k = length(classes)
		y_pred = 0
		feat_scores = list()
#		print(self$feats)
		
		# Get predictions from each boosting step and modality.
		# At each step, get the final prediction probabilities using all modalities.
		# Then multiply these by alpha and keep a cumuative sum
		print("Creating linear sum of models ...")
		for (i in 1:length(self$models)) { #Iteration
			print(paste0("Iteration = ", i))
			
			# Get prediction for each modality and store in a data.frame
			# Also get feature importance scores for each modality
			for (j in 1:length(self$tasks)) { # Modality
				print(paste0("Modality = ", j))
				task_id = self$tasks[[j]]$task.desc$id
				pred = predict(self$models[[i]][[j]], task = self$tasks[[j]], subset = test_subset)

				if (j == 1) {
					results = data.frame("id" = pred$data$id, "ID" = row.names(pred$data), "truth" = pred$data$truth)
				}
				if (decision_type == "prob") {
					probs = pred$data[, !colnames(pred$data) %in% c('id', 'truth', 'response')]
					colnames(probs) = paste0(task_id, ".", classes)
					results = cbind(results, probs)
					row.names(results) = NULL
				} else {
					results[, task_id] = pred$data$response
				}
#				print(results)

				if (!(task_id %in% names(feat_scores))) {
					feat_names = getTaskFeatureNames(self$tasks[[j]])
					feat_scores[[task_id]] = data.frame(rep(0, length(feat_names)), row.names = feat_names)
				}
				selected_feats = row.names(self$feats[[i]][[j]])
				if (!is.null(self$feats[[i]][[j]]) && !is.na(self$feats[[i]][[j]])){
					feat_scores[[task_id]][selected_feats, 1] = feat_scores[[task_id]][selected_feats, 1] + 
																											(self$alphas[[i]] * self$feats[[i]][[j]][, "all"])	
				}
#				stab$save_features(getFilteredFeatures(models[[task_id]]))
			}
			
			# Get the final decision using the results from each modality
			results = self$get_final_decision(results, classes, decision_type, i)
			print("Results for each modality:")
			print(results)
			
			# Update the weighted linear sum of models
			m = matrix(0, nrow = length(test_subset), ncol = length(classes), dimnames = list(NULL, classes))
			if (decision_type == "prob") {
				for (cls in classes) {
					m[, cls] = results[, paste0('prob.', cls)]
				}
			} else {
				m[cbind(1:nrow(m), as.factor(results$response))] = 1
			}
			
#			if (decision_type == "prob") {
#				for (i in 1:length(classes)) {
#					m[, classes[i]] = rowSums(as.data.frame(results[, grepl(classes[i], colnames(results))]), na.rm = TRUE)
#				}				
#			} else {
#				tbl = apply(results[,!colnames(results) %in% c('id', 'ID', 'truth')], 1, function(x) table(x))
#				for (idx in 1:length(tbl)) {
#					for (cls in classes) {
#						m[idx, cls] = tbl[[idx]][cls]
#					}  
#				}
#				m[is.na(m)] = 0
#			}
			print("Matrix:")
			print(m)

			y_pred = y_pred + (self$alphas[[i]] * m)
			print(paste0("alpha = ", self$alphas[[i]]))
			print("y_pred:")
			print(y_pred)
		}

		y_pred_max = apply(y_pred, 1, which.max)
		final = data.frame('id' = results$id, 'ID' = results$ID, 'truth' = results$truth, 'response' = classes[y_pred_max])
		print("Final Result:")
		print(final)
		
		#Normalise the feature scores
		alpha_sum = sum(unlist(self$alphas))
		for (j in 1:length(self$tasks)) { # For each modality	
			feat_scores[[j]] = feat_scores[[j]] / alpha_sum
		}
#		print("Features:")
#		print(feat_scores)
		
		return(list("pred" = self$make_mlr_prediction(final, self$tasks[[1]]), "features" = feat_scores))
	})
)
