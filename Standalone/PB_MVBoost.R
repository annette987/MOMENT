#This code is implements PB-MVBoost.

#Related Paper:
#Multiview Boosting by Controlling the Diversity and the Accuracy of View-specific Voters
#by Anil Goyal, Emilie Morvant, Pascal Germain and Massih-Reza Amini

#Link to the paper:
#https://arxiv.org/abs/1808.05784

#Author = "Anil Goyal"
#Author of the R implementation - Annette Spooner

library(R6)

source("MV_Cbound_opt.R")

# This class implements the multiview learning algorithm based on boosting. We call this algorithm as PB-MVBoost.
# This algorithm is based on two-level multiview learning approach. It learns the distribution over view-specific
# classifiers and distribution over views in one step following a boosing approach.

PB_MVBoost = R6Class("PB_MVBoost", list(
	all_views = list(),
	tasks = list(),
	learners = list(),
	target_var = 'Label',
	predn_type = "response",
	cache_dir = "",
	num_train_examples = 0,
	num_test_examples = 0,
	num_iterations = 10,
	decision_tree_depth = 2,
	nb_view_classifiers = NULL,
	X_train = NULL,
	X_test = NULL,
	y_train = NULL,
	y_test = NULL,
	models = list(),
	train_predictions_classifiers = list(),
	validation_predictions_classifiers = list(),
	test_predictions_classfiers = list(),
	weights_classfiers = list(),
	rho = list(),
	rho_vectors = list(),

	initialize = function(X_train, y_train, X_test, y_test, views, tasks, learners, models, pred_type, cache_dir, num_iterations, decision_tree_depth = 1 ) {
			self$all_views = views
			self$tasks = tasks
			self$learners = learners
			self$predn_type = pred_type
			self$cache_dir = cache_dir
			self$num_train_examples = length(y_train[[1]])
			self$num_test_examples = length(y_test[[1]])
			self$num_iterations = num_iterations
			self$decision_tree_depth = decision_tree_depth
			self$nb_view_classifiers = rep(0, length(self$all_views))

			#Variables for training and test data
			self$X_train = X_train
			self$y_train = y_train
			self$X_test = X_test
			self$y_test = y_test

			# Variables to store the train and test predictions and weights after each iteration
			for (view_name in self$all_views) {
				self$train_predictions_classifiers[[view_name]] = list()
				self$test_predictions_classfiers[[view_name]] = list()
				self$weights_classfiers[[view_name]] = list()
				self$models[[view_name]] = list()
			}
	},

#	This function is helping function to compute weight of hypothesis based on error pased to it.
#	It Computes 0.5 * ln (1-error/ error)
#	:param error: Error value
#	:return: Weight value

	compute_weight = function(error, view_index, classes) {
		view_weight = self$rho[[view_index]]
		if (view_weight == 0) {
				return(0)
		} else {
#				return(0.5 * log((1 - error) / error))
				return(0.5 * log((1 - error) / (error)) + log(length(classes)-1))  # multiclass
				#return(0.5 * (1 / view_weight) * (log((1 - error) / (error) + 2))
		}
	},


#	This function returns the error and f1-score.
#	:param predicted_values:  Predicted labels of any estimator
#	:param true_values: True labels
#	:return:

	compute_stats = function(predicted_values, true_values) {
		# remove the elements with output zero.
		predicted_values = as.numeric(predicted_values[predicted_values != 0])
		true_values      = as.numeric(true_values[predicted_values != 0])

#		error = mean(predicted_values * true_values <= 0.0)
		error = mean(predicted_values != true_values)
		f1 = measureF1(true_values, predicted_values, 1)
		return(list("error" = error, "f1" = f1))
	},


#	This function learns the weak classifier and returns a weight for this learned classifier. 
# Fitting is done on weighted samples which are passed as an input parameter.
# The classifier is trained on one view on one fold of data.
#	Input
#	======
#	:param view_name : View name for which we need to learn a classfier
#	:param example_weights : Weight of input training examples
#	:return: Weight of Classifier, training data labels, test data labels.

	learn_classifier = function(view_name, view_index, train_subset, test_subset, example_weights, classes) {
#		print(paste0("In learn_classifier, view_name = ", view_name))
		X_train = self$X_train[[view_name]]
		X_test = self$X_test[[view_name]]
		y_train = self$y_train[[view_name]]

		#Learning classifier
		# The weights change on each iteration, but mlrCPO does not handle weights.
		# Therefore they can't be passed through the pipeline, so they have to be
		# handled by taking a weighted sample of the training set and using that.
		
		wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = example_weights)
		mod = train(learner = self$learners[[view_index]], task = self$tasks[[view_index]], subset = wght_sample)
		pred_train = predict(mod, task = self$tasks[[view_index]], subset = train_subset)
		pred_test  = predict(mod, task = self$tasks[[view_index]], subset = test_subset)

		#predicting labels for training and test data
		predicted_labels_train = pred_train$data$response
		predicted_labels_test  = pred_test$data$response
		
		#computing error
		if (isFailureModel(mod)) {
			print(paste0("Model ", view_index, " failed: ", getFailureModelMsg(mod)))
			error_t_weighted = 0
		} else {
			error_t = as.numeric(as.character(predicted_labels_train) != as.character(y_train))
			error_t_weighted = (t(example_weights) %*% error_t) / sum(example_weights)
		}
		if (any(is.na(error_t_weighted)) || any(is.nan(error_t_weighted))) {
			print("Found NAs")
			error_t_weighted = 0
		}

		# Reweighing the Weights of hypothesis if weighted error is zero to avoid warning at step 7 of algorithm.
		if (error_t_weighted == 0) {
			error_t_weighted = 0.1 * min(example_weights)
		}

		# Compute hypothesis weight (Line 7 of Algorithm)
		Q_t = self$compute_weight(error_t_weighted, view_index, classes)

		return(list("Q_t" = Q_t, 
								"predicted_labels_train" = predicted_labels_train,
								"predicted_labels_test" = predicted_labels_test,
								"model" = mod))
	},


#	This function learns the weights over views.
#	:param initial_guess: initial weights over views.
#	:return: rho : weight over views.

	learn_view_weights = function(initial_guess, example_weights) {
#		print("In learn_view_weights")
		errors_t = list()
		disagreement_t = list()

		# Computing View-Specific Error and disagreement on weighted training data
		for (view_name in self$all_views) {
			classifier_errors = list()
			paired_disagreements = list()

			# compute view-specific error
			for (classifier_output in self$train_predictions_classifiers[[view_name]]) {
				error = as.numeric(classifier_output != self$y_train[[view_name]])
				weighted_error = (t(example_weights) %*% error) / sum(example_weights)
				classifier_errors = append(classifier_errors, weighted_error)
			}
			
			classifier_errors = unlist(classifier_errors)
			classifier_weights = unlist(self$weights_classfiers[[view_name]])
			errors_t = append(errors_t, sum(classifier_errors * classifier_weights))

			# compute view-specific disagreement
			for (index_1 in 1:length(self$train_predictions_classifiers[[view_name]])) {
				classifier_output_1 = self$train_predictions_classifiers[[view_name]][[index_1]]
				
				for (index_2 in 1:length(self$train_predictions_classifiers[[view_name]])) {
					classifier_output_2 = self$train_predictions_classifiers[[view_name]][[index_2]]
					disagreement = as.numeric(classifier_output_1 != classifier_output_2)
					weighted_disagreement = (t(example_weights) %*% disagreement) / sum(example_weights)

					classifier_weights = unlist(self$weights_classfiers[[view_name]])
					weight_1 = classifier_weights[[index_1]]
					weight_2 = classifier_weights[[index_2]]

					paired_disagreements = append(paired_disagreements, weighted_disagreement * weight_1 * weight_2)
				}
			}
			disagreement_t = append(disagreement_t, sum(unlist(paired_disagreements)))
		}

		optimise = MV_Cbound_opt$new(initial_guess, unlist(errors_t), unlist(disagreement_t))
		rho = optimise$learn_weights()
		return(rho)
	},


#	This function computes the C-Bound on the value of gibbs risk and gibbs disagreement.
#	:return: C-bound value

	compute_Cbound = function(risk, disagreement) {
		C_bound = 1-((1-2*risk)**2 / (1-2*disagreement))
		return(C_bound)
	},


#	This function calculates the majority vote
#	:param data : tells on which data we need to compute the majority vote
#	:return: predictions of majority vote

	calculate_majority_vote = function(data = 'train', classes) {
# Convert to multi-class problem using matrix of predictions

		if (data == 'train') {
#			predictions = rep(0, self$num_train_examples)
			predictions = matrix(0, nrow = self$num_train_examples, ncol = length(classes), dimnames = list(NULL, classes))
			classifiers_outputs = self$train_predictions_classifiers
		} else if (data == 'test') {
#			predictions = rep(0, self$num_test_examples)
			predictions = matrix(0, nrow = self$num_test_examples, ncol = length(classes), dimnames = list(NULL, levels(classes)))
			classifiers_outputs = self$test_predictions_classfiers
		}

		for (view_index in 1:length(self$all_views)) {
			view_name = self$all_views[[view_index]]
			
			for (t in 1:length(classifiers_outputs[[view_name]])) {
#				output = as.numeric(classifiers_outputs[[view_name]][[t]])
				output = matrix(0, nrow = nrow(predictions), ncol = length(classes), dimnames = list(NULL, levels(classes)))
				output[cbind(1:nrow(predictions), classifiers_outputs[[view_name]][[t]])] = 1
				classifier_weights = unlist(self$weights_classfiers[[view_name]])
				predictions = predictions + (self$rho[[view_index]] * classifier_weights[t] * output)

			}
		}

#		predictions = sign(predictions)
		y_pred_max = apply(predictions, 1, which.max)
		predictions = levels(classes)[y_pred_max]
		return(predictions)
	},


#	This function will compute the 2nd form of multiview c-bound for mv-boost.
#	:param data : this parameter will tell on which data we have to compute the c-bound.
#	:return: the value of c-bound on input data.

	mv_cbound = function(data = 'train') {
		if (data == 'train') {
			predictions =  self$train_predictions_classifiers
			labels = self$y_train
		}

		errors_t = list()
		disagreement_t = list()
		# example_weights = rep(1, self$num_train_examples) / self$num_train_examples # to not to consider example weights.

		# Computing View-Specific Error and disagreement on weighted training data.(Line 11-12)
		for (view_name in self$all_views) {
			classifier_errors = list()
			paired_disagreements = list()

			# compute view-specific error (Line 11)
			for (classifier_output in predictions[[view_name]]) {
				error = as.numeric(classifier_output != labels[[view_name]])
				weighted_error = mean(error)
				classifier_errors = append(classifier_errors, weighted_error)
			}

			classifier_errors = unlist(classifier_errors)
			classifier_weights = unlist(self$weights_classfiers[[view_name]]) / sum(unlist(self$weights_classfiers[[view_name]]))
			errors_t = append(errors_t, sum(classifier_errors * classifier_weights))

			# compute view-specific disagreement (Line 12)
			for (index_1 in 1:length(predictions[[view_name]])) {
				classifier_output_1 = predictions[[view_name]][[index_1]]
				
				for (index_2 in 1:length(predictions[[view_name]])) {
					classifier_output_2 = predictions[[view_name]][[index_2]]
					disagreement = as.numeric(classifier_output_1 != classifier_output_2)
					weighted_disagreement = mean(disagreement)
					classifier_weights = unlist(self$weights_classfiers[[view_name]]) / sum(unlist(self$weights_classfiers[[view_name]]))

					weight_1 = classifier_weights[index_1]
					weight_2 = classifier_weights[index_2]
					paired_disagreements = append(paired_disagreements, weighted_disagreement * weight_1 * weight_2)
				}

				disagreement_t = append(disagreement_t, sum(unlist(paired_disagreements)))
			}
		}

		rho = self$rho
		risk_total = sum(unlist(errors_t) * rho)
		disagreement_total = sum(unlist(disagreement_t) * rho)
		c_bound = self$compute_Cbound(risk_total, disagreement_total)
		return(c_bound)
	},


#	This function will learn the mvboost model for input multiview learning data.
#	:return: Accuracy and F1 Measure on Training and Test Data. 
#          Also, Multiview C-Bound value on Training Data after T iterations.

	learn = function(train_subset, test_subset, base_learners, base_filters, classes, roc) {
#		print("In learn")
		result_futures = list()
		
		#Initializing weights for training data (Line 1 and 2 of Algorithm)
		w = rep(1/self$num_train_examples, times = self$num_train_examples)

		# T Iterations iterations. (Beginnning of loop at line 4 of Algorithm)
		for (t in 1:self$num_iterations) {
			print(paste0("Iteration: ", t))
			if (t == 1) {
				self$rho = rep(1/length(self$all_views), times = length(self$all_views))   # Line 3 of Algorithm
			}

			#Learn view-specific classifiers and weights over them (Line 5-7) in parallel and wait for results
			for (view_index in 1:length(self$all_views)) {
				view_name = self$all_views[[view_index]]
				result_futures[[view_name]] = future(self$learn_classifier(view_name, view_index, train_subset, test_subset, example_weights = w, classes))
			}
			result_futures[["rho"]] = future({
				#Computing weights over views (Line 8)
				if (t == 1) {
						self$rho = rep(1, length(self$all_views)) / length(self$all_views) #Line 9 of Algorithm.
						self$rho_vectors = list(self$rho)
				} else {
						initial_guess = rep(1, length(self$all_views)) / length(self$all_views)
						self$rho = self$learn_view_weights(initial_guess, w)
						self$rho_vectors[[length(self$rho_vectors) + 1]] = self$rho
				}
			})
			resolve(result_futures)
			
			for (i in 1:length(result_futures)) {
				view_name = names(result_futures)[[i]]
				if (view_name != "rho") {
					results = value(result_futures[[view_name]])
					#Storing the view-specific train and test outputs along with hypothesis weights
					self$train_predictions_classifiers[[view_name]][[length(self$train_predictions_classifiers[[view_name]]) + 1]] = results$predicted_labels_train
					self$test_predictions_classfiers[[view_name]][[length(self$test_predictions_classfiers[[view_name]]) + 1]] = results$predicted_labels_test
					self$weights_classfiers[[view_name]][[length(self$weights_classfiers[[view_name]]) + 1]] = results$Q_t
					self$models[[view_name]][[length(self$models[[view_name]]) + 1]] = results$model
				}
			}


			# Update  weights over training sample (Line 9-10)
			train_predictions = rep(0, self$num_train_examples)
			for (index in 1:length(self$all_views)) {
				view_name = self$all_views[[index]]
				classifier_weights = unlist(self$weights_classfiers[[view_name]])
#				print(paste0("Classifier weights for view ", view_name))
#				print(classifier_weights)
#				print(paste0("View weights for view ", view_name))
#				print(self$rho[[index]])
				predictions = self$rho[[index]] * tail(classifier_weights, 1) * as.numeric(as.character(unlist(tail(self$train_predictions_classifiers[[view_name]], 1))))
				
				if (!all(is.na(predictions))) {
					train_predictions = train_predictions + predictions
				}
			}
			w = w * exp(-1 * train_predictions * as.numeric(self$y_train[[view_name]]))
			w = w / sum(w)

			# Computing Majority-vote error and f1-measure at each iteration.
			test_predictions = self$calculate_majority_vote(data='test', classes)
			train_predictions = self$calculate_majority_vote(data='train', classes)

			results_test = self$compute_stats(predicted_values = test_predictions, true_values = self$y_test[[view_name]])
			results_train = self$compute_stats(predicted_values = train_predictions, true_values = self$y_train[[view_name]])
			c_bound_train = self$mv_cbound(data = 'train')
			feat_scores = get_feature_importances(classes)

#			print(paste0("Accuracy on Training Data: ", 1 - results_train[["error"]]))
#			print(paste0("F1 Score on Training Data: ", results_train[["f1"]]))
#			print(paste0("Multiview C-Bound  Training Data: ", c_bound_train))
#			print(paste0("Accuracy on Test Data: ", 1 - results_test[["error"]]))
#			print(paste0("F1 Score on Test Data: ", results_test[["f1"]]))
#			print("===========================================")
		}

		return(list(
							"response" = test_predictions, 
							"truth" = self$y_test[[view_name]],
							"test_acc" = 1 - results_test[["error"]], 
							"test_f1" = results_test[["f1"]], 
							"train_acc" = 1 - results_train[["error"]], 
							"train_f1" = results_train[["f1"]], 
							"c_bound" = c_bound_train, 
							"feats" = feat_scores))
	},
		

	# Extract the feature importance scores from each model on each iteration on one fold of data.
	# Then multiple these by the weights for each model.
	get_feature_importances = function(classes) {
		print("In get_feature_importances")
#		print(self$models)
		feats = list()
		feat_scores = list()
		
		# First extract the feature importance scores from the saved models for each task
		for (view_name in self$all_views) {
#			print(paste0("view_name = ", view_name))
			feats[[view_name]] = list()
	
#			print(self$models[[view_name]])
			for (i in 1:length(self$models[[view_name]])) {
				if (isFailureModel(self$models[[view_name]][[i]])) {
					print(paste0("Model for ", view_name, " failed on iteration ", i))
					print(getFailureModelMsg(self$models[[view_name]][[i]]))
				} else {
					scores = getFeatImpScores(getLearnerModel(self$models[[view_name]][[i]], more.unwrap = TRUE), classes)
					selected = getFilteredFeatures(getLearnerModel(self$models[[view_name]][[i]], more.unwrap = FALSE))
					not_selected = setdiff(getTaskFeatureNames(self$tasks[[view_name]]), selected)
					feats[[view_name]][[i]] = scores[, "all"]
					names(feats[[view_name]][[i]]) = rownames(scores)
					if (length(not_selected) > 0) {
						feats[[view_name]][[i]][not_selected] = 0
						names(feats[[view_name]][[i]][not_selected]) = not_selected
					}
				}
			}

			# Calculate the weighted scores using the model weights
			df = as.data.frame(dplyr::bind_rows(feats[[view_name]]))
			seln_counts = colSums(df != 0)
			if (length(self$weights_classfiers) > 1) {
				feat_scores[[view_name]] = colSums(sapply(df, function(x) {unlist(self$weights_classfiers) * unlist(x)}), na.rm = TRUE) / sum(unlist(self$weights_classfiers))
			}
			feat_scores[[view_name]][seln_counts <= nrow(df)/2] = 0
		}		
		return(feat_scores)
	})
)

