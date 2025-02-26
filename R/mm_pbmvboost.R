#' R6 Class representing a multi-modal PB-MVBoost classifier
#'
#' @description
#' Creates a multi-modal PB-MVBoost classifier, based on boosting.
#'
#' @details
#' This classifier is based on a two-level multiview learning approach. It learns the distribution over view-specific
#' classifiers and the distribution over views in one step following a boosing approach.
#'
#' @references
#' Multiview Boosting by Controlling the Diversity and the Accuracy of View-specific Voters
#' by Anil Goyal, Emilie Morvant, Pascal Germain and Massih-Reza Amini
#' https://arxiv.org/abs/1808.05784
#'
#' Original Author = "Anil Goyal"
#' Author of the R implementation - Annette Spooner
#'
#' @name PB_MVBoost
#' @docType package
NULL

PB_MVBoost = R6::R6Class("PB_MVBoost", 
	inherit = MM_Model,
	public = list(
		all_views = list(),
		num_train_examples = 0,
		num_test_examples = 0,
		nrounds = 10,
		decision_tree_depth = 2,
		nb_view_classifiers = NULL,
		X_train = NULL,
		X_test = NULL,
		y_train = NULL,
		y_test = NULL,
		train_predictions_classifiers = list(),
		validation_predictions_classifiers = list(),
		test_predictions_classfiers = list(),
		weights_classfiers = list(),
		rho = list(),
		rho_vectors = list(),

			#' @description 
			#' Create a new PB_MVBoost object.
			#' @param config (MM_Config)\cr
			#' Configuration object, specifying how the model should be constructed.
			#' @param model_type (character)\cr
			#' Type of model - "CLASSIF" for classification or "SURV" for survival analysis. 
			#' @param decision (character)\cr
			#' Type of prediction - 'response' or 'prob'.
			#' @param subset (integer)\cr
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
			#' @return A new`PB_MVBoost` object.
			#' @export
		initialize = function(config, nrounds = 10, decision_tree_depth = 2, decision = "prob", subset = NULL, balance = FALSE, validate = FALSE, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
				super$initialize(config, "CLASSIF", decision, subset, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)
				self$all_views = names(self$tasks)
				self$nb_view_classifiers = rep(0, length(self$all_views))

				self$nrounds = nrounds
				self$decision_tree_depth = decision_tree_depth

				# Variables to store the train and test predictions and weights after each iteration
				for (view_name in self$all_views) {
					self$train_predictions_classifiers[[view_name]] = list()
					self$test_predictions_classfiers[[view_name]] = list()
					self$weights_classfiers[[view_name]] = list()
					private$models[[view_name]] = list()
				}
		},
		
		#' @description 
		#' Format the data for one fold as PB_MVBoost expects it:
		#'   4 named lists with one element per view
    #' @param train_subset (integer)\cr
		#' A vector containing the indices of the samples in the training set.
    #' @param test_subset (integer)\cr
		#' A vector containing the indices of the samples in the test set.
    #' @return Nothing but the relevant internal variables are set up.
		#' @noRd
		format_data = function(train_subset, test_subset) {
			self$X_train = list()
			self$y_train = list()
			self$X_test = list()
			self$y_test = list()
			
			for (i in 1:length(self$tasks)) {
				task_id = self$tasks[[i]]$task.desc$id
				dat = getTaskData(self$tasks[[i]])
				
				# Convert y to numeric
				y = dat[, self$targetVar]
				if (task_id == 'Clinical') {
					X = dat[, !colnames(dat) %in% c(self$targetVar, 'ID', 'DOB', 'Age', 'Gender', 'Ethnicity')]
				} else {
					X = dat[, !colnames(dat) %in% c(self$targetVar, 'ID')]
				}
					
				self$X_train[[task_id]] = X[train_subset, ]
				self$X_test[[task_id]]  = X[test_subset, ]
				self$y_train[[task_id]] = y[train_subset]
				self$y_test[[task_id]]  = y[test_subset]
			}	
			
			self$num_train_examples = length(self$y_train[[1]])
			self$num_test_examples = length(self$y_test[[1]])
		},


		#' @description 
		#' A helper function to compute the weight of the hypothesis based on the error passed to it.\cr
		#'	It computes 0.5 * ln (1-error/ error)
    #' @param error (integer)\cr
		#' The error value.
    #' @param view_index (integer)\cr
		#' The view for which the error is caluclated.
		#' @param classes (factor)\cr
		#' The classes in the target variable
    #' @return The weight value
		#' @noRd
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


		#' @description 
		#' Computes the error and f1-score.
    #' @param predicted_values (vector)\cr
		#' The predicted labels of any classifier.
    #' @param true_values (integer)\cr
		#' The true labels of the classifier.
		#' @return A list containing the error and F1 score
		#' @noRd
		compute_stats = function(predicted_values, true_values) {
			# remove the elements with output zero.
			predicted_values = as.numeric(predicted_values[predicted_values != 0])
			true_values      = as.numeric(true_values[predicted_values != 0])

	#		error = mean(predicted_values * true_values <= 0.0)
			error = mean(predicted_values != true_values)
			f1 = measureF1(true_values, predicted_values, 1)
			return(list("error" = error, "f1" = f1))
		},


		#' @description 
		#' This function learns the weak classifier and returns a weight for this learned classifier. 
		#' Fitting is done on weighted samples which are passed as an input parameter.
		#' The classifier is trained on one view on one fold of data.
		#' @param view_name (character)\cr
		#' The name of the view  for which we need to learn a classfier
		#' @param view_index (character)\cr
		#' The index of the view  for which we need to learn a classfier		
    #' @param train_subset (integer)\cr
		#' A vector containing the indices of the samples in the training set.
    #' @param test_subset (integer)\cr
		#' A vector containing the indices of the samples in the test set.
		#' @param example_weights (numeric)\cr: 
		#' Weight of the input training examples
		#' @param classes (factor)\cr
		#' The classes in the target variable
		#' @return Weight of Classifier, training data labels, test data labels
		#' @noRd
		learn_classifier = function(view_name, view_index, train_subset, test_subset, example_weights, classes) {
			X_train = self$X_train[[view_name]]
			X_test = self$X_test[[view_name]]
			y_train = self$y_train[[view_name]]

			#Learning classifier
			# The weights change on each iteration, but mlrCPO does not handle weights.
			# Therefore they can't be passed through the pipeline, so they have to be
			# handled by taking a weighted sample of the training set and using that.
			
			wght_sample = sample(train_subset, length(train_subset), replace = TRUE, prob = example_weights)
			mod = mlr::train(learner = self$learners[[view_index]], task = self$tasks[[view_index]], subset = wght_sample)
			pred_train = mlr::predictLearner(mod, task = self$tasks[[view_index]], subset = train_subset)
			pred_test  = mlr::predictLearner(mod, task = self$tasks[[view_index]], subset = test_subset)

			#predicting labels for training and test data
			predicted_labels_train = pred_train$data$response
			predicted_labels_test  = pred_test$data$response
			
			#computing error
			if (mlr::isFailureModel(mod)) {
				warning(paste0("Model ", view_index, " failed: ", mlr::getFailureModelMsg(mod)))
				error_t_weighted = 0
			} else {
				error_t = as.numeric(as.character(predicted_labels_train) != as.character(y_train))
				error_t_weighted = (t(example_weights) %*% error_t) / sum(example_weights)
			}
			if (any(is.na(error_t_weighted)) || any(is.nan(error_t_weighted))) {
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


		#' @description 
		#' This function learns the weights over views.
		#' @param initial_guess (numeric)\cr
		#' Initial weights over the views.
		#' @param example_weights (numeric)\cr: 
		#' Weight of the input training examples
		#' @return rho - the weight over the views.
		#' @noRd
		learn_view_weights = function(initial_guess, example_weights) {
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


		#' @description 
		#' Computes the C-Bound on the value of gibbs risk and gibbs disagreement.
		#' @param risk (numeric)\cr
		#' Gibbs risk.
		#' @param disagreement (numeric)\cr: 
		#' Gibbs disagreement
		#' @return C-bound value.
		#' @noRd
		compute_Cbound = function(risk, disagreement) {
			C_bound = 1-((1-2*risk)**2 / (1-2*disagreement))
			return(C_bound)
		},


		#' @description 
		#' Calculates predictions using a majority vote. 
		#' Converted to multi-class problem using matrix of predictions		
		#' @param data (data.frame)\cr
		#' The data on which to compute the majority vote.
		#' @param classes (factor)\cr
		#' The classes in the target variable
		#' @return predictions using a majority vote
		#' @noRd
		calculate_majority_vote = function(data = 'train', classes) {

			if (data == 'train') {
				predictions = matrix(0, nrow = self$num_train_examples, ncol = length(classes), dimnames = list(NULL, classes))
				classifiers_outputs = self$train_predictions_classifiers
			} else if (data == 'test') {
				predictions = matrix(0, nrow = self$num_test_examples, ncol = length(classes), dimnames = list(NULL, levels(classes)))
				classifiers_outputs = self$test_predictions_classfiers
			}

			for (view_index in 1:length(self$all_views)) {
				view_name = self$all_views[[view_index]]
				
				for (t in 1:length(classifiers_outputs[[view_name]])) {
					output = matrix(0, nrow = nrow(predictions), ncol = length(classes), dimnames = list(NULL, levels(classes)))
					output[cbind(1:nrow(predictions), classifiers_outputs[[view_name]][[t]])] = 1
					classifier_weights = unlist(self$weights_classfiers[[view_name]])
					predictions = predictions + (self$rho[[view_index]] * classifier_weights[t] * output)

				}
			}

			y_pred_max = apply(predictions, 1, which.max)
			predictions = levels(classes)[y_pred_max]
			return(predictions)
		},


		#' @description 
		#' Computes the 2nd form of multiview c-bound for mv-boost. 	
		#' @param data (data.frame)\cr
		#' The data on which to compute the c-bound.
		#' @return the value of c-bound on the input data
		#' @noRd
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


		#' @description 
		#' Learn the pbmvboost model for one fold of data. 	
    #' @param train_subset (integer)\cr
		#' A vector containing the indices of the samples in the training set.
    #' @param test_subset (integer)\cr
		#' A vector containing the indices of the samples in the test set.
		#' @param classes (factor)\cr
		#' The classes in the target variable
		#' @param fold (integer)\cr
		#' The fold of data to be used in learning the model.
		#' @return Accuracy and F1 Measure on Training and Test Data.\cr 
		#' Also, Multiview C-Bound value on Training Data after T iterations.
		#' @noRd
		learn_pbmv = function(train_subset, test_subset, classes, fold) {
			result_futures = list()
			
			#Initializing weights for training data (Line 1 and 2 of Algorithm)
			w = rep(1/self$num_train_examples, times = self$num_train_examples)

			# T Iterations iterations. (Beginnning of loop at line 4 of Algorithm)
			for (t in 1:self$nrounds) {
				if (t == 1) {
					self$rho = rep(1/length(self$all_views), times = length(self$all_views))   # Line 3 of Algorithm
				}

				#Learn view-specific classifiers and weights over them (Line 5-7) in parallel and wait for results
				for (view_index in 1:length(self$all_views)) {
					view_name = self$all_views[[view_index]]
					result_futures[[view_name]] = future::future(self$learn_classifier(view_name, view_index, train_subset, test_subset, example_weights = w, classes))
				}
				result_futures[["rho"]] = future::future({
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
				future::resolve(result_futures)
				
				for (i in 1:length(result_futures)) {
					view_name = names(result_futures)[[i]]
					if (view_name != "rho") {
						results = value(result_futures[[view_name]])
						#Storing the view-specific train and test outputs along with hypothesis weights
						self$train_predictions_classifiers[[view_name]][[length(self$train_predictions_classifiers[[view_name]]) + 1]] = results$predicted_labels_train
						self$test_predictions_classfiers[[view_name]][[length(self$test_predictions_classfiers[[view_name]]) + 1]] = results$predicted_labels_test
						self$weights_classfiers[[view_name]][[length(self$weights_classfiers[[view_name]]) + 1]] = results$Q_t
						private$models[[view_name]][[length(private$models[[view_name]]) + 1]] = results$model
					}
				}


				# Update  weights over training sample (Line 9-10)
				train_predictions = rep(0, self$num_train_examples)
				for (index in 1:length(self$all_views)) {
					view_name = self$all_views[[index]]
					classifier_weights = unlist(self$weights_classfiers[[view_name]])
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
			}
			feat_scores = self$get_feature_importances(fold)
			
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
			

		#' @description 
		#' Calculate the weighted feature importance scores from each model on each iteration on one fold of data.
		#' The raw feature importance scores are multiplied by the weights for each model.
		#' @param fold (integer)\cr
		#' The fold of data for which feature importance scores are to be calculated..
		#' @return The feature importance scores for the given fold. 
		#' @noRd
		get_feature_importances = function(fold) {
			feats = list()
			feat_scores = list()
			
			# First extract the feature importance scores from the saved models for each task
			for (view_name in self$all_views) {
				feats[[view_name]] = list()
		
				for (i in 1:length(private$models[[view_name]])) {
					if (mlr::isFailureModel(private$models[[view_name]][[i]])) {
						warning(paste0("Model for ", view_name, " failed on iteration ", i))
						warning(mlr::getFailureModelMsg(private$models[[view_name]][[i]]))
					} else {
						self$results$save_features(private$models[[view_name]][[i]], self$tasks[[view_name]], "PBMV", fold)
					}
				}

				# Calculate the weighted scores using the model weights
				df = as.data.frame(dplyr::bind_rows(self$results$feats$featsel[[view_name]]))
				seln_counts = colSums(df != 0)

				if (length(self$weights_classfiers) > 1) {
					feat_scores[[view_name]] = colSums(sapply(df, function(x) {unlist(self$weights_classfiers) * unlist(x)}), na.rm = TRUE) / sum(unlist(self$weights_classfiers))
				}
				feat_scores[[view_name]][seln_counts <= nrow(df)/2] = 0
			}		
			return(feat_scores)
		},
		
		
		#' @description 
		#' Learn a pb_mvboost model.
		#' @return The model results. 
		#' @export
		learn = function()
		{
			final_response = list()
			tree_depth = 2

			for (rep in 1:self$ri$desc$reps) {
				fold_responses = list()
				for (fold in 1:self$ri$desc$folds) {
					subset_idx = (rep - 1) * self$ri$desc$folds + fold
					train_subset = self$ri$train.inds[[subset_idx]]
					test_subset = self$ri$test.inds[[subset_idx]]
					
					# Train and predict using PB_MVBoost classifier
					self$format_data(train_subset, test_subset)
					pbmv_res = self$learn_pbmv(train_subset, test_subset, self$classes, fold)

					fold_result = data.frame('id' = test_subset, 
																	 'ID' = row.names(self$X_train[[1]][test_subset, ]), 
																	 'truth' = pbmv_res$truth,
																	 'response' = pbmv_res$response)
					self$results$save_responses(fold_result, rep, fold)
					fold_responses[[fold]] = fold_result
				}
				
				# Combine the responses for each fold
				final_response[[rep]] = dplyr::bind_rows(fold_responses)
				final_response[[rep]] = final_response[[rep]] %>% arrange(id)
				final_response[[rep]][, "rpt"] = rep
			}
				
			# Combine the responses for each repeat into a single data frame
			all_responses = dplyr::bind_rows(final_response)
						
			if (self$validation) {
				self$validate()
			}
			
			self$results$complete("PB-MVBoost")
			return(self$results)
		}

	),	
		
	private = list(
		#' @field models (list)
		#' List of the models created in training.
		models   = list()	
	)

)

