library(mice)
library(mlr)
library(mlrCPO)
library(dplyr)
library(tryCatchLog)


source("const.R")
source("config.R")
source("imputation.R")
source("normalisation.R")
source("roc_measures.R")
source("perf_measures.R")
source("filter_boruta.R")
source("filter_sis.R")
source("performance_classfn.R")
source("stability.R")
source("mm_features.R")
source("learners.R")
source("feature_selectors.R")
source("data_helpers.R")


Combination = R6Class("Combination", list(
	tasks    = NULL,
	learners = NULL,
	models   = list(),
	feats		 = list(),
	target_var   = "",
	
	initialize = function(tasks, learners, target) {
		self$tasks = tasks
		self$learners = learners
		self$target_var = target
		print(paste0("Number of tasks: ", length(tasks)))
		print(paste0("Number of learners: ", length(learners)))
	},


	# Calculate the final response, according to the decision_type type and
	# add a response column to the results
	#
	get_final_decision = function(results, classes, decision_type, task_names) {
		print("In get_final_decision (combn)")
#		print(classes)
#		print(task_names)
		final_results = NULL
#		prev_results = NULL
		if (decision_type %in% c('vote', 'hard')) {
			# Calculate final prediction with a majority vote across modalities
			raw_responses = as.data.frame(results[,!colnames(results) %in% c('id', 'ID', 'truth')])
			final_results = results[, c('id', 'ID', 'truth')]
			final_results$response = as.factor(apply(raw_responses, 1, function(x) names(which.max(table(x)))))
			
		} else if (decision_type %in% c('prob', 'soft')) {
			# Calculate sum of probabilities for each class and take max of that as prediction
			assert(length(classes) == 2)
			ovr_class = ifelse(classes[1] == 'REST', classes[2], classes[1])
#			print(ovr_class)
#			acc = list()
#			for (tsk in task_names) {
#				tmp = results[, c('truth', paste0(tsk, '.', ovr_class), paste0(tsk, '.', 'REST'))]
#				tmp$response = ifelse(tmp[, paste0(tsk, '.', ovr_class)] > tmp[, paste0(tsk, '.', 'REST')], ovr_class, 'REST')
#				acc[tsk] = sum(tmp$response == tmp$truth) / nrow(tmp)
#				print(paste0("Accuracy of task ", tsk, " = ", acc[tsk]))					
#			}
#			print(colnames(results)[grepl(paste0("\\<", ovr_class, "\\>"), colnames(results))])
#			finished = FALSE
#			accuracies = sort(unlist(acc), decreasing = TRUE)
#			print(paste0("Max accuracy = ", accuracies[[1]], " in modality ", names(accuracies)[1]))

#			curr_idx = 1
#			curr_list = list(names(accuracies)[curr_idx])
#			curr_acc = accuracies[curr_idx]
#			prev_results = results[, c('id', 'ID', 'truth')]
#			prev_results = cbind(prev_results, results[, c(paste0(names(accuracies)[curr_idx], '.', ovr_class), paste0(names(accuracies)[curr_idx], '.', 'REST'))])
#			prev_results[, paste0('prob.', ovr_class)] = prev_results[, paste0(names(accuracies)[curr_idx], '.', ovr_class)]
#			prev_results[, paste0('prob.', "REST")]    = prev_results[, paste0(names(accuracies)[curr_idx], '.', "REST")] 
#			prev_results$response = apply(prev_results[, grepl("prob.", colnames(prev_results))], 1, function(x) names(which.max(x)))
#			prev_results$response = strsplit(as.character(prev_results$response), ".", fixed = TRUE)
#			prev_results$response = as.factor(sapply(prev_results$response, "[[", 2))
#			print(head(prev_results))
			
#			while (!finished && curr_idx < length(accuracies)) {
#				curr_idx = curr_idx + 1
#				curr_list = append(curr_list, names(accuracies)[curr_idx])
#				print(paste(curr_list, collapse = "|"))
#				tmp = results[, grepl(paste(curr_list, collapse = "|"), colnames(results))]
#				print(colnames(tmp))
				
#					if (classes[i] == "CIR") {
#							results = results[, grep("CLIN|CYT|MET|OG", colnames(results))]
#					} else if (classes[i] == "CON") {
#							results = results[, grep("CYT|MET", colnames(results))]
#					} else if (classes[i] == "LN") {
#							results = results[, grep("CLIN|CYT|MET", colnames(results))]
#					} else if (classes[i] == "LX") {
#							results = results[, grep("CLIN|PATH|MET|LIP", colnames(results))]
#					}

				final_results = results[, c('id', 'ID', 'truth')]
#				final_results = cbind(final_results, tmp)
#				final_results[, paste0('prob.', ovr_class)] = rowSums(as.data.frame(tmp[, grepl(paste0("\\<", ovr_class, "\\>"), colnames(tmp))]), na.rm = TRUE)
#				final_results[, paste0('prob.', "REST")] = rowSums(as.data.frame(tmp[, grepl(paste0("\\<", "REST", "\\>"), colnames(tmp))]), na.rm = TRUE)

				final_results[, paste0('prob.', ovr_class)] = rowSums(as.data.frame(results[, grepl(paste0("\\<", ovr_class, "\\>"), colnames(results))]), na.rm = TRUE)
				final_results[, paste0('prob.', "REST")] = rowSums(as.data.frame(results[, grepl(paste0("\\<", "REST", "\\>"), colnames(results))]), na.rm = TRUE)
#				print(colnames(final_results))
				final_results$response = apply(final_results[, grepl("prob.", colnames(final_results))], 1, function(x) names(which.max(x)))					
				if (!is.null(final_results$response)) {
					final_results$response = strsplit(as.character(final_results$response), ".", fixed = TRUE)
					final_results$response = as.factor(sapply(final_results$response, "[[", 2))
				} else {
					print("Error: Response is NULL!")
				}
#				print(head(final_results))
#				new_acc = sum(final_results$response == final_results$truth) / nrow(final_results)
#				print(paste0("New accuracy: ", new_acc))
#				if (new_acc <= curr_acc) {
#						finished = TRUE
#						print("Finished")
#						final_results = prev_results[, c('id', 'ID', 'truth', paste0('prob.', ovr_class), 'prob.REST', 'response')]
#						print(colnames(final_results))
#				} else {
#					prev_results = final_results
#					print("Not finished")
#					print(colnames(prev_results))
#				}
#				curr_acc = new_acc			
#			}
		} else {
			print("Invalid decision type")
		}
		
		return(final_results)
	},
	

	#
	# Train the  models on one fold of the training data 
	# Then predict on the test data for that fold
	# Gives a set of predictions: 1 per fold per modality
	#
	train_base_mods = function(tasks, learners, training_set, test_set, fold_num, sample_ids, decision, classes, feats) 
	{
		print(paste0("In train_base_mods: ", fold_num))
#		print(classes)
#		print(training_set)
#		print(test_set)
#		print(sample_ids)
#		responses = list()
		responses = NULL
		accuracies = list()
				
		for (i in 1:length(tasks)) {
			lrn_idx = ifelse(length(tasks) == length(learners), i, 1L)
			task_id = tasks[[i]]$task.desc$id
			print(paste0("Task: ", task_id))

			mod = train(learner = learners[[lrn_idx]], task = tasks[[i]], subset = training_set)
			
			if (isFailureModel(mod)) {
				print(paste0("Model ", task_id, " failed on iteration ", fold_num))
				print(getFailureModelMsg(mod))
			} else {
			
				# Save feature importance scores
				scores = getFeatImpScores(getLearnerModel(mod, more.unwrap = TRUE), classes)
				selected = getFilteredFeatures(getLearnerModel(mod, more.unwrap = FALSE))
				not_selected = setdiff(getTaskFeatureNames(tasks[[i]]), selected)
				feat_scores = scores[, "all"]
				names(feat_scores) = rownames(scores)
				if (length(not_selected) > 0) {
					feat_scores[not_selected] = 0
					names(feat_scores[not_selected]) = not_selected
				}
				feats$save("combn", feat_scores, task_id, fold_num)

				# Prediction
				pred = predict(mod, tasks[[i]], subset = test_set)
#				print("Prediction:")
#				print(pred$data)
				
				if (is.null(responses)) {
					responses = pred$data[, c('id', 'truth')]
					responses$ID = sample_ids[sample_ids$id %in% test_set, 'ID']  # From Old MOE
#					responses$ID = rownames(pred$data)
				}
				if ((decision == 'vote') || (decision == 'hard')) {
					res = pred$data[, 'response', drop = FALSE]
					res$ID = rownames(pred$data)
					responses[, task_id] = res[match(responses$ID, res$ID), 'response']
				} else if ((decision == 'prob') || (decision == 'soft')) {
					probs = pred$data[, grepl("prob.", colnames(pred$data))]
					prob_cols = gsub("prob", task_id, colnames(probs))
#					print(prob_cols)
					probs$ID = rownames(pred$data)
					responses[, prob_cols] = probs[match(responses$ID, probs$ID), grepl("prob.", colnames(probs)), drop = FALSE]
							
#					probs = pred$data[, grepl("prob.", colnames(pred$data))]
#					prob_cols = colnames(probs)
#					print(prob_cols)
#					new_cols = gsub("prob", task_id, colnames(probs))
#					probs$ID = rownames(pred$data)
#					responses[, new_cols] = probs[match(responses$ID, probs$ID), prob_cols, drop = FALSE]
#					print("Responses")
#					print(responses)
				}
											
			}
		}
#		print("Responses")
#		print(responses)
		return(as.data.frame(responses))
	})
)