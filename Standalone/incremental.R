#-------------------------------------------------------------
# CLASS Incremental
# Forward/backward sequential building of a sequence of views
#-------------------------------------------------------------

library(BBmisc)
library(R6)
library(testthat)
library(mice)
library(mlr)

epsilon = 0.01


Incremental = R6Class("Incremental", list(
	target_var = "",
	tasks      = NULL,
	learners   = NULL,
	classes		 = NULL,
	perf_type  = "F1",  # Metric for deciding on best performance - F1 Score, Accuracy or AUC
	perf			 = NULL,	# List of data.frames, one per modality, containing performance for each base model
	feats 		 = NULL,	# List of data.frames, one per modality, containing features selected
	predns		 = NULL,	# List of data.frames, one per modality, containing predictions across all folds
	curr_set   = NULL,	# List of names of modalities in current set
	remainder  = NULL,	# List of modalities left to choose from
	curr_perf  = NULL,	# List of performance scores for each modality added to the current set
	
	initialize = function(tasks, learners, target, classes, perf_type) {
		self$tasks      = tasks
		self$feats 			= MM_Features$new(self$tasks)
		self$learners   = learners
		self$target_var = target
		self$classes    = classes
		self$perf_type  = perf_type
		self$perf   		= list()
		self$predns 		= list()
		self$curr_set   = list()								
		self$remainder  = list()
		self$curr_perf  = list()
	},
	
	
#########################################################################################
#
	calc_stats = function(truth, response, class_names) {
		print("In calc_stats")
		print(class_names)
		response = factor(response, levels = levels(truth))  # In case some classes were not predicted
		measures = list()
		neg_class = "ALL"
		tt = table(response)
		print(tt)

		for (j in 1:length(class_names)) {
			print(paste0("j = ", j))
			pos_class = class_names[[j]]
			print(paste0("Positive class: ", pos_class))
#			if (pos_class != neg_class && !is.na(tt[pos_class]) && as.integer(tt[pos_class]) > 0) {
			if (!is.na(tt[pos_class]) && as.integer(tt[pos_class]) > 0) {
				tmp_truth = as.factor(truth)
				tmp_response = as.factor(response)
				levels(tmp_truth)[levels(tmp_truth) != class_names[[j]]] = neg_class
				levels(tmp_response)[levels(tmp_response) != class_names[[j]]] = neg_class
				tp = measureTP(truth, response, as.character(pos_class))
				tn = measureTN(truth, response, neg_class)
				fp = measureFP(truth, response, as.character(pos_class))
				fn = measureFN(truth, response, neg_class)
				acc = (tn + tp) / (tp + fp + tn + fn)
				f1 = measureF1(tmp_truth, tmp_response, as.character(pos_class))
				measures[[j]] = list("F1" = f1, "Acc" = acc)
			} else {
				measures[[j]] = list("F1" = 0, "Acc" = 0)
			}
		}
		names(measures) = class_names
		print(unlist(measures))
		return(as.data.frame(measures))
	},
	
	##########################################################################################
	# Train a model on each view for each fold. These will be reused.
	#
	train_base_models = function(ri, decision) {
		print(paste0("In train_base_models: ", length(self$tasks)))
		predn_futures = list()
		
		print("Training...")
		for (i in 1:length(self$tasks)) {
			task_id = names(self$tasks)[[i]]
			print(paste0("Task ", task_id))
			predn_futures[[task_id]] = list()
			
			for (rep in 1:ri$desc$reps) {
				for (fold in 1:ri$desc$folds) {
					subset_idx = (rep - 1) * ri$desc$folds + fold
					print(paste0("Subset_idx = ", subset_idx))
					train_subset = ri$train.inds[[subset_idx]]
					test_subset = ri$test.inds[[subset_idx]]
				
					predn_futures[[task_id]][[subset_idx]] = future( {
						mod = train(self$learners[[i]], self$tasks[[i]], subset = train_subset)
									
						# Save feature importance scores
						scores = getFeatImpScores(getLearnerModel(mod, more.unwrap = TRUE), self$classes)
						selected = getFilteredFeatures(getLearnerModel(mod, more.unwrap = FALSE))
						not_selected = setdiff(getTaskFeatureNames(self$tasks[[i]]), selected)
						feat_scores = scores[, "all"]
						names(feat_scores) = rownames(scores)
						if (length(not_selected) > 0) {
							feat_scores[not_selected] = 0
							names(feat_scores[not_selected]) = not_selected
						}
#						print("Feature importance scores:")
#						print(feat_scores)
						self$feats$save("inc", feat_scores, task_id, subset_idx)

						if (isFailureModel(mod)) {
							print(paste0("Model ", i, " failed: ", getFailureModelMsg(mod)))
						} else {
							print(paste0("Predicting task ", i))
							predict(mod, task = self$tasks[[i]], subset = test_subset)
						}
					})
				}
			}
		}
								
		# Wait for results
		resolve(predn_futures)
		print("Futures resolved")
		predns = list()
		perf = list()

		for (i in 1:length(self$tasks)) {
			task_id = names(self$tasks)[[i]]
			print(paste0("Task: ", task_id))
			predns[[task_id]] = list()
			perf[[task_id]] = list()
			
			for (rep in 1:ri$desc$reps) {
				print(paste0("Rep = ", rep))
				fold_predns = list()
				fold_perf = list()
				
				for (fold in 1:ri$desc$folds) {
					subset_idx = (rep - 1) * ri$desc$folds + fold
					print(paste("Subset Index = ", subset_idx))
					res = value(predn_futures[[task_id]][[subset_idx]])
					print("Results of prediction:")
					print(res$data)
					fold_predns[[fold]] = res$data
					fold_perf[[fold]] = self$calc_stats(res$data$truth, res$data$response, self$classes)
				}
				
				predns[[task_id]][[rep]] = bind_rows(fold_predns)
				predns[[task_id]][[rep]] = predns[[task_id]][[rep]] %>% arrange(id)
				predns[[task_id]][[rep]][, "rpt"] = rep
				perf[[task_id]][[rep]] = bind_rows(fold_perf)
				perf[[task_id]][[rep]][, "rpt"] = rep
			}

			self$predns[[task_id]] = bind_rows(predns[[task_id]])
			print(paste0("Predictions for task: ", task_id))
			print(self$predns[[task_id]])
			self$perf[[task_id]]   = bind_rows(perf[[task_id]])
			print(paste0("Performance for task: ", task_id))
			print(self$perf[[task_id]])
		}
	},

	
	##########################################################################################
	# Calculate the final response, according to the decision type from the given set of views
	#
	get_response = function(results, views, exp_type, decision_type, config) {	
		print(paste0("In get_response: ", exp_type, ", ", decision_type))
		print(unlist(views))
		if (length(views) <= 0) {
			print("Error: No views to get response from")
			return(results)
		}

		task_subset = self$tasks[unlist(views)]	
		if (decision_type == 'vote') {
			res = list()
			for (i in 1:length(views)) {
				res[[views[[i]]]] = results[[views[[i]]]][['response']]
			}
			curr_resp = as.data.frame(res)
			
			# Calculate final prediction with a majority vote across views and add 'response' column
			if (length(views) == 1) {
				curr_resp[, "response"] = curr_resp
				rownames(curr_resp) = rownames(results)
			} else {
				curr_resp$response = as.factor(apply(curr_resp, 1, function(x) names(which.max(table(x)))))
			}
			
		} else if (decision_type == 'prob') {
			res = list()
			for (i in 1:length(views)) {
				print(paste0("i = ", i))
#				print(results[[views[[i]]]])
				res[[views[[i]]]] = results[[views[[i]]]][, grepl('prob', colnames(results[[views[[i]]]]))]
			}
			curr_resp = as.data.frame(res)
			print("Current response:")
			print(curr_resp)

			# Calculate sum of probabilities for each class and take max of that as prediction
			for (i in 1:length(self$classes)) {
				curr_resp[, paste0('sum.', self$classes[i])] = rowSums(as.data.frame(curr_resp[, grepl(self$classes[i], colnames(curr_resp))]), na.rm = TRUE)
			}
			curr_resp$response = apply(curr_resp[, grepl("sum.", colnames(curr_resp))], 1, function(x) names(which.max(x)))
			tmp = strsplit(curr_resp$response, '.', fixed = TRUE)
			curr_resp$response = sapply(tmp, function(x) {tail(x, n=1)})
		}
		
		curr_resp$id = results[[1]]$id
		curr_resp$truth = results[[1]]$truth
		curr_resp$rpt = results[[1]]$rpt
		print("Finally:")
		print(curr_resp)
		return(curr_resp)
	},
	
	
	##########################################################################################
	# Train a multi-modal (multi-view) incremental model in a forward manner
	# Add the best performing view on each iteration until performance drops
	#
	train_incremental_forward = function(config, tasks, metric, exp_type, decision) {
		print(paste0("In train_incremental_forward: ", metric))
		finished = FALSE
		
		# Change this bit to calc avg perf for each task
		avg_perf = sapply(self$perf, function(x) {rowMeans(x[, grepl(metric, colnames(x))])}) # Gives avg over classes
		avg_perf = colMeans(avg_perf) 	# Gives average over folds
		print("Average Performance:")
		print(avg_perf)
		curr_best_perf = 0
		
		curr_best_idx = which.max(avg_perf)
		curr_best_view = names(tasks)[curr_best_idx] 
		curr_best_val  = avg_perf[curr_best_idx]
		self$curr_set  = list(curr_best_view)  # Add curr_best_view to current set of views
		print(paste0("curr_best_idx = ", curr_best_idx, ", curr_best_view = ", curr_best_view, ", curr_best_val = ", curr_best_val))
		print("Best initial set:")
		print(self$curr_set)
		self$remainder  = as.list(names(tasks))
		
		while (!finished) {
			# Update the current set to include the next best performer
			self$curr_perf = list()
		  self$remainder[[curr_best_idx]] = NULL		# Remove curr_best_view from remainder

			# Get performance stats for each of the remaining views added to the current set
			for (view in self$remainder) {
				curr_views = append(self$curr_set, view)
				resp = self$get_response(self$predns, unlist(curr_views), exp_type, decision, config)
				perf = self$calc_stats(resp$truth, resp$response, self$classes)
				print(perf)
				avg_perf = rowMeans(perf[, grepl(metric, colnames(perf))])
				print("Average performance:")
				print(avg_perf)
				self$curr_perf[[view]] = avg_perf
			}
			
			# Choose the next best performer to add
			print("Current performance:")
			print(self$curr_perf)
			best_idx = which.max(unlist(self$curr_perf))
			best_view = names(self$curr_perf)[best_idx] 
			best_val  = self$curr_perf[[best_idx]]
			print(paste0("best_idx = ", best_idx, ", best view = ", best_view, ", best_val = ", best_val))
			finished = ((length(self$curr_set) > 1) && ((best_val < (curr_best_val - epsilon)) || (length(self$remainder) == 1)))
			print(paste0("Finished = ", finished))
			if (best_val >= (curr_best_val - epsilon) || length(self$curr_set) == 1) {
				curr_best_idx  = best_idx
				curr_best_view = best_view
				curr_best_val  = best_val
				self$curr_set = append(self$curr_set, curr_best_view)
			}
		}
		return(self$curr_set)
	},
	
		
	##########################################################################################
	# Train a multi-modal (multi-view) incremental model in reverse
	# Remove the worst performing view on each iteration until performance drops
	#
	train_incremental_reverse = function(config, tasks, metric, exp_type, decision, result_file) {
		print(paste0("In train_incremental_reverse: ", metric))
		finished = FALSE
		inc_results = data.frame("Modality" = character(), "Performance" = numeric())
#		print(self$perf)
#		perf = as.data.frame(do.call(cbind, self$perf))
#		print(perf)
		
		# Change this bit to calc avg perf for each task
		avg_perf = sapply(self$perf, function(x) {rowMeans(x[, grepl(metric, colnames(x))])}) # Gives avg over classes
		avg_perf = colMeans(avg_perf) 	# Gives average over folds
		print("Average Performance:")
		print(avg_perf)
		curr_best_perf = 0
		
		self$curr_set  = as.list(names(tasks)[1:length(tasks)])  # Start with full set of views
		self$curr_perf = list()
		print("Initial set:")
		print(unlist(self$curr_set))
		
		while (!finished) {
			print("Start of loop")
			# Get performance stats for each of the remaining views removed from the current set
			for (view_idx in 1:length(self$curr_set)) {
#				print(paste0("Removing view: ", view_idx))
				curr_views = lapply(self$curr_set, function(i) i)  # Create a copy at a different location so we don't disturn the original
				curr_views[[view_idx]] = NULL
#				print("Current views:")
#				print(unlist(curr_views))
				resp = self$get_response(self$predns, unlist(curr_views), exp_type, decision, config)
				perf = self$calc_stats(resp$truth, resp$response, self$classes)
				curr_perf = rowMeans(perf[, grepl(metric, colnames(perf))])
#				print("Current performance:")
#				print(unlist(curr_perf))
				self$curr_perf[[view_idx]] = curr_perf
				names(self$curr_perf)[[view_idx]] = self$curr_set[[view_idx]]
			}
			
			# Choose the next modality to remove - i.e. the one with best performance as that is the performance without that  modality
#			print("Performance with each modality removed:")
#			print(unlist(self$curr_perf))
			best_idx = which.max(unlist(self$curr_perf))
			best_view = names(self$curr_perf)[best_idx] 
			best_perf  = self$curr_perf[[best_idx]]
			print(paste0("best_idx = ", best_idx, ", best_view = ", best_view, ", best_perf = ", best_perf))
			inc_results[nrow(inc_results) + 1, ] = c(best_view, best_perf)
			finished = ((length(self$curr_set) == 2) || (best_perf < (curr_best_perf - epsilon)))
#			print(paste0("Finished = ", finished))
			if (best_perf >= (curr_best_perf - epsilon) || length(self$curr_set) == 1) {
				curr_best_idx  = best_idx
				curr_best_view = best_view
				curr_best_perf  = best_perf
				self$curr_set[curr_best_idx] = NULL
				print(paste0("curr_best_idx = ", curr_best_idx, ", curr_best_view = ", curr_best_view, ", curr_best_perf = ", curr_best_perf))
			}
			
			# Update the current set to remove the next worst performer
		  self$curr_perf[[curr_best_idx]] = NULL		# Remove curr_best_view from current set
#			if (finished) {
				self$feats$write(result_file, paste(self$curr_set, collapse = "_"), self$curr_set)
#			}
		}
		return(list("set" = self$curr_set, "results" = inc_results))
	}
))
