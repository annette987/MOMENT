#' R6 Class representing an incremental model to determine
#' an optimal subset of modalities for model building.
#'
#' @description
#' Forward/backward sequential building of a sequence of views.
#' Only works for classification.
#'
#' @name MM_Incremental
#' @docType package
NULL

epsilon = 0.01

MM_Incremental = R6::R6Class("MM_Incremental", 
	inherit = MM_Model,
	public = list(
		perf_metric = "F1", # Metric for deciding on best performance - F1 Score, Accuracy or AUC
		curr_set    = list(),	# List of names of modalities in current set
		remainder   = list(),	# List of modalities left to choose from
		curr_perf   = list(),	# List of performance scores for each modality added to the current set
	
		#' @description 
		#' Create a new MM_Incremental object.
		#' @inheritParams MM_Model$initialize
		#' @param metric Metric for deciding on best performance - 'F1', 'Acc' or 'AUC' (character)
		#' @param decision Aggregation method - 'vote' (majority vote) or 'prob' (soft vote)
		#' @return A new `MM_Incremental`object.
		#' @export
		initialize = function(config, metric = "F1", decision = "prob", subset = NULL, balance = FALSE, validate = FALSE, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
			super$initialize(config, "INC", decision, subset, FALSE, balance, validate, filter_zeroes, filter_missings, filter_corr, filter_var)
			self$perf_metric = metric
		},
		
		
		#' @description 
		#' Calculate the performance measures for the current set of model responses.
		#' @param truth (factor)\cr
		#' The ground truth labels
		#' @param response (factor)\cr
		#' The model responses.
		#' @param class_names (factor)\cr
		#' The names of the classes in the target variable.
		#' @return A data.frame containing the performance metrics.
		#' @noRd
		calc_stats = function(truth, response, class_names) {
			response = factor(response, levels = levels(truth))  # In case some classes were not predicted
			measures = list()
			neg_class = "ALL"
			tt = table(response)

			for (j in 1:length(class_names)) {
				pos_class = class_names[[j]]
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
			return(as.data.frame(measures))
		},
		

		#' @description 
		#' Train a model on each view for each fold of data. 
		#' These models will be reused when building the incremental model.
		#' @param ri (ResampleInstance)\cr
		#' The resampling instance for the cross validation used in building the model.
		#' @return A list containing the predictions and the performance of the models.
		#' @noRd
		train_base_models = function(ri) {
			predn_futures = list()
			
			for (i in 1:length(self$tasks)) {
				task_id = names(self$tasks)[[i]]
				predn_futures[[task_id]] = list()
				
				for (rep in 1:self$ri$desc$reps) {
					for (fold in 1:ri$desc$folds) {
						subset_idx = (rep - 1) * self$ri$desc$folds + fold
						train_subset = self$ri$train.inds[[subset_idx]]
						test_subset = self$ri$test.inds[[subset_idx]]
					
						predn_futures[[task_id]][[subset_idx]] = future::future( {
							mod = mlr::train(self$learners[[i]], self$tasks[[i]], subset = train_subset)
							if (mlr::isFailureModel(mod)) {
								warnimg(paste0("Model ", i, " failed: ", mlr::getFailureModelMsg(mod)))
							} else {
								mlr::predictLearner(mod, task = self$tasks[[i]], subset = test_subset)
							}
						})
					}
				}
			}
									
			# Wait for results
			future::resolve(predn_futures)
			predns = list()
			perf = list()
			for (i in 1:length(self$tasks)) {
				task_id = names(self$tasks)[[i]]
				predns[[task_id]] = list()
				perf[[task_id]] = list()
				
				for (rep in 1:ri$desc$reps) {
					fold_predns = list()
					fold_perf = list()
					
					for (fold in 1:ri$desc$folds) {
						subset_idx = (rep - 1) * ri$desc$folds + fold
						res = future::value(predn_futures[[task_id]][[subset_idx]])
						fold_predns[[fold]] = res$data
						fold_perf[[fold]] = self$calc_stats(res$data$truth, res$data$response, self$classes)
					}
					
					predns[[task_id]][[rep]] = dplyr::bind_rows(fold_predns)
					predns[[task_id]][[rep]] = predns[[task_id]][[rep]] %>% arrange(id)
					predns[[task_id]][[rep]][, "rpt"] = rep
					perf[[task_id]][[rep]] = dplyr::bind_rows(fold_perf)
					perf[[task_id]][[rep]][, "rpt"] = rep
				}

				predns[[task_id]] = dplyr::bind_rows(predns[[task_id]])
				perf[[task_id]]   = dplyr::bind_rows(perf[[task_id]])
			}
			return(list("predns" = predns, "perf" = perf))
		},

		
		#' @description 
		#' Calculate the final response, according to the decision type from the given set of views. 
		#' @param results (data.frame)\cr
		#' The results from each modality concatenated into a data.frame.
		#' @param views (list)\cr
		#' A list of view to be combined to obtain a final decision. (Indices or view names???)		
		#' @param decision_type (character)\cr
		#' The method of combining the results from each modality - a hard or soft vote.
		#' @return A list containing the predictions and the performance of the models.
		#' @noRd
		get_response = function(results, views, decision_type) {
			if (length(views) <= 0) {
				stop("No views to get response from")
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
					res[[views[[i]]]] = results[[views[[i]]]][, grepl('prob', colnames(results[[views[[i]]]]))]
				}
				curr_resp = as.data.frame(res)

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
			return(curr_resp)
		},
		
		
		#' @description
		#' Train a multi-modal incremental model in a forward manner.
		#' On each iteration, the best performing view is added until performance drops.
    #' @return list
		#' @export
		train_incremental_forward = function() {
			finished = FALSE			
			train_results = self$train_base_models(self$ri, self$decision)
			
			# Change this bit to calc avg perf for each task
			avg_perf = sapply(train_results$perf, function(x) {rowMeans(x[, grepl(self$perf_metric, colnames(x))])}) # Gives avg over classes
			avg_perf = colMeans(avg_perf) 	# Gives average over folds
			curr_best_perf = 0
			
			curr_best_idx = which.max(avg_perf)
			curr_best_view = names(self$tasks)[curr_best_idx] 
			curr_best_val  = avg_perf[curr_best_idx]
			self$curr_set  = list(curr_best_view)  # Add curr_best_view to current set of views
			self$remainder  = as.list(names(self$tasks))
			
			while (!finished) {
				# Update the current set to include the next best performer
				self$curr_perf = list()
				self$remainder[[curr_best_idx]] = NULL		# Remove curr_best_view from remainder

				# Get performance stats for each of the remaining views added to the current set
				for (view in self$remainder) {
					curr_views = append(self$curr_set, view)
					resp = self$get_response(train_results$predns, unlist(curr_views), self$decision)
					perf = self$calc_stats(resp$truth, resp$response, self$classes)
					avg_perf = rowMeans(perf[, grepl(self$perf_metric, colnames(perf))])
					self$curr_perf[[view]] = avg_perf
				}
				
				# Choose the next best performer to add
				best_idx = which.max(unlist(self$curr_perf))
				best_view = names(self$curr_perf)[best_idx] 
				best_val  = self$curr_perf[[best_idx]]
				finished = ((length(self$curr_set) > 1) && ((best_val < (curr_best_val - epsilon)) || (length(self$remainder) == 1)))
				if (best_val >= (curr_best_val - epsilon) || length(self$curr_set) == 1) {
					curr_best_idx  = best_idx
					curr_best_view = best_view
					curr_best_val  = best_val
					self$curr_set = append(self$curr_set, curr_best_view)
				}
			}
			return(self$curr_set)
		},
		
			
		#' @description
		#' Train a multi-modal incremental model in a forward manner.
		#' On each iteration, the worst performing view is removed until performance drops.
    #' @return list
		#' @export
		train_incremental_reverse = function() {
			finished = FALSE
			train_results = self$train_base_models(self$ri, self$decision)
			inc_results = data.frame("Modality" = character(), "Performance" = numeric())
			
			# Change this bit to calc avg perf for each task
			avg_perf = sapply(train_results$perf, function(x) {rowMeans(x[, grepl(self$perf_metric, colnames(x))])}) # Gives avg over classes
			avg_perf = colMeans(avg_perf) 	# Gives average over folds
			curr_best_perf = 0
			
			self$curr_set  = as.list(names(self$tasks)[1:length(self$tasks)])  # Start with full set of views
			self$curr_perf = list()
			
			while (!finished) {
				# Get performance stats for each of the remaining views removed from the current set
				for (view_idx in 1:length(self$curr_set)) {
					curr_views = lapply(self$curr_set, function(i) i)  # Create a copy at a different location so we don't disturn the original
					curr_views[[view_idx]] = NULL
					resp = self$get_response(train_results$predns, unlist(curr_views), self$decision)
					perf = self$calc_stats(resp$truth, resp$response, self$classes)
					curr_perf = rowMeans(perf[, grepl(self$perf_metric, colnames(perf))])
					self$curr_perf[[view_idx]] = curr_perf
					names(self$curr_perf)[[view_idx]] = self$curr_set[[view_idx]]
				}
				
				# Choose the next modality to remove - i.e. the one with best performance as that is the performance without that  modality
				best_idx = which.max(unlist(self$curr_perf))
				best_view = names(self$curr_perf)[best_idx] 
				best_perf  = self$curr_perf[[best_idx]]
				inc_results[nrow(inc_results) + 1, ] = c(best_view, best_perf)
				finished = ((length(self$curr_set) == 2) || (best_perf < (curr_best_perf - epsilon)))
				if (best_perf >= (curr_best_perf - epsilon) || length(self$curr_set) == 1) {
					curr_best_idx  = best_idx
					curr_best_view = best_view
					curr_best_perf  = best_perf
					self$curr_set[curr_best_idx] = NULL
				}
				
				# Update the current set to remove the next worst performer
				self$curr_perf[[curr_best_idx]] = NULL		# Remove curr_best_view from current set

			}
			return(list("set" = self$curr_set, "results" = inc_results))
		}
	)
)
