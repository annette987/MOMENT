#' @title R6 class to hold feature selections
#'
#' @description
#' Stores feature selections and gives access to feature importance scores.
#'
#' @name Features
NULL

Features = R6::R6Class("Features", list(
	#' @field featsel (list)\cr
	#' Record of the features selected by each model in each fold of cross-validation.
	#' Each element of the list is a data.frame, containing the raw feature selections for one modality.
	#' The data.frame contains one row per feature and one column per cross-validation fold. 
	featsel = list(),
	
	#' @field featsel_aggr (list)\cr
	#' Record of features selected, aggregated across all folds of cross-validation.
	#' Each element of the list is a data.frame, containing the aggregated feature selections for one modality.
	#' The data.frame contains one row per feature and a single column.
	featsel_aggr = list(),

	#' @description 
	#' Create a new `Features` object.
	#' Extracts the features names from the tasks for each modality.
	#' @param tasks (list)\cr
	#' List of tasks (datasets) for the multi-modal model - one per modality
	#' @return A new `Features` object
	#' @export
	initialize = function(tasks) {
		for (i in 1:length(tasks)) {
			all_feats = mlr::getTaskFeatureNames(tasks[[i]])
			print(all_feats)
			self$featsel[[names(tasks)[[i]]]]      = data.frame(row.names = all_feats)
			self$featsel_aggr[[names(tasks)[[i]]]] = data.frame(row.names = all_feats)
		}
	},	


	#' @description 
	#' Retrieve and save the feature importance scores from one mlr model.
	#' @param scores (numeric)
	#' 	A vector of feature importance scores
	#' @param mod (object)\cr
	#' 	The mlr model
	#' @param task (Task)\cr
	#' 	The task for which the scores were calculated.
	#' @param method (character)\cr
	#' 	The name of the method used ???
	#' @param fold_num (integer)\cr
	#' 	The index of the fold for which the scores were calculated.
	#' @return Nothing
	#' @export	
	save = function(scores, mod, task, method, fold_num)
	{
			print("Saving features")
			print(scores)
			col_name = paste0(method, "-", fold_num)
			print(str(getLearnerModel(mod, more.unwrap = FALSE)), max.level = 1)
			selected = mlr::getFilteredFeatures(getLearnerModel(mod, more.unwrap = FALSE))
			print(selected)
			not_selected = setdiff(getTaskFeatureNames(task), selected)
			print(not_selected)
			feat_scores = scores[, "all"]
			names(feat_scores) = rownames(scores)
			print(feat_scores)
			if (length(not_selected) > 0) {
				feat_scores[not_selected] = 0
				names(feat_scores[not_selected]) = not_selected
			}
			print(feat_scores)
			self$featsel[[mlr::getTaskId(task)]][, col_name] = feat_scores
	},


	#' @description 
	#' Save a set of raw feature importance scores from a single multi-class task for one cross-validation fold.
	#' @param method (character)\cr
	#'	The name of the method used ???
	#' @param results (data.frame)\cr
	#' 	A data.frame containing feature importance score, one column per class, one row per feature.
	#' @param task_id (character)\cr
	#' 	The name of the task for which the scores were calculated.
	#' @param fold_num (integer)\cr
	#' 	The index of the fold for which the scores were calculated.
	#' @param seln_col (character)\cr
	#' 	The name of the column in the results data.frame containing the scores for the model as a whole
	#' @return A data.frame containing feature importance scores
	#' @export	
	save_multiclass = function(method, results, task_id, fold_num, seln_col = 'all') {
		results = results[order(row.names(results)), seln_col, drop = FALSE]
		colnames(results) = c(paste0(method, "-", fold_num))
		self$featsel[[task_id]] = transform(merge(self$featsel[[task_id]], results, by = 'row.names', all = TRUE), row.names = Row.names, Row.names = NULL)
	},
		

	#' @description 
	#' The scores are  normalised across all modalities so that they can be compared.
	#' Only features apearing in at least 75% of cross-vaidated runs are given a score.
	#' The scores are also aggregated by averaging across all cross-validation runs. 
	#' The scores from the individual cross-validation runs are saved to a file ending in '_featsel.csv'.
	#' The aggregated scores are saved to a file ending in '_featsel_aggr.csv'
	#' @return Nothing
	#' @export	
	complete = function() {
		# Aggregate and normalise
		if (length(self$featsel) > 1) {
			for (tsk in 1:length(self$featsel)) {
				if (ncol(self$featsel[[tsk]]) > 0) {
					# Normalise the values so that they can be compared across datasets
					if (!all(self$featsel[[tsk]] == 0)) {
						self$featsel[[tsk]] = as.data.frame(apply(self$featsel[[tsk]], 2, norm_minMax))
						self$featsel[[tsk]][self$featsel[[tsk]] < 0.00001] = 0
					}
					
					# Calculate aggregate scores across folds
					self$featsel_aggr[[tsk]][1:nrow(self$featsel[[tsk]]), 1] = rowMeans(self$featsel[[tsk]], na.rm = TRUE)
					self$featsel_aggr[[tsk]][self$featsel_aggr[[tsk]] < 0.00001] = 0
				}
			}
		} else {
				self$featsel_aggr[[1]][1:nrow(self$featsel[[1]]), 1] = rowMeans(self$featsel[[1]], na.rm = TRUE)
				self$featsel_aggr[[1]][self$featsel_aggr[[1]] < 0.00001] = 0
		}
	},
	
	
	#' @description 
	#' Return the selected features - for each fold and an aggregated result.
	#' @param perc (numeric)\cr
	#' 	Only features which are selected in this percentage of folds or greater are returned.
	#'  To retrieve all selected features, pass 1??
	#' @return A list containing 2 data.frames
	#' @export	
	get_results = function(perc = 1.0) {
			self$complete()
			out_aggr  = as.data.frame(do.call(rbind, self$featsel_aggr))
			out_feats = as.data.frame(do.call(rbind, self$featsel))
			out_feats$Avg = rowMeans(out_feats != 0, na.rm = TRUE)
			out_feats$Count = rowSums(out_feats != 0, na.rm = TRUE) - 1  # -1 so as not to include the Avg column
			out_aggr[out_feats$Count < (ncol(out_feats) * perc),] = 0
			return(list("aggr" = out_aggr, "feats" = out_feats))
	},
	
	#' @description 
	#' Write the selected features to disk.
	#' @param result_file (character)\cr
	#' 	The prefix of the name of the files to which the results will be written.
	#' @param suffix (character)\cr
	#' 	The suffix will be appended to the prefix of the output file name.
	#' @param perc (numeric)\cr
	#' 	Only features which are selected in this percentage of folds or greater are returned.
	#'  To retrieve all selected features, pass 1??
	#' @return Nothing
	#' @export	
	write = function(result_file, suffix = "", perc = 0.75) {
		# Only output features selected in perc % of folds
		out_aggr  = as.data.frame(do.call(rbind, self$featsel_aggr))
		out_feats = as.data.frame(do.call(rbind, self$featsel))
		out_feats$Avg = rowMeans(out_feats != 0, na.rm = TRUE)
		out_feats$Count = rowSums(out_feats != 0, na.rm = TRUE) - 1  # -1 so as not to include the Avg column
		out_aggr[out_feats$Count < (ncol(out_feats) * perc),] = 0

		if (!is.null(suffix) && suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		#feats$featsel[["Total"]] = apply(feats$featsel, 1, function(x) sum(x, na.rm=TRUE))
		write.csv(out_feats, paste0(result_file, "_featsel", "_", suffix, ".csv"), row.names=TRUE)
		write.csv(out_aggr, paste0(result_file, "_featsel_aggr", "_", suffix, ".csv"), row.names=TRUE)
	})	
)
		
