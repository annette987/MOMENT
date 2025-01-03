#--------------------------------------------------------
# CLASS FEATURES
# Stores feature selections
#--------------------------------------------------------

library(R6)
library(testthat)

# Multi-modal Features object.
# Stores the features selected by a muti-modal method - one set per task. 

MM_Features = R6Class("Features", list(
	featsel = list(),
	featsel_aggr = list(),
	
	initialize = function(tasks) {
		for (i in 1:length(tasks)) {
			all_feats = getTaskFeatureNames(tasks[[i]])
			self$featsel[[names(tasks)[[i]]]]      = data.frame(row.names = all_feats)
			self$featsel_aggr[[names(tasks)[[i]]]] = data.frame(row.names = all_feats)
		}
	},
	
	# Save features from one task for one fold
	# Results is a named vector, for one task, giving feature scores
	save = function(method, results, task_id, fold_num) {
		print(paste0("Saving features: ", task_id))
		print(results)
		col_name = paste0(method, "-", fold_num)
		self$featsel[[task_id]][, col_name] = 0
		if (!is.null(results) && length(results) > 0) {
			if ((class(results) == "data.frame") || (class(results) == "Matrix")) {
				res_vec = results[, 'all']
				names(res_vec) = rownames(results)
				results = res_vec
			}
			x = names(results)
			self$featsel[[task_id]][x, col_name] = results[x]
#			print(self$featsel[[task_id]])
		}
	},

	# Save features from a single multi-class task for one fold
	# Results is a data.frame, one column per class, one row per feature, giving feature scores
	save_multiclass = function(method, results, task_id, fold_num, seln_col = 'all') {
		print("Saving multiclass features")
		print(task_id)
		results = results[order(row.names(results)), seln_col, drop = FALSE]
		colnames(results) = c(paste0(method, "-", fold_num))
		self$featsel[[task_id]] = transform(merge(self$featsel[[task_id]], results, by = 'row.names', all = TRUE), row.names = Row.names, Row.names = NULL)
	},
		
	write = function(result_file, suffix = "", subset = NULL) {
		print("Writing out features")
		print(result_file)
		print(suffix)
		print(subset)
		if (!is.null(suffix) && suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		
		if (!is.null(subset)) {
			# Subset should be a comma separated list of numbers, indexes into featsel list - check this properly later
			self$featsel = self$featsel[unlist(subset)]
			self$featsel_aggr = self$featsel_aggr[unlist(subset)]
		}
		
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
		
		# Only output features selected in 75% of folds
		# Save to disk
#		print("Writing aggregated feature selections")
		out_aggr  = as.data.frame(do.call(rbind, self$featsel_aggr))
#		print(dim(out_aggr))
		write.csv(out_aggr, paste0(result_file, "_featsel_aggr.csv"), row.names=TRUE)
		
#		print("Writing raw feature selections")
		out_feats = as.data.frame(do.call(rbind, self$featsel))
		out_feats$Avg = rowMeans(out_feats != 0, na.rm = TRUE)
#		out_feats$Count = rowSums(out_feats != 0, na.rm = TRUE) - 1  # -1 so as not to include the Avg column
#		out_aggr[out_feats$Count < (ncol(out_feats) * 0.75),] = 0
		
		write.csv(out_feats, paste0(result_file, "_featsel.csv"), row.names=TRUE)
	})
)
		
