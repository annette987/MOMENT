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
			self$featsel[[names(tasks)[[i]]]]      = data.frame(row.names = all_feats)
			self$featsel_aggr[[names(tasks)[[i]]]] = data.frame(row.names = all_feats)
		}
	},	

	#' @description 
	#' Extract the feature importance scores from an mlr model.
	#' Convert the custom importance data from each model into a uniform format -
	#' a data.frame with one row per feature and one column per class, 
	#' plus a column labelled 'all', which gives the feature importance for the model as a whole.
	#' The 'all' column may contain zeroes if this data is not available.
	#' @param mod (???)\cr
	#' 	The mlr model
	#' @param class_names (character vector)
	#' 	Names of the classes in the data
	#' @return A data.frame containing feature importance scores
	#' @export
	getFeatImpScores = function(mod, class_names = NULL)
	{	
		imp_data = NULL
		if (is.null(mod)) {
			cat(paste("=======> Model is null!!\n"))		
		} else if (inherits(mod, "WrappedModel") && isFailureModel(mod)) {
			cat(paste("=======> Model is a FailureModel!!!\n"))
			cat(mlr::getFailureModelMsg(mod))		
		} else if (inherits(mod, "coxph")) {
			pvalues = summary(mod)$coefficients[,5]
			imp_data = as.data.frame(pvalues)
			colnames(imp_data) = c('all')
		} else if (inherits(mod, "CoxBoost")) {
			coef.nonzero = CoxBoost::coef(mod)
			imp_data = coef.nonzero
		} else if (inherits(mod, "glmboost")) {
			coef.nonzero = glmboost::coef(mod)
			imp_data  = coef.nonzero[2:length(coef.nonzero)]						# 1st column is Intercept - ignore this
		} else if (inherits(mod, "cv.glmnet")) {
			coef.min = glmnet::coef(mod, s = mod$lambda.min)
			# For binary class problems coef.min is a single dgCMatrix
			# For multi-class problems coef.min is a list of dgCMatrix, one per class
			if (inherits(coef.min, "dgCMatrix")) {
				coef.min = coef.min[!rownames(coef.min) %in% c("(Intercept)"), ]
				imp_data = data.frame('all' = coef.min)
			} else {
				imp_data = as.data.frame(do.call(cbind, lapply(coef.min, as.matrix)))
				imp_data = imp_data[!rownames(imp_data) %in% c("(Intercept)"), ]
				colnames(imp_data) = class_names[1:ncol(imp_data)]
				imp_data['all'] = rep(0, nrow(imp_data))
			}
			
		} else if (inherits(mod, "rfsrc")) {
			imp_data = mod$importance
			colnames(imp_data) = c('all')
		} else if (inherits(mod, "randomForest")) {
			imp_data = mod$importance
			colnames(imp_data) = c('all')
		} else if (inherits(mod, "ranger")) {
			# N.B. This one needs to use local.importance but mlr doesn't allow it!
			ind = which(mod$variable.importance != 0)
			imp_data = as.data.frame(mod$variable.importance)
			rownames(imp_data) =  names(mod$variable.importance)
			imp_data = imp_data[ind, , drop = FALSE]
			colnames(imp_data) = c('all')
		} else if (inherits(mod, "xgb.Booster")) {
			if (mod$params$booster == BOOSTER_LINEAR) {
				imp_col = "Weight"
			} else {
				imp_col = "Gain"
			}
			if (mod$params$booster == BOOSTER_LINEAR) {
				imp = xgboost::xgb.importance(model = mod)
				if ('Class' %in% colnames(imp)) {
					imp_data = as.data.frame(imp %>% 
											tidyr::pivot_wider(names_from = "Class", 
																				 values_from = "Weight", 
																					names_prefix = "Class"))
					colnames(imp_data) = c("Feature", class_names[1:(length(class_names))])
				} else {
					imp_data = as.data.frame(imp)
				}
				
				rownames(imp_data) = imp_data$Feature
				imp_data$Feature = NULL
				imp_data['all'] = 0		# Not provided - no way to calculate
			} else {
				imp = xgboost::xgb.importance(model = mod)
				imp_data = as.data.frame(imp)[, c("Feature", imp_col)]
				colnames(imp_data) = c("Feature", "all")
				rownames(imp_data) = imp_data$Feature
				imp_data$Feature = NULL
			}
		} else if (inherits(mod, "svm")) {
				imp <- t(mod$coefs) %*% mod$SV                   				# weight vectors
				imp <- apply(imp, 2, function(v){sqrt(sum(v^2))})  	# weight
				imp <- sort(imp, decreasing = T)
				imp_data = data.frame("all" = imp)
		} else if (inherits(mod, "gbm")) {
				imp = gbm::summary.gbm(mod, method = gbm::relative.influence)
				colnames(imp) = c("var", "all")
				imp_data = data.frame('all' = imp[, "all"], row.names = imp[, "var"])
		} else if (inherits(mod, "rpart")) {
				imp_data = data.frame('all' = mod$variable.importance)
		} else if (inherits(mod, "naiveBayes")) {
		} else {
			warning(paste0("Unknown model class: ", class(mod)))
		}

		return(imp_data)
	},

	#' @description 
	#' Retrieve and save the feature importance scores from one mlr model.
	#' @param mod (???)\cr
	#' 	The mlr model
	#' @param task (Task)\cr
	#' 	The task for which the scores were calculated.
	#' @param classes (character vector)
	#' 	Names of the classes in the data
	#' @param method (character)\cr
	#' 	The name of the method used ???
	#' @param fold_num (integer)\cr
	#' 	The index of the fold for which the scores were calculated.
	#' @return A data.frame containing feature importance scores
	#' @export	
	save = function(mod, task, classes, method, fold_num)
	{
			scores = self$getFeatImpScores(mlr::getLearnerModel(mod, more.unwrap = TRUE), classes)
			selected = mlr::getFilteredFeatures(mlr::getLearnerModel(mod, more.unwrap = FALSE))
			not_selected = setdiff(mlr::getTaskFeatureNames(task), selected)
			
			feat_scores = scores[, "all"]
			names(feat_scores) = rownames(scores)
			if (length(not_selected) > 0) {
				feat_scores[not_selected] = 0
				names(feat_scores[not_selected]) = not_selected
			}
			
			col_name = paste0(method, "-", fold_num)
			task_id = task$task.desc$id
			self$featsel[[task_id]][, col_name] = feat_scores
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
	#' @return list
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
		
