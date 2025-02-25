#' @title R6 class to hold the stability results
#'
#' @description
#' Calculates and stores stability results for each stability metric.
#'
#' @details
#' Five stability metrics are available: the Jaccard Index, the Dice Score,
#' Kuncheva's Index, Lustgarten's Index and the Relative Weighted Consistency Index.
#'
#' @references Relative weighted consistency index - Song et al 2019, Petr Somol and Jana Novovicova 2010
#'
#' @name Stability
NULL

Stability = R6::R6Class("Stability", list(
	#' @field stab (data.frame)\cr
	#' A data.frame containing one row per performance metric requested.
	stab = NULL,
	
	#' @field class_names (factor)\cr
	#' The names of the classes in the multi-class data.
	class_names = NULL,
	
	#' @field feature_sets (list)\cr
	#' The sets of features on which stability is calculated.
	feature_sets = list(),
	
	#' @description 
	#' Create a new `Stability` object.
	#' @param classes (list)\cr
	#' The names of the classes in the multi-class data.
	#' @return A new `Stability` object
	#' @examples
	#' stab = Stability$new(c("C1", "C2", "C3"))
	#' @export
	initialize = function(classes) {
		self$class_names = c(classes, "All")
		stopifnot(is.character(self$class_names), length(self$class_names) >= 1)
		self$stab = data.frame(matrix(ncol = 0,
																	nrow = 5, 
																	dimnames = list(c("Jaccard", "Dice", "Kuncheva", "Lustgarten", "Consistency"))))
	},

	
	#' @description 
	#' Calculate the value of the Jaccard Index for two sets of features.
	#' @param set1 (character vector)\cr
	#' 	The first set of features.
	#' @param set2 (character vector)\cr
	#' 	The second set of features.
	#' @return The value of the Jaccard Index
	#' @examples
	#'  set1 = c('cat', 'dog', 'mouse')
	#'  set2 = c('cat', 'mouse', 'elephant')
	#' 	stab = Stability$calc_jaccard(set1, set2)
	#' @noRd
	calc_jaccard = function(set1, set2)
	{
		jaccard = 0
		numerator = length(intersect(set1, set2))
		denominator = length(union(set1, set2))
		if (denominator != 0) {
			jaccard = numerator / denominator
		}
		return(jaccard)
	},


	#' @description 
	#' Calculate the value of the Dice Score for two sets of features.
	#' @param set1 (character vector)\cr
	#' 	The first set of features.
	#' @param set2 (character vector)\cr
	#' 	The second set of features.
	#' @return The value of the Dice Score
	#' @examples
	#'  set1 = c('cat', 'dog', 'mouse')
	#'  set2 = c('cat', 'mouse', 'elephant')
	#' 	stab = Stability$calc_dice(set1, set2)
	#' @noRd
	calc_dice = function(set1, set2)
	{
		return(2 * length(intersect(set1, set2)) / (length(set1) + length(set2)))
	},


	#' @description 
	#' Calculate the value of Kuncheva's Index for two sets of features.
	#' @param set1 (character vector)\cr
	#' 	The first set of features.
	#' @param set2 (character vector)\cr
	#' 	The second set of features.
	#' @param total (integer)\cr
	#' 	Total number of features available.
	#' @return The value of Kuncheva's Index
	#' @examples
	#'  set1 = c('cat', 'dog', 'mouse')
	#'  set2 = c('cat', 'mouse', 'elephant')
	#' 	stab = Stability$calc_kuncheva(set1, set2, 4)
	#' @noRd
	calc_kuncheva = function(set1, set2, total)
	{
		kuncheva = 0
		r = length(intersect(set1, set2))
		k = min(length(set1), length(set2))
		numerator = (r * total) - (k * k)
		denominator = k * (total - k)
		if (denominator != 0) {
			kuncheva = numerator / denominator
		}
		return(kuncheva)
	},

		
	#' @description 
	#' Calculate the value of Lustgarten's Index for two sets of features.
	#' @param set1 (character vector)\cr
	#' 	The first set of features.
	#' @param set2 (character vector)\cr
	#' 	The second set of features.
	#' @param total (integer)\cr
	#' 	Total number of features available.
	#' @return The value of Lustgarten's Index
	#' @examples
	#'  set1 = c('cat', 'dog', 'mouse')
	#'  set2 = c('cat', 'mouse', 'elephant')
	#' 	stab = Stability$calc_lustgarten(set1, set2, 4)
	#' @noRd
	calc_lustgarten = function(set1, set2, total)
	{
		lustgarten = 0
		numerator = length(intersect(set1, set2)) - (length(set1) * length(set2) / total)
		denominator = min(length(set1), length(set2)) - max(0, length(set1) + length(set2) - total)
		if (denominator != 0) {
			lustgarten = numerator / denominator
		}
		return(lustgarten)
	},

		
	#' @description 
	#' Calculate the value of the Relative Weighted Consistency Index for two sets of features.
	#' @param sets (list???)\cr
	#' 	The sets of features.
	#' @param num_sets (integer)\cr
	#' 	The total number of sets.
	#' @param num_feats (integer)\cr
	#' 	The total number of features available.
	#' @return The value of the Relative Weighted Consistency Index
	#' @examples
	#'  set1 = c('cat', 'dog', 'mouse')
	#'  set2 = c('cat', 'mouse', 'elephant')
	#' 	stab = Stability$calc_consistency(list(set1, set2), 2, 4)
	#' @noRd
	calc_consistency = function(sets, num_sets, num_feats)
	{
		N =  sum(lengths(sets))
		D = N %% num_feats
		H = N %% num_sets
		counts <- table(unlist(lapply(sets, unique)))
		prods = lapply(counts, function(x) x * (x-1))
		sum_prods = sum(unlist(prods))
		numerator = num_feats * (N - D + sum_prods) - N^2 + D^2
		denominator = num_feats * (H^2 + num_sets * (N - H) - D) - N^2 + D^2
		rwci = ifelse(denominator == 0, 1, numerator / denominator)
		return(rwci)
	},


	#' @description 
	#' Calculate the value of all stability metrics.
	#' The first num_sets sets are used in the calculation
	#' @param sets (list)\cr
	#' 	The sets of features.
	#' @param num_sets (integer)\cr
	#' 	The total number of sets.
	#' @param num_feats (integer)\cr
	#' 	The total number of features available.
	#' @return The value of the 4 stability metrics, as a vector.
	#' @examples
	#'  set1 = c('cat', 'dog', 'mouse')
	#'  set2 = c('cat', 'mouse', 'elephant')
	#' 	stab = Stability$calculate(list(set1, set2), 2, 4)
	#' @export
	calculate = function(sets, num_sets, num_feats)
	{
		jaccard = 0
		dice = 0
		kuncheva = 0
		lustgarten = 0
		consistency = 0
		if (num_sets > 1) {
			for (i in 1:(num_sets - 1)) {
				for (j in (i+1):num_sets) {
					jaccard = jaccard + self$calc_jaccard(sets[[i]], sets[[j]])
					dice = dice + self$calc_dice(sets[[i]], sets[[j]])
					kuncheva = kuncheva + self$calc_kuncheva(sets[[i]], sets[[j]], num_feats)
					lustgarten = lustgarten + self$calc_lustgarten(sets[[i]], sets[[j]], num_feats)
				}
			}
			jaccard = jaccard * 2 / (num_sets * (num_sets - 1))
			dice = dice * 2 / (num_sets * (num_sets - 1))
			kuncheva = kuncheva * 2 / (num_sets * (num_sets - 1))
			lustgarten = lustgarten * 2 / (num_sets * (num_sets - 1))
			consistency = self$calc_consistency(sets, length(sets), num_feats)
		}
		return( c(jaccard, dice, kuncheva, lustgarten, consistency) )
	},


	#' @description 
	#' Save one set of features, selected from one fold of the cross-validated model.
	#' @param fset (character)\cr
	#' 	A character vector containing the set of features.
  #' @return Nothing
	#' @examples
	#'  featset = c('cat', 'dog', 'mouse')
	#' 	Stability$save_features(featset)
	#' @export
	save_features = function(fset) {
		self$feature_sets[[length(self$feature_sets) + 1]] = fset
	},


	#' @description 
	#' Determine if a list is a nested list.
	#' @param lst (list)\cr
	#' 	The list to test.
  #' @return logical
	#' @export
	isNested = function(lst) {
		if (!is.list(lst))
				return(FALSE)
		return(any(unlist( lapply(lst,is.list) )))
	},


	#' @description 
	#' Save all of the features sets from all folds of the cross-validation.
	#' @param method (character)\cr
	#'  A character string that forms part of the column name in the output. Typically the model type.
	#' @param sets (list)\cr
	#' 	The sets of features.
	#' @param num_sets (integer)\cr
	#' 	The total number of sets.
	#' @param num_feats (integer)\cr
	#' 	The total number of features available.
  #' @return Nothing
	#' @export	
	save = function(method, sets, num_feats) {
		if (self$isNested(sets)) {
			for (cls in self$class_names) {
				self$stab[[paste0(method, ".", cls)]] = self$calculate(sets[[cls]], length(sets[[cls]]), num_feats)
			}
		} else {
			self$stab[[paste0(method, '-All')]] = self$calculate(sets, length(sets), num_feats)
		}
	},


	#' @description 
	#' Calculate the stability  from a list of data.frames, one per task or view
	#' Each data.frame column is one set of features
	#' @param method (character)\cr
	#'  A character string that forms part of the column name in the output data.frame. Typically the model used.
	#' @param feat_df_list (list)\cr
	#' 	The sets of features.
  #' @return Nothing
	#' @export	
	save_all = function(method, feat_df_list) {
		df_all = dplyr::bind_rows(feat_df_list)
		df_all$Count = rowSums(df_all != 0, na.rm = TRUE)
		df_all[df_all$Count < (ncol(df_all) * 0.8),] = 0
		feat_sets = apply(df_all, 2, function(x) {as.list(row.names(df_all)[x > 0])})
		self$stab[[method]] = self$calculate(feat_sets, length(feat_sets), nrow(df_all))
	},	
		
	#' @description 
	#' Calculate stability value from a list of data.frames, one per task or view
	#' Each data.frame column is one set of features
	#' @param method (character)\cr
	#'  A character string that forms part of the column name in the output data.frame. Typically the model used.
	#' @param sets (list)\cr
	#' 	The sets of features.
  #' @return Nothing
	#' @export	
	save_df = function(method, features_df) {
		feat_list = as.list(features_df[, grepl(method, colnames(features_df))])
		self$stab[[method]] = self$calculate(feat_list, length(feat_list), nrow(features_df))
	},

	#' @description 
	#' Return the stability results.
	#' @return A data.frame containing the stability results.
	#' @export
	get_results = function() {
		return(self$stab)
	},
			
	write = function(result_file, suffix = "") {
		if (!is.null(suffix) && suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		write.csv(self$stab, paste0(result_file, "_stab.csv"), row.names=TRUE)
	})
)

