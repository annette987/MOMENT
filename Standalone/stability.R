#--------------------------------------------------------
# CLASS STABILITY
# Stores and calculates stability metrics
#--------------------------------------------------------

library(R6)

# Stability object.
# Result of a stability experiment with the following members:
#		stab - the stability values

Stability = R6Class("Stability", list(
	stab = NULL,
	class_names = NULL,
	feature_sets = list(),
	
	initialize = function(classes) {
#		print("Initialising stability")
		self$class_names = c(classes, "All")
		stopifnot(is.character(self$class_names), length(self$class_names) >= 1)
		self$stab = data.frame(matrix(ncol = 0,
																	nrow = 5, 
																	dimnames = list(c("Jaccard", "Dice", "Kuncheva", "Lustgarten", "Consistency"))))
	},
	
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

	calc_dice = function(set1, set2)
	{
		return(2 * length(intersect(set1, set2)) / (length(set1) + length(set2)))
	},

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

	# Relative weighted consistency index - Song et al 2019, Petr Somol and Jana Novovicova 2010
	calc_consistency = function(sets, num_sets, num_feats)
	{
#		print(paste0("In calc_consistency, num_feats = ", num_feats, ", num_sets = ", num_sets))
		N =  sum(lengths(sets))
		D = N %% num_feats
		H = N %% num_sets
#		print(paste0("N = ", N, ", D = ", D, ", H = ", H))
		counts <- table(unlist(lapply(sets, unique)))
		prods = lapply(counts, function(x) x * (x-1))
		sum_prods = sum(unlist(prods))
#		print("counts = ")
#		print(counts)
#		print("prods = ")
#		print(prods)
#		print(paste0("sum_prods = ", sum_prods))
		numerator = num_feats * (N - D + sum_prods) - N^2 + D^2
		denominator = num_feats * (H^2 + num_sets * (N - H) - D) - N^2 + D^2
#		print(paste0("numerator = ", numerator, ", denominator = ", denominator))
		rwci = ifelse(denominator == 0, 1, numerator / denominator)
		return(rwci)
	},

	# Use the first num_sets sets in the calculation.
	# That way we can run it once with a large number of repeats and 
	# do the calculation multiple times on different numbers of runs.
	#
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

	save_features = function(fset) {
		self$feature_sets[[length(self$feature_sets) + 1]] = fset
	},

	isNested = function(lst) {
		if (!is.list(lst))
				return(FALSE)
		return(any(unlist( lapply(lst,is.list) )))
	},
	
	save = function(method, sets, num_feats) {
		if (self$isNested(sets)) {
			for (cls in self$class_names) {
				self$stab[[paste0(method, ".", cls)]] = self$calculate(sets[[cls]], length(sets[[cls]]), num_feats)
			}
		} else {
			self$stab[[paste0(method, '-All')]] = self$calculate(sets, length(sets), num_feats)
		}
	},
	
	# Calculate stability value from a list of data.frames, one per task or view
	# Each data.frame column is one set of features
	# Should really call this save_from_df
	save_all = function(method, feat_df_list) {
#		print("In stability$save_all")
		df_all = bind_rows(feat_df_list)
		df_all$Count = rowSums(df_all != 0, na.rm = TRUE)
		df_all[df_all$Count < (ncol(df_all) * 0.8),] = 0
		feat_sets = apply(df_all, 2, function(x) {as.list(row.names(df_all)[x > 0])})
#		print(feat_sets)
#		print(length(feat_sets))
#		print(nrow(df_all))
		self$stab[[method]] = self$calculate(feat_sets, length(feat_sets), nrow(df_all))
	},	
		
	save_df = function(method, features_df) {
		feat_list = as.list(features_df[, grepl(method, colnames(features_df))])
		self$stab[[method]] = self$calculate(feat_list, length(feat_list), nrow(features_df))
	},
		
	write = function(result_file, suffix = "") {
		if (suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		write.csv(self$stab, paste0(result_file, "_stab.csv"), row.names=TRUE)
	})
)

