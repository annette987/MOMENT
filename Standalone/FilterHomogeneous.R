library(mlr)
library(checkmate)
library(testthat)
library(data.table)
#library(ConsRank)
library(RVenn)
library(MASS)
library(RobustRankAggreg)
library(NbClust)
library(votesys)
library(cluster)
library(dendextend)
library(dynamicTreeCut)


source(paste(code_dir, "threshold.R", sep=""))

#dat <- sapply(dat, function(x) if (all(x %in% c(0,1))) {ifelse(x == 1,0,1)} else x)

derangement = function(x) {
#		print("Derangement:")
    cnt = 1
		y = sample(x)
    while (cnt < 10 && any(x == y)) {
#			print(paste0("Attempt ", cnt))
      y = sample(x)
			cnt = cnt + 1
   }
	 return(y)
}


#' Creates and registers custom honogeneous ensemble feature filters. 

# generate homogeneous filter -------------------------------------------------------
# helper function to generate and evaluate the samples for the homogeneous filter
#
# For most filters, the larger the value returned from the filter, the more important the feature.
# Features must be ranked so that the most important features have the largest value of rank.
# This equates to ranking in ascending order - smallest values get smallest rank and largest values get largest rank.
# This is because filterFeatures orders the features in decreasing order of value, and in an ensemble this value is the rank.
# But for RF minimal depth, the smaller the value returned from the filter, the more important the feature.
# So, this means for the RF minimal depth filter, values have to be multipled by -1. This is done in the filter itself
# Same for XGBoost??? What about Lasso??
# NAs are put last so that they get the smallest rank.
#
genHomogeneousFilter = function(task, nselect, resamp, base_method, base_args, permute = FALSE) {
	if (inherits(resamp, "ResampleDesc")) {
		resamp = makeResampleInstance(resamp, task = task)
	}
	assertClass(resamp, classes = "ResampleInstance")

	fv_sets = list()	# List of data.table
	for (i in 1:resamp$desc$iters) {
		t = subsetTask(task = task, subset = resamp$train.inds[[i]])	
		
		# If using probes, shuffle the probes each time
		if (permute) {
#			print("genHomogeneousFilter - shuffling")
			data = getTaskData(t, target.extra = FALSE)
			ind = grep("^shadow_*", colnames(data), value=FALSE)
			shadows = as.data.frame(data[sample(nrow(data)), ind])   # Shadows shuffled row-wise to maintain correlations between them
			data = cbind(data[,-ind], shadows)			
		}
		
		fv = tryCatch({
				generateFilterValuesData(t, method = base_method, nselect = nselect, more.args = base_args)
			}, 
			error = function(cond) {
					print(paste("generateFilterValuesData returned error: ", cond))
					return(NULL)	
		})

		if (!is.null(fv)) {
			fv_sets[[i]] = fv$data[, rank := frankv(fv$data, cols = "value", order = 1L, na.last = FALSE, ties.method = "first")]
			setorderv(fv_sets[[i]], c("rank"), order = -1L, na.last = TRUE)
			idx = is.na(fv_sets[[i]][,'value'])
			fv_sets[[i]][idx[,1], 'rank'] = NA  # Set rank to NA where value = NA

		}
	}
	return(fv_sets)
}


#' Creates and registers custom heterogeneous ensemble feature filters. 

# generate heterogeneous filter -------------------------------------------------------
# helper function to generate and evaluate the samples for the heterogeneous filter
#
genHeterogeneousFilter = function(task, nselect, resamp, base_methods, base_args, permute = FALSE) {
	if (inherits(resamp, "ResampleDesc")) {
		resamp = makeResampleInstance(resamp, task = task)
	}
	assertClass(resamp, classes = "ResampleInstance")
	if (resamp$desc$iters != length(base_methods)) {
		print("Error: need one sample per base method")
		return
	}

	fv_sets = list()	# List of data.table
	for (i in 1:resamp$desc$iters) {
#		print(paste0("i = ", i))
		t = subsetTask(task = task, subset = resamp$train.inds[[i]])
		p = getTaskNFeats(t)
		
		# If using probes, shuffle the probes each time
		if (permute) {
			data = getTaskData(t, target.extra = FALSE)
			ind = grep("^shadow_*", colnames(data), value=FALSE)
#			shadows = as.data.frame(lapply(data[, ind], derangement))
			shadows = as.data.frame(lapply(data[, ind], sample))
			data = cbind(data[,-ind], shadows)
			t = makeSurvTask(data = data, target = getTaskTargetNames(task))
		}
		
		fv = generateFilterValuesData(t, method = base_methods[[i]], nselect = nselect, more.args = base_args[[i]])
		if (!is.null(fv)) {
			fv_sets[[i]] = fv$data[, rank := frankv(fv$data, cols = "value", order = 1L, na.last = "keep", ties.method = "first")]
			setorderv(fv_sets[[i]], c("rank"), order = -1L, na.last = TRUE)
		}
	}
	return(fv_sets)
}


# H-individual ----------------
#' Individual filter with custom auto thresholding.
#' Allows comparison with the homogeneous ensemble filters with auto thresholding.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-Individual",
  desc = "Individual filter with custom thresholding.",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, autothresh, filename, base.method, base.args) {

		base.args[[base.method]][["ensemble"]] = FALSE
		fv = generateFilterValuesData(task, method = base.method, nselect = nselect, more.args = base.args)
		fval.ens = fv$data[, rank := frankv(fv$data, cols = "value", order = 1L, na.last = FALSE, ties.method = "first")]
		fval.ens = as.data.frame(fval.ens[, c("name", "value")])
		setorderv(fval.ens, c("value"), order = -1L, na.last = TRUE)
#		print(fval.ens)
		
		if (all(is.na(fval.ens$value))) {
			fval.ens[fval.ens$name == 'Age', 'value'] = 0.001
		} else {
			#Apply automatic threshold, if requested, to determine the most important features
			factor = 1		
			fval.ens = threshold_auto(fval.ens, fval.ens, "value", autothresh, factor)
			
			# Set value of 0.0 back to NA so that those features are excluded
			# This must be done after auto thresholding as k-means doesn't handle NAs
			fval.ens$value[fval.ens$value == 0] = NA
		}
#		print("sorted aggregated values")
#		print(as.data.frame(fval.ens))

		scores = setNames(fval.ens[,2], fval.ens[,1])
		return(scores)
  }
)


# H-min-rank ----------------
#' Minimum homogeneous ensemble filter. Takes the minimum (best) rank achieved by each feature.
#' for each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-min-rank",
  desc = "Minimum rank homogeneous ensemble filter. Takes the minimum (best) rank achieved by each feature.",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, base.method, base.args) {
    fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args)
	  fval.all = do.call("rbind", fval.all)

		fval.ens = aggregate(fval.all$rank, by = list(fval.all$name), FUN = min, na.rm = TRUE)
		fval.ens[fval.ens == Inf] = NA
    colnames(fval.ens) = c("name", "value")
		setNames(fval.ens[,2], fval.ens[,1])
  }
)

# H-min-value ----------------
#' Minimum value homogeneous ensemble filter. Takes the minimum (worst) value achieved by each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-min-value",
  desc = "Minimum value homogeneous ensemble filter. Takes the minimum (worst) value achieved by each feature.",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, base.method, base.args) {
    fval.all = genHomogeneousValueFilter(task = task, nselect = nselect, resamp = resamp, autothresh = autothresh,
											base_method = base.method, base_args = base.args)
		
		fval.ens = aggregate(fval.all$value, by = list(fval.all$name), FUN = min, na.rm = TRUE)
		fval.ens[fval.ens == Inf] = NA
    colnames(fval.ens) = c("name", "value")
		rank_values(base.method, fval.ens)
  }
)

# H-max-rank ----------------
#' Maximum homogeneous ensemble filter. Takes the maximum (worst) rank achieved by each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-max-rank",
  desc = "Maximum rank homogeneous ensemble filter. Takes the maximum (worst) rank achieved by each feature.",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, base.method, base.args) {
    fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args)
		fval.all = do.call("rbind", fval.all)
	 
		fval.ens = aggregate(fval.all$rank, by = list(fval.all$name), FUN = max, na.rm = TRUE)
		fval.ens[fval.ens == -Inf] = NA
    colnames(fval.ens) = c("name", "value")
		setNames(fval.ens[,2], fval.ens[,1])
  }
)

# H-max-value ----------------
#' Maximum value homogeneous ensemble filter. Takes the maximum (best) value achieved by each feature..
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-max-value",
  desc = "Maximum value homogeneous ensemble filter. Takes the maximum (best) value achieved by each feature..",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, base.method, base.args) {
    fval.all = genHomogeneousValueFilter(task = task, nselect = nselect, resamp = resamp, autothresh = autothresh,
											base_method = base.method, base_args = base.args)
		
		fval.ens = aggregate(fval.all$value, by = list(fval.all$name), FUN = max, na.rm = TRUE)
		fval.ens[fval.ens == -Inf] = NA
    colnames(fval.ens) = c("name", "value")
		rank_values(base.method, fval.ens)
  }
)

# H-mean-rank ----------------
#' Mean homogeneous ensemble filter. Takes the mean of the ranks achieved by each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-mean-rank",
  desc = "Mean homogeneous ensemble filter. Takes the mean of the ranks achieved by each feature.",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, filename, base.method, base.args, hom = TRUE) {

		print("In mean rank")
		nselect = ifelse(nselect <= 1, 2, nselect)	# Glmnet models require at least 2 features
		using_probes = length(grep("^probe_*", autothresh)) > 0
		for (i in 1:length(base.args))
				base.args[[i]][["ensemble"]] = TRUE

		if (hom) {
			fval.all = tryCatch({
										genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
															base_method = base.method, base_args = base.args, permute = using_probes)										
									}, 
									error = function(cond) {
											print(paste("genHomogeneousFilter returned error: ", cond))
											return(NULL)	
									})
		} else {
			resampling = makeResampleDesc("Bootstrap", iters = length(base.method), stratify = TRUE)
			fval.all = genHeterogeneousFilter(task = task, nselect = nselect, resamp = resampling,
												base_methods = base.method, base_args = base.args, permute = using_probes)			
		}
		
		if (is.null(fval.all)) {
				print("Method failed")
				# Return two values to prevent it crashing
				names = getTaskFeatureNames(task)
				vals  = rep(NA, length(names))
				vals[1:2] = c(1000.0, 1000.0)		
				scores = setNames(vals, names) 
		} else {
				fval.all = do.call("rbind", fval.all)
#				print("Before aggregation")
#				print(as.data.frame(fval.all))
				
				# If value = NA, feature wasn't selected, so set rank to zero so that it is included in the mean calculation
				# If this is not done, features only selected infrequently are given an advantage 
				# The other problem was that features not selected were ranked alphabetically, artificially increasing their rank.
				fval.ens = aggregate(replace(fval.all$rank, is.na(fval.all$rank), 0), by = list(fval.all$name), FUN = mean)
				colnames(fval.ens) = c("name", "rank")
#				print("After aggregation")

				#Sort by rank in decreasing order so that largest rank is best, with NA's last
				setorderv(fval.ens, c("rank"), order = -1L, na.last = TRUE)
#				print("aggregated values")
#				print(fval.ens)		
#				all_na = all(fval.ens$rank == 0) || all(is.na(fval.ens$rank))
				
				#Apply automatic threshold, if requested, to determine the most important features
				fval.ens = threshold_auto(fval.ens, fval.all, "rank", autothresh)
		
				# Set rank of 0.0 back to NA so that those features are excluded
				# This must be done after auto thresholding as k-means doesn't handle NAs
				# Also, for sparse selectors get best probe score of -Inf because you can't compare NA ro NA
				fval.ens$rank[fval.ens$rank == 0] = NA
				fval.ens$rank[is.nan(fval.ens$rank)] = NA
#				print(paste0("Number of non-nulls: ", length(na.omit(fval.ens$rank))))
				
				#Sort by rank in decreasing order so that largest rank is best, with NA's last
				setorderv(fval.ens, c("rank"), order = -1L, na.last = TRUE)
				if (nselect < length(na.omit(fval.ens$rank)))
					fval.ens$rank[(nselect+1):length(fval.ens$rank)] = NA
#				print("sorted aggregated thresholded values")
#				print(as.data.frame(fval.ens))
				scores = setNames(fval.ens[,2], fval.ens[,1])
#				print(scores)
				
				all_na = all(fval.ens$rank == 0) || all(is.na(fval.ens$rank))
				if (all_na) {
						print("Method returned all NAs")
						# Return two values to prevent it crashing
						names = getTaskFeatureNames(task)
						vals  = rep(NA, length(names))
						vals[1:2] = c(1000.0, 1000.0)		
						scores = setNames(vals, names) 
				}
		}
#		write_raw_features(filename, "ens", scores)
		return(scores)
  }
)

# H-mean-value ----------------
#' Mean homogeneous ensemble filter. Takes the mean of the values achieved by each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-mean-value",
  desc = "Mean homogeneous ensemble filter. Takes the mean of the values achieved by each feature.",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, filename, base.method, base.args, hom = TRUE) {
		
#		print("In H-mean-value")
		nselect = ifelse(nselect <= 1, 2, nselect)	# Glmnet models require at least 2 features
		using_probes = length(grep("^probe_*", autothresh)) > 0
		for (i in 1:length(base.args))
				base.args[[i]][["ensemble"]] = TRUE
		
		if (hom) {
			fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args, permute = using_probes)										
		} else {
			resampling = makeResampleDesc("Bootstrap", iters = length(base.method), stratify = TRUE)
			fval.all = genHeterogeneousFilter(task = task, nselect = nselect, resamp = resampling,
												base_methods = base.method, base_args = base.args, permute = using_probes)			
		}
		
#		print("genHomogeneousFilter returned")
		if (is.null(fval.all)) {
				print("Method failed")
				# Return two values to prevent it crashing
				names = getTaskFeatureNames(task)
				vals  = rep(NA, length(names))
				vals[1:2] = c(1000.0, 1000.0)		
				scores = setNames(vals, names) 
		} else {
#				print("fval.all is non-null")
				fval.all = do.call("rbind", fval.all)
#				print(as.data.frame(fval.all))
				
				# If value = NA, feature wasn't selected, so set value to zero so that it is included in the mean calculation
				# If this is not done, features only selected infrequently are given an advantage
				# If value = Inf or -Inf, set value to zero so that it is included in the mean calculation and doesn't cause a crash
				fval.ens = aggregate(replace(fval.all$value, is.na(fval.all$value), 0), by = list(fval.all$name), FUN = mean)
				colnames(fval.ens) = c("name", "value")

				#Sort by value in decreasing order so that the largest value is best, with NA's last
				setorderv(fval.ens, c("value"), order = -1L, na.last = TRUE)
#				print("aggregated values")
#				print(fval.ens)	

#				all_na = 	all(fval.ens$value == 0) || all(is.na(fval.ens$value))
				#Apply automatic threshold, if requested, to determine the most important features
				#If probes are used, auto treshold will remove them
				fval.ens = threshold_auto(fval.ens, fval.all, "value", autothresh)
#				print("Thresholding done")
#				print(length(fval.ens))
				
				# Set rank value of 0.0 back to NA so that those features are excluded
				# NaN is returned by mean when all items were NA. Set NaN back to NA so that those features are excluded
				# This must be done after auto thresholding as k-means doesn't handle NAs
				fval.ens$value[fval.ens$value == 0] = NA
				fval.ens$value[is.nan(fval.ens$value)] = NA
				
				#Sort by value in decreasing order so that the largest value is best, with NA's last
				setorderv(fval.ens, c("value"), order = -1L, na.last = TRUE)
				if (nselect < length(na.omit(fval.ens$value)))
					fval.ens$value[(nselect+1):length(fval.ens$value)] = NA
#				print("sorted aggregated thresholded values")
#				print(as.data.frame(fval.ens))

				scores = setNames(fval.ens[,2], fval.ens[,1])
				all_na = 	all(fval.ens$value == 0) || all(is.na(fval.ens$value))
				if (all_na) {
						print("Method returned all NAs")
						# Return two values to prevent it crashing
						names = getTaskFeatureNames(task)
						vals  = rep(NA, length(names))
						vals[1:2] = c(1000.0, 1000.0)		
						scores = setNames(vals, names) 
				}
		}
#		print(scores)
#		write_raw_features(filename, "ens", scores)
		return(scores)
  }
)


# H-freq ----------------
#' Feature occurrence frequency ensemble filter. 
#' For sparse feature selectors, score 1 point each time a feature is selected by the FS
#' Rank features according to the points scored
#' Finds features which are most often near the top of the list
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-freq",
  desc = "Feature occurrence frequency homogeneous ensemble filter. Calculates the number of times each feature was selected. Threshold before aggregation.",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, filename, base.method, base.args, hom = TRUE) {
	
		using_probes = length(grep("^probe_*", autothresh)) > 0
		for (i in 1:length(base.args))
				base.args[[i]][["ensemble"]] = TRUE

		if (hom) {
			fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args, permute = using_probes)										
		} else {
			resampling = makeResampleDesc("Bootstrap", iters = length(base.method), stratify = TRUE)
			fval.all = genHeterogeneousFilter(task = task, nselect = nselect, resamp = resampling,
												base_methods = base.method, base_args = base.args, permute = using_probes)			
		}
												
		if (is.null(fval.all)) {
				print("Method failed")
				# Return two values to prevent it crashing
				names = getTaskFeatureNames(task)
				vals  = rep(NA, length(names))
				vals[1:2] = c(1000.0, 1000.0)		
				scores = setNames(vals, names) 
		} else {
				# Lists are already sorted by genHomogeneousFilter in order of highest rank first
				# Score 1 if a feature is ranked in top 50% 
				frequencies = rep(0, nrow(fval.all[[1]]))
				names(frequencies) = unlist(fval.all[[1]][,'name'])
				
				for (i in (1:length(fval.all))) {
					top = na.omit(fval.all[[i]])
					topnames = unlist(top[, 'name'])
					frequencies[topnames] = frequencies[topnames] + 1
				}
				frequencies = sort(frequencies, decreasing = TRUE, na.last = TRUE)
#				print(frequencies)
				
				#Apply automatic threshold, if requested, to determine the most important features
				fval.ens = data.table('name' = names(frequencies), 'value' = frequencies)
				fval.ens$value[fval.ens$value == 0] = NA
#				print("aggregated values")
#				print(fval.ens)
		
		
#				all_na = 	all(fval.ens$value == 0) || all(is.na(fval.ens$value))
				fval.all = do.call("rbind", fval.all)

				fval.ens = threshold_auto(fval.ens, fval.all, "value", autothresh)
				
				if (nselect < length(na.omit(fval.ens$value)))
					fval.ens$value[(nselect+1):length(fval.ens$value)] = NA
#				print("sorted aggregated thresholded values")
#				print(as.data.frame(fval.ens))

				scores = setNames(unlist(fval.ens[,2]), unlist(fval.ens[,1]))
				all_na = 	all(fval.ens$value == 0) || all(is.na(fval.ens$value))
				if (all_na) {
						print("Method returned all NAs")
						# Return two values to prevent it crashing
						names = getTaskFeatureNames(task)
						vals  = rep(NA, length(names))
						vals[1:2] = c(1000.0, 1000.0)		
						scores = setNames(vals, names) 
				}
		}
		
#		write_raw_features(filename, "ens", scores)
		return(scores)
 }
)


#' H-RRA ----------------
#' Robust rank aggregation ensemble filter. 
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-RRA",
  desc = "Robust rank aggregation ensemble filter.",
  pkg = "RobustRankAggreg",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, filename, base.method, base.args, hom = TRUE) {

		# DO NOT REMOVE!
		if (inherits(resamp, "ResampleDesc")) {
			resamp = makeResampleInstance(resamp, task = task)
		}
		
		using_probes = length(grep("^probe_*", autothresh)) > 0
		for (i in 1:length(base.args))
				base.args[[i]][["ensemble"]] = TRUE

		if (hom) {
			fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args, permute = using_probes)
		} else {
			resamp = makeResampleDesc("Bootstrap", iters = length(base.method), stratify = TRUE)
			fval.all = genHeterogeneousFilter(task = task, nselect = nselect, resamp = resamp,
												base_methods = base.method, base_args = base.args, permute = using_probes)			
		}
	
		if (is.null(fval.all)) {
				print("Method failed")
				# Return two values to prevent it crashing
				names = getTaskFeatureNames(task)
				vals  = rep(NA, length(names))
				vals[1:2] = c(1000.0, 1000.0)		
				scores = setNames(vals, names) 
		} else {
			sets = list()
			for (i in 1:resamp$desc$iters) {
				df = data.frame(fval.all[[i]])
				sets[[i]] = df[!is.na(df[, 'value']), 'name']
			}
#			print("Sets:")
#			print(sets)
#			r = rankMatrix(sets, full = FALSE)
#			print(r)
			res = aggregateRanks(glist = sets, method = "RRA", exact = FALSE)
#			print(res)
			
			# Remove features with p-value > 0.05
			# Multiply scores by -1 so that the smallest ones, which are the best, are selected
			if (sum(res$Score <= 0.05, na.rm=TRUE) < 2) {
				res$Score[3:length(res$Score)] = rep(NA, length(res$Score) - 2)
			} else {
				res$Score[res$Score > 0.05] = NA
			}
			res$Score = res$Score * -1L
#			print(res)
			all_na = 	all(is.na(res$Score))
			
			names = getTaskFeatureNames(task)
			vals  = rep(NA, length(names))
			names(vals) = names
			vals[res$Name] = res$Score
			scores = setNames(vals, names)
#			print(scores)
			if (all_na) {
					print("Method returned all NAs")
					# Return two values to prevent it crashing
					vals[1:2] = c(1000.0, 1000.0)		
					scores = setNames(vals, names) 
			}
		}
#		write_raw_features(filename, "ens", scores)
		return(scores)
  }
)


#' H-threshold ----------------
#' Threshold algorithm from slides on information retrieval
#' Feature rankings are accessed sequentially i.e.the first element of each list is examined first, then the second element of each list etc. 
#' At each sequential access a threshold equal to the sum of the scores in that access is set.
#' A list of the highest scoring k items seen so far is maintained.
#' Stop when all items in that set are greater than or equal to the threshold. 
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-threshold",
  desc = "Threshold algorithm",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, filename, base.method, base.args, hom = TRUE) {

		# DO NOT REMOVE!
		if (inherits(resamp, "ResampleDesc")) {
			resamp = makeResampleInstance(resamp, task = task)
		}
		
		using_probes = length(grep("^probe_*", autothresh)) > 0
		for (i in 1:length(base.args))
				base.args[[i]][["ensemble"]] = TRUE

		if (hom) {
			fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args, permute = using_probes)										
		} else {
			resamp = makeResampleDesc("Bootstrap", iters = length(base.method), stratify = TRUE)
			fval.all = genHeterogeneousFilter(task = task, nselect = nselect, resamp = resamp,
												base_methods = base.method, base_args = base.args, permute = using_probes)			
		}
		
		if (is.null(fval.all)) {
			print("Method failed")
			# Return two values to prevent it crashing
			names = getTaskFeatureNames(task)
			vals  = rep(NA, length(names))
			vals[1:2] = c(1000.0, 1000.0)		
			scores = setNames(vals, names) 
		} else {
#			print(as.data.frame(fval.all))
			fval.ens = do.call("rbind", fval.all)
			fval.ens = aggregate(fval.ens$value, by = list(fval.ens$name), FUN = sum, na.rm = TRUE)
			colnames(fval.ens) = c("name", "value")
			scores = fval.ens
			scores$value = NA

#			all_na = 	all(fval.ens$value == 0) || all(is.na(fval.ens$value))
			rankings = data.frame(matrix(, nrow = nrow(fval.ens), ncol = 0))
			rscores = data.frame(matrix(, nrow = nrow(fval.ens), ncol = 0))
			for (i in 1:length(fval.all)) {
					tmp = fval.all[[i]]
					non_na = length(na.omit(tmp$value))
					rscores[, paste0("R", i)] = tmp$value
					rankings[1: non_na, paste0("R", i)] = tmp$name[1:non_na]
			}
#			print(rankings)

			k = mean(colSums(!is.na(rankings)))
			if (k == nrow(rankings))
				k = k * 0.5
#			print(paste0("k = ", k))
			rsums = rowSums(rscores, na.rm = TRUE)
#			print(rsums)
			seen = c()
			finished = FALSE
			row_num = 1
			while (!finished && row_num <= nrow(rankings)) {
#				print(paste0("row_num = ", row_num))
				feats = unlist(unique(as.list(rankings[row_num, ])))
				seen = union(seen, feats)
#				print("Features seen:")
#				print(seen)
				seen_scores = fval.ens[fval.ens$name %in% seen, ]
				seen_scores = seen_scores[order(-seen_scores$value),]
#				print("Seen scores:")
#				print(seen_scores)
				thresh = rsums[row_num]
#				print(thresh)
				cnt = sum(fval.ens[fval.ens$name %in% seen, 'value'] > thresh)
#				print(cnt)
				finished = cnt >= k || all(fval.ens[fval.ens$name %in% seen_scores$name[1:k], 'value'] > thresh, na.rm = TRUE)
				row_num = row_num + 1
			}
#  		print(paste0("Threshold: ", thresh))
			final_feats = fval.ens[fval.ens$value > thresh, ]
#			print("Final features:")
#			print(final_feats)
			scores[scores$name %in% final_feats$name, 'value'] = final_feats$value
			scores = scores[order(scores$value, decreasing = TRUE, na.last = TRUE),]
#			print("Scores:")
#			print(scores)
		
			if (all(scores$value == 0) || all(is.na(scores$value))) {
					print("Method returned all NAs")
					# Return two values to prevent it crashing
					names = getTaskFeatureNames(task)
					vals  = rep(NA, length(names))
					vals[1:2] = c(1000.0, 1000.0)		
					scores = setNames(vals, names) 
			}
		}
		
		scores = setNames(scores$value, scores$name)
#		write_raw_features(filename, "ens", scores)
		return(scores)
  }
)


#' H-medrank ----------------
#' Medrank algorithm from slides on information retrieval
#' Feature rankings are accessed sequentially i.e.the first element of each list is examined first, then the second element of each list etc. 
#' When a feature has appeared in more than half of the ranked lists, it is output to the aggregated ranking. 
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-medrank",
  desc = "Medrank algorithm",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, filename, base.method, base.args, hom = TRUE) {

		# DO NOT REMOVE!
		if (inherits(resamp, "ResampleDesc")) {
			resamp = makeResampleInstance(resamp, task = task)
		}
		
		using_probes = length(grep("^probe_*", autothresh)) > 0
		for (i in 1:length(base.args))
				base.args[[i]][["ensemble"]] = TRUE

		if (hom) {
			fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args, permute = using_probes)										
		} else {
			resamp = makeResampleDesc("Bootstrap", iters = length(base.method), stratify = TRUE)
			fval.all = genHeterogeneousFilter(task = task, nselect = nselect, resamp = resamp,
												base_methods = base.method, base_args = base.args, permute = using_probes)			
		}
			
		if (is.null(fval.all)) {
			print("Method failed")
			# Return two values to prevent it crashing
			names = getTaskFeatureNames(task)
			vals  = rep(NA, length(names))
			vals[1:2] = c(1000.0, 1000.0)		
			scores = setNames(vals, names) 
		} else {
			fval.ens = do.call("rbind", fval.all)
			fval.ens = aggregate(fval.ens$value, by = list(fval.ens$name), FUN = sum, na.rm = TRUE)
			colnames(fval.ens) = c("name", "value")
			scores = fval.ens
			scores$value = NA

#			all_na = 	all(fval.ens$value == 0) || all(is.na(fval.ens$value))
			rankings = data.frame(matrix(, nrow = nrow(fval.ens), ncol = 0))
			for (i in 1:length(fval.all)) {
				tmp = fval.all[[i]]
				non_na = length(na.omit(tmp$value))
				rankings[1: non_na, paste0("R", i)] = tmp$name[1:non_na]
			}
			
			k = mean(colSums(!is.na(rankings)))
			if (k == nrow(rankings))
				k = k * 0.5
#			print(paste0("k =  ", k))
			threshold = 0.2 * length(fval.all)   # NB 20% no 50% like in original algorithm
			seen = rep(0, nrow(fval.ens))
			names(seen) = sort(fval.ens$name)
			finished = FALSE
			row_num = 1

			while (!finished && row_num <= nrow(rankings)) {
	#			print(paste0("row_num = ", row_num))
				t = table(unlist(rankings[row_num, ]), useNA = "no")
				finished = all(t == 0)
				if (!finished) {
					for (i in 1:length(t)) {
						nm = names(t[i]); 
						seen[nm] = seen[nm] + t[i]
					}
					finished = length(seen[seen > threshold]) >= k
				}
				row_num = row_num + 1
			}
			
			scores[scores$name %in% names(seen), 'value'] = seen[scores$name]
			scores[scores$value < threshold, 'value' ] = NA
			scores = scores[order(scores$value, decreasing = TRUE, na.last = TRUE),]
			
			if (all(scores$value == 0) || all(is.na(scores$value))) {
					print("Method returned all NAs")
					# Return two values to prevent it crashing
					names = getTaskFeatureNames(task)
					vals  = rep(NA, length(names))
					vals[1:2] = c(1000.0, 1000.0)		
					scores = setNames(vals, names) 
			}
		}
		
		scores = setNames(scores$value, scores$name)
#		write_raw_features(filename, "ens", scores)
		return(scores)
  }
)


#' H-medthreshold ----------------
#' Combination of the Medrank algorithm and teh Threshold algorithm
#' Feature rankings are accessed sequentially i.e.the first element of each list is examined first, then the second element of each list etc. 
#' When a feature has appeared in more than half of the ranked lists, it is output to the aggregated ranking. 
#' At each sequential access a threshold equal to the sum of the scores in that access is set.
#' Stop when all items found so far are greater than or equal to the threshold. Don't need to set k in advance, it is dynamic.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-medthreshold",
  desc = "MedThreshold algorithm",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, filename, base.method, base.args, hom = TRUE) {

		# DO NOT REMOVE!
		if (inherits(resamp, "ResampleDesc")) {
			resamp = makeResampleInstance(resamp, task = task)
		}
		
		using_probes = length(grep("^probe_*", autothresh)) > 0
		for (i in 1:length(base.args))
				base.args[[i]][["ensemble"]] = TRUE

		if (hom) {
			fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args, permute = using_probes)										
		} else {
			resamp = makeResampleDesc("Bootstrap", iters = length(base.method), stratify = TRUE)
			fval.all = genHeterogeneousFilter(task = task, nselect = nselect, resamp = resamp,
												base_methods = base.method, base_args = base.args, permute = using_probes)			
		}
			
		if (is.null(fval.all)) {
			print("Method failed")
			# Return two values to prevent it crashing
			names = getTaskFeatureNames(task)
			vals  = rep(NA, length(names))
			vals[1:2] = c(1000.0, 1000.0)		
			scores = setNames(vals, names) 
		} else {
			fval.ens = do.call("rbind", fval.all)
			fval.ens = aggregate(fval.ens$value, by = list(fval.ens$name), FUN = sum, na.rm = TRUE)
			colnames(fval.ens) = c("name", "value")
			scores = fval.ens
			scores$value = NA

			# Set up the ranked lists of names and scores
#			all_na = 	all(fval.ens$value == 0) || all(is.na(fval.ens$value))
			rankings = data.frame(matrix(, nrow = nrow(fval.ens), ncol = 0))   # Ranked feature lists - one list per column - names
			rscores  = data.frame(matrix(, nrow = nrow(fval.ens), ncol = 0))   # Ranked feature lists - one list per column - scores
			for (i in 1:length(fval.all)) {
				tmp = fval.all[[i]]
				non_na = length(na.omit(tmp$value))
				rscores[, paste0("R", i)] = tmp$value
				rankings[1: non_na, paste0("R", i)] = tmp$name[1:non_na]
			}
			rsums = rowSums(rscores, na.rm = TRUE)
			seln_length = 0.5 * length(fval.all)   # Original algoritrhm uses 0.5
			seen_num = rep(0, nrow(fval.ens))       # Number of times each feature has been seen
			names(seen_num) = sort(fval.ens$name)
			seen_scores = rep(0, nrow(fval.ens))			# Total scores of each feature seen
			names(seen_scores) = sort(fval.ens$name)
			
			seen = c()
			finished = FALSE
			row_num = 1
			k = 0

			while (!finished && row_num <= nrow(fval.ens)) {
#				print(paste0("Row # = ", row_num))
				t = table(unlist(rankings[row_num, ]), useNA = "no")
				finished = all(t == 0)
				if (!finished) {
					for (i in 1:length(t)) {
						nm = names(t[i]); 
						seen_num[nm] = seen_num[nm] + t[i]
					}
				}
#				print("Number of times seen:")
#				print(seen_num)
				
				for (j in 1:ncol(rankings)) {
					nm = rankings[row_num, j]
					seen_scores[nm] = seen_scores[nm] + rscores[row_num, j]
				}
#				print("Seen scores:")
#				print(seen_scores)
				
				feats = unlist(unique(as.list(rankings[row_num, ])))
				seen = union(seen, feats)
#				print("Seen so far:")
#				print(seen)
				thresh = rsums[row_num]
#				print(paste0("Thresh = ", thresh))
				
				feats_output = names(seen_num[seen_num > seln_length])
#				print("feats_output")
#				print(feats_output)
#				print("Scores of feats_output:")
#				print(seen_scores[names(seen_scores) %in% feats_output])
				finished = any(seen_scores[names(seen_scores) %in% feats_output] > thresh)
#				row_num = row_num + 1
			}
			
			scores[scores$name %in% feats_output, 'value'] = seen_scores[names(seen_scores) %in% feats_output]
			scores = scores[order(scores$value, decreasing = TRUE, na.last = TRUE),]
			
			if (all(scores$value == 0) || all(is.na(scores$value))) {
					print("Method returned all NAs")
					# Return two values to prevent it crashing
					names = getTaskFeatureNames(task)
					vals  = rep(NA, length(names))
					vals[1:2] = c(1000.0, 1000.0)		
					scores = setNames(vals, names) 
			}
		}
		
		scores = setNames(scores$value, scores$name)
#		write_raw_features(filename, "ens", scores)
		return(scores)
  }
)

#' H-union ----------------
#' Union ensemble filter. Takes the union of multiple sets.
#' For use with sparse base filters.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-union",
  desc = "Union ensemble filter. Takes the union of multiple sets.",
  pkg = "RVenn",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, base.method, base.args) {
		if (inherits(resamp, "ResampleDesc"))
			resamp = makeResampleInstance(resamp, task = task)
			
    fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args)
		
		sets = list()
		for (i in 1:resamp$desc$iters) {
			df = fval.all[[i]]
			sets[i] = df[!is.na(df$value), 'name']	
		}
		
		sets = Venn(sets)
		result = unite(sets)
		if (nselect < length(result)) {
			result = result[1:nselect]
		}
		setNames(result, result)
  }
)


#' H-intersection ----------------
#' Intersection ensemble filter. Takes the intersection of multiple sets.
#' For use with sparse base filters.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-intersection",
  desc = "Intersection ensemble filter. Takes the intersection of multiple sets.",
  pkg = "RVenn",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, base.method, base.args) {
		if (inherits(resamp, "ResampleDesc"))
			resamp = makeResampleInstance(resamp, task = task)
			
    fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args)

		sets = list()
		for (i in 1:resamp$desc$iters) {
			df = fval.all[[i]]
			sets[i] = df[!is.na(df$value), 'name']	
		}
		
		sets = Venn(sets)
		result = overlap(sets)
		if (nselect < length(result)) {
			result = result[1:nselect]
		}
		
		if (length(result) == 0) {
			item = sets@sets$Set_1[1]
			return(setNames(item, item))
		} else {
			return(setNames(result, result))
		}
  }
)


# H-Borda ----------------
#' Borda Count homogeneous ensemble filter. Uses the Borda Count voting system
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-Borda",
  desc = "Borda Count homogeneous ensemble filter. Uses modified borda count from library votesys",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, base.method, base.args) {
    fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args)										
	  fval.all = do.call("rbind", fval.all)
		
		fval.ens = aggregate(fval.all$rank, by = list(fval.all$name), FUN = mean, na.rm = TRUE)		
    colnames(fval.ens) = c("name", "value")	
		setorderv(fval.ens, c("value"), order = -1L, na.last = TRUE)
		
		#Apply automatic threshold, if any, to determine the most important features
		fval.ens = threshold_auto(fval.ens, fval.all, "value", autothresh)
		
		setNames(fval.ens[,2], fval.ens[,1])
  }
)


# H-salience ----------------
#' Salience homogeneous ensemble filter. Takes the mean of the values divided by the variance.
#' The idea is that it selects features that achieve a high score consistently - Gelareh's suggestion.
#' Could be applied to any value - rank, frequency etc. Pass this as an argument?
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-salience",
  desc = "Salience homogeneous ensemble filter. Takes the mean of the values divided by the variance.",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, filename, base.method, base.args) {
		base.args[[base.method]][["ensemble"]] = TRUE
		fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args)										
	  fval.all = do.call("rbind", fval.all)
		
		# If value = NA, feature wasn't selected, so set value to zero so that it is included in the mean calculation
		# If this is not done, features only selected infrequently are given an advantage
		# If value = Inf or -Inf, set value to zero so that it is included in the mean calculation and doesn't cause a crash
		fval.all$value[is.na(fval.all$value)] = 0
		fval.all$value[is.infinite(fval.all$value)] = 0
		
		fval.ens = aggregate(fval.all$value, by = list(fval.all$name), FUN = function(x) mean(x, na.rm = TRUE)/var(x, na.rm = TRUE))  
    colnames(fval.ens) = c("name", "value")
		setorderv(fval.ens, c("value"), order = -1L, na.last = TRUE)		
		
		#Apply automatic threshold, if requested, to determine the most important features
		fval.ens = threshold_auto(fval.ens, fval.all, "value", autothresh)
		
		# Set rank value of 0.0 back to NA so that those features are excluded
		# NaN is returned by mean when all items were NA. Set NaN back to NA so that those features are excluded
		# This must be done after auto thresholding as k-means doesn't handle NAs
		fval.ens$value[fval.ens$value == 0] = NA
		fval.ens$value[is.nan(fval.ens$value)] = NA
		
		scores = setNames(fval.ens[,2], fval.ens[,1])
#		write_raw_features(filename, "ens", scores)
		return(scores)
  }
)


#' H-consrank ----------------
#' Consensus ranking ensemble filter - too slow to be practical
#' Uses package ConsRank and its tau-x (variation on Kendall's tau) to arrive at a consensus ranking.
#' For use with ranked lists
#'
#' @rdname makeFilter
#' @name makeFilter
#makeFilter(
#  name = "H-consrank",
#  desc = "Consensus ranking ensemble filter. Uses package ConsRank and its tau-x (variation on Kendall's tau) to arrive at a consensus ranking",
#  pkg = "ConsRank",
#  supported.tasks = c("classif", "regr", "surv"),
#  supported.features = c("numerics", "factors", "ordered"),
#  fun = function(task, nselect, resamp, autothresh, base.method, base.args) {
#		if (inherits(resamp, "ResampleDesc"))
#			resamp = makeResampleInstance(resamp, task = task)
#    fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
#											base_method = base.method, base_args = base.args)
		
#		names = sort(fval.all[[1]][['name']])
#		cons.mat <- matrix(, nrow = resamp$desc$iters, ncol = length(names))
#		colnames(cons.mat) = names
#		for (i in 1:resamp$desc$iters) {
#			df = setDF(fval.all[[i]])
#			cons.mat[i, names] = df[match(names, df$name), 'rank']
#		}
		
#		cons.ranking = consrank(X = cons.mat, algorithm = "quick")   # Crashes with: object 'out' not found
#		fval.ens = sort(cons.ranking$Consensus, decreasing = TRUE)
#		if (nselect < length(na.omit(fval.ens))) {
#			fval.ens = fval.ens[1:nselect]
#		}
#		return(fval.ens)
#  }
#)


# H-std-dev ----------------
#' Mean homogeneous ensemble filter. Takes the mean of the values achieved by each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "H-sd",
  desc = "Mean and std dev homogeneous ensemble filter. Takes the mean and std dev of the values achieved by each feature.",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, resamp, autothresh, filename, base.method, base.args, hom = TRUE) {
		
		for (i in 1:length(base.args))
				base.args[[i]][["ensemble"]] = TRUE
		
		using_probes = length(grep("^probe_*", autothresh)) > 0
		fval.all = genHomogeneousFilter(task = task, nselect = nselect, resamp = resamp,
											base_method = base.method, base_args = base.args, permute = using_probes)												
		fval.all = do.call("rbind", fval.all)
		
		fval.ens = aggregate(replace(fval.all$value, is.na(fval.all$value), 0), by = list(fval.all$name), FUN = mean)
		colnames(fval.ens) = c("name", "value")
		setorderv(fval.ens, c("value"), order = -1L, na.last = TRUE)
#		print("Mean")
#		print(as.data.frame(fval.ens))
		
		fval.ens = aggregate(replace(fval.all$value, is.na(fval.all$value), 0), by = list(fval.all$name), FUN = sd)
		colnames(fval.ens) = c("name", "value")
		setorderv(fval.ens, c("value"), order = -1L, na.last = TRUE)
#		print("Standard deviation")
#		print(as.data.frame(fval.ens))

		scores = setNames(fval.ens[,2], fval.ens[,1])
		return(scores)
  }
)

