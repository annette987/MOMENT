library(mlr)

##########################################################################
# Custom filters. Must return a single set of named scores (not per class)
##########################################################################

#
# ranger permutation filter
#
makeFilter(
  name = "ranger.fse",
  desc = "Variable importance based on ranger permutation importance, only if the model exceeds a performance threshold",
  pkg = "ranger",
  supported.tasks = c("regr", "classif", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, method = "permutation", filename, ensemble, ...) {
		
		# Setup all zeroes result as default
		fns = getTaskFeatureNames(task)
		scores = setNames(rep(0.0, length(fns)), fns)

		lrn.type = paste0(getTaskType(task), ".ranger")	
    ranger.lrn = makeLearner(cl = lrn.type, predict.type = "prob", importance = method, ...)		
		model = train(ranger.lrn, task)
		mod = getLearnerModel(model, more.unwrap = TRUE)
		imp_data = getFeatImpScores(mod, getTaskClassLevels(task))

		if (!is.null(imp_data)) {
			scores = imp_data[, "all"]
			scores = setNames(scores, row.names(imp_data))
		}
		return(scores)
  }
)


#
# random forest variable importance with optional performance check ----------------
#
makeFilter(
  name = "rfsrc.importance.fse",
  desc = "Use rfsrc to select features, only if the model exceeds a performance threshold",
  pkg = "randomForestSRC",
  supported.tasks = c("regr", "classif", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, method="permute", filename, ensemble, ...) {
		
		# Setup all zeroes result as default
		fns = getTaskFeatureNames(task)
		scores = setNames(rep(0.0, length(fns)), fns)

		if (method == "md" || method == "vh") {
				im = randomForestSRC::var.select(getTaskFormula(task), getTaskData(task), method = method, verbose = FALSE, ...)
				# For RF-MD the lowest score is the best so multiply by -1 to reverse this.
				im$varselect[, "depth"] = -1L * im$varselect[, "depth"]
				im$varselect[setdiff(rownames(im$varselect), im$topvars), "depth"] = NA
				scores = setNames(im$varselect[, "depth"], rownames(im$varselect))		
		} else {
				im = randomForestSRC::rfsrc(getTaskFormula(task), data = getTaskData(task), proximity = FALSE, forest = FALSE, importance = method, ...)$importance
				if (inherits(task, "ClassifTask")) {
					ns = rownames(im)
					y = im[, "all"]
				} else {
					ns = names(im)
					y = unname(im)
				}
				scores = setNames(y, ns)
		}
		return(scores)		
  }
)


#
# cv.glmnet with optional performance check ----------------
#
makeFilter(
  name = "cv.glmnet.fse",
  desc = "Use cv.glmnet to select features, only if the model exceeds a performance threshold",
  pkg = "glmnet",
  supported.tasks = c("regr", "classif", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, filename, ensemble, ...) {

		# Setup all zeroes result as default
    fns = getTaskFeatureNames(task)
		scores = setNames(rep(0.0, length(fns)), fns)
		
		lrn.type = paste0(getTaskType(task), ".cvglmnet")	
    glmnet.lrn = makeLearner(cl = lrn.type, predict.type = "prob", ...)
		model = train(glmnet.lrn, task)
		mod = getLearnerModel(model, more.unwrap = TRUE)
		imp_data = getFeatImpScores(mod, getTaskClassLevels(task))
		scores = imp_data[, "all"]  # There is no 'all' class for cv.glmnet. What to do?
		scores = setNames(scores, rownames(imp_data))

		return(scores)
  }
)


#
# XGBoost with optional performance check ----------------
# This one has problems
#
makeFilter(
  name = "XGBoost.fse",
  desc = "Use XGBoost to select features, only if the model exceeds a performance threshold",
  pkg = "xgboost",
  supported.tasks = c("regr", "classif", "surv"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, filename, ensemble, ...) {

		# Setup all zeroes result as default
		fns = getTaskFeatureNames(task)
		scores = setNames(rep(0.0, length(fns)), fns)

		lrn.type = paste0(getTaskType(task), ".xgboost")	
    xgboost.lrn = makeLearner(cl = lrn.type, predict.type = "prob", ...)
		model = train(xgboost.lrn, task)
		mod = getLearnerModel(model, more.unwrap = FALSE)
		imp_data = getFeatImpScores(mod, getTaskClassLevels(task))

		if (!is.null(imp_data)) {
			scores = imp_data[, "all"]
			scores = setNames(scores, rownames(imp_data))
		}
		return(scores)
}
)



#
# simple variance filter with optional performance check ----------------
#
makeFilter(
  name = "variance.fse",
  desc = "Simple variance filter",
  pkg = character(0L),
  supported.tasks = c("regr", "classif", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, method = "permutation", filename, ensemble, ...) {

    data = getTaskData(task)
    res = sapply(getTaskFeatureNames(task), function(feat.name) {
      var(data[[feat.name]], na.rm = TRUE)
    })
		return(res)
  }
)

#
# univariate model score with optional performance check ----------------
#
makeFilter(
  name = "univariate.fse",
  desc = "Resamples an mlr learner for each input feature individually. Cox model is the defaut. The resampling performance is used as filter score only if the model exceeds a performance threshold",
  pkg = character(0L),
  supported.tasks = c("regr", "classif", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, perf.learner = NULL, filename, ensemble, ...) {

		perf.learner = makeLearner(cl="surv.coxph", id = "perf.cox", predict.type="prob")
    perf.measure = getDefaultMeasure(task)
    perf.resampling = makeResampleDesc("Subsample", iters = 1L, split = 0.67)  		

    fns = getTaskFeatureNames(task)
    res = double(length(fns))
    for (i in seq_along(fns)) {
      subtask = subsetTask(task, features = fns[i])
      res[i] = resample(learner = perf.learner, task = subtask, resampling = perf.resampling, measures = perf.measure, keep.pred = FALSE, show.info = FALSE)$aggr
			if (ensemble && !is.na(res[i]) && res[i] < perf.threshold)
				res[i] = NA
		}

    scores = setNames(res, fns)
		return(scores)
  }
)



