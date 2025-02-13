#' @export
makeRLearner.classif.imputedRF = function() {
  makeRLearnerClassif(
    cl = "classif.imputedRF",
    package = "randomForest",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerLearnerParam(id = "ntree", default = 500L, lower = 1L),
      ParamHelpers::makeIntegerLearnerParam(id = "mtry", lower = 1L),
      ParamHelpers::makeLogicalLearnerParam(id = "replace", default = TRUE),
      ParamHelpers::makeNumericVectorLearnerParam(id = "classwt", lower = 0),
      ParamHelpers::makeNumericVectorLearnerParam(id = "cutoff", lower = 0, upper = 1),
      ParamHelpers::makeUntypedLearnerParam(id = "strata", tunable = FALSE),
      ParamHelpers::makeIntegerVectorLearnerParam(id = "sampsize", lower = 1L),
      ParamHelpers::makeIntegerLearnerParam(id = "nodesize", default = 1L, lower = 1L),
      ParamHelpers::makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      ParamHelpers::makeLogicalLearnerParam(id = "importance", default = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "localImp", default = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "proximity", default = FALSE, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "oob.prox", requires = quote(proximity == TRUE), tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "norm.votes", default = TRUE, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "keep.forest", default = TRUE, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "ordered", "prob", "class.weights", "oobpreds", "featimp"),
    class.weights.param = "classwt",
    name = "Random Forest",
    short.name = "rf",
    note = "Note that the rf can freeze the R process if trained on a task with 1 feature which is constant. This can happen in feature forward selection, also due to resampling, and you need to remove such features with removeConstantFeatures.",
    callees = "randomForest"
  )
}

#' @export
trainLearner.classif.imputedRF = function(.learner, .task, .subset, .weights = NULL, classwt = NULL, cutoff, ...) {
  f = mlr::getTaskFormula(.task)
  data = mlr::getTaskData(.task, .subset, recode.target = "drop.levels")
  levs = levels(data[, mlr::getTaskTargetNames(.task)])
  n = length(levs)
  if (missing(cutoff)) {
    cutoff = rep(1 / n, n)
  }
  if (!missing(classwt) && is.numeric(classwt) && length(classwt) == n && is.null(names(classwt))) {
    names(classwt) = levs
  }
  if (is.numeric(cutoff) && length(cutoff) == n && is.null(names(cutoff))) {
    names(cutoff) = levs
  }
  randomForest::randomForest(f, data = data, classwt = classwt, cutoff = cutoff, ...)
}

#' @export
predictLearner.classif.imputedRF = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "response", "prob")
  randomForest::predict(.model$learner.model, newdata = .newdata, type = type, ...)
}

#' @export
getOOBPredsLearner.classif.imputedRF = function(.learner, .model) {
  if (.learner$predict.type == "response") {
    m = mlr::getLearnerModel(.model, more.unwrap = TRUE)
    unname(m$predicted)
  } else {
    mlr::getLearnerModel(.model, more.unwrap = TRUE)$votes
  }
}

#' @export
getFeatureImportanceLearner.classif.imputedRF = function(.learner, .model, ...) {
  mod = mlr::getLearnerModel(.model, more.unwrap = TRUE)
  ctrl = list(...)
  if (is.null(ctrl$type)) {
    ctrl$type = 2L
  } else {
    if (ctrl$type == 1L) {
      has.fiv = .learner$par.vals$importance
      if (is.null(has.fiv) || has.fiv != TRUE) {
        stop("You need to train the learner with parameter 'importance' set to TRUE")
      }
    }
  }
  randomForest::importance(mod, ctrl$type)[, 1]
}
