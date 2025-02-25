#' @importFrom mlr makeRLearner
#' @importFrom mlr trainLearner
#' @importFrom mlr predictLearner

#' @export
makeRLearner.classif.imputedRF = function() {
  mlr::makeRLearnerClassif(
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
  mlr::predictLearner(.model$learner.model, newdata = .newdata, type = type, ...)
}
