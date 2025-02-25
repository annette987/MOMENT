#' @importFrom mlr makeRLearner
#' @importFrom mlr trainLearner
#' @importFrom mlr predictLearner

#' @export
makeRLearner.surv.CoxBoost = function() {
  mlr::makeRLearnerSurv(
    cl = "surv.CoxBoost",
    package = "!CoxBoost",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerVectorLearnerParam(id = "unpen.index"),
      ParamHelpers::makeLogicalLearnerParam(id = "standardize", default = TRUE),
      ParamHelpers::makeNumericLearnerParam(id = "penalty", lower = 0),
      ParamHelpers::makeDiscreteLearnerParam(id = "criterion", default = "pscore", values = c("pscore", "score", "hpscore", "hscore")),
      ParamHelpers::makeNumericLearnerParam(id = "stepsize.factor", default = 1, lower = 0),
      ParamHelpers::makeIntegerLearnerParam(id = "stepno", default = 100L, lower = 1),
      ParamHelpers::makeLogicalLearnerParam(id = "return.score", default = TRUE, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(return.score = FALSE),
    properties = c("numerics", "factors", "ordered", "weights"),
    name = "Cox Proportional Hazards Model with Componentwise Likelihood based Boosting",
    short.name = "coxboost",
    note = "Factors automatically get converted to dummy columns, ordered factors to integer.",
    callees = "CoxBoost"
  )
}

#' @export
trainLearner.surv.CoxBoost = function(.learner, .task, .subset, .weights = NULL, penalty = NULL, unpen.index = NULL, ...) {
  data = mlr::getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "surv")
  info = mlr:::getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
  data$data = as.matrix(mlr:::fixDataForLearner(data$data, info))

  if (is.null(penalty)) {
    penalty = 9 * sum(data$target[, 2L])
  }

  mlr:::attachTrainingInfo(CoxBoost::CoxBoost(
    time = data$target[, 1L],
    status = data$target[, 2L],
    x = data$data,
    weights = .weights,
    penalty = penalty,
    ...
  ), info)
}

#' @export
predictLearner.surv.CoxBoost = function(.learner, .model, .newdata, ...) {
  info = mlr:::getTrainingInfo(.model)
  .newdata = as.matrix(mlr:::fixDataForLearner(.newdata, info))
  as.numeric(mlr::predictLearner(.model$learner.model, newdata = .newdata, type = "lp"))
}
