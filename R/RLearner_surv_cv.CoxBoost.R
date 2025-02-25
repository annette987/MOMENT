#' @importFrom mlr makeRLearner
#' @importFrom mlr trainLearner
#' @importFrom mlr predictLearner


#' @export
makeRLearner.surv.cv.CoxBoost = function() {
  mlr::makeRLearnerSurv(
    cl = "surv.cv.CoxBoost",
    package = "!CoxBoost",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerLearnerParam(id = "maxstepno", default = 100L, lower = 0L),
      ParamHelpers::makeIntegerLearnerParam(id = "K", default = 10L, lower = 1L),
      ParamHelpers::makeDiscreteLearnerParam(id = "type", default = "verweij", values = c("verweij", "naive")),
      ParamHelpers::makeLogicalLearnerParam(id = "parallel", default = FALSE, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "upload.x", default = FALSE, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "multicore", default = FALSE, tunable = FALSE),
      ParamHelpers::makeIntegerVectorLearnerParam(id = "unpen.index"),
      ParamHelpers::makeLogicalLearnerParam(id = "standardize", default = TRUE),
      ParamHelpers::makeNumericLearnerParam(id = "penalty", lower = 0),
      ParamHelpers::makeDiscreteLearnerParam(id = "criterion", default = "pscore", values = c("pscore", "score", "hpscore", "hscore")),
      ParamHelpers::makeNumericLearnerParam(id = "stepsize.factor", default = 1, lower = 0),
      ParamHelpers::makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors", "weights"),
    name = "Cox Proportional Hazards Model with Componentwise Likelihood based Boosting, tuned for the optimal number of boosting steps",
    short.name = "cv.CoxBoost",
    note = "Factors automatically get converted to dummy columns, ordered factors to integer.",
    callees = c("cv.CoxBoost", "CoxBoost")
  )
}

#' @export
trainLearner.surv.cv.CoxBoost = function(.learner, .task, .subset, .weights = NULL, penalty = NULL, unpen.index = NULL, ...) {

  data = mlr::getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "surv")
  info = mlr:::getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)

  if (is.null(penalty)) {
    penalty = 9 * sum(data$target[, 2L])
  }

  pars = c(list(
    time = data$target[, 1L],
    status = data$target[, 2L],
    x = as.matrix(mlr:::fixDataForLearner(data$data, info)),
    penalty = penalty,
    weights = .weights
  ), list(...))
  rm(data)

  res = do.call(CoxBoost::cv.CoxBoost, pars)
  res$optimal.step
  if (res$optimal.step == 0L) {
    warning("Could not determine the optimal step number in cv.CoxBoost")
  }

  pars = R.utils::insert(pars, list(stepno = res$optimal.step))
  pars$maxstepno = NULL
  mlr:::attachTrainingInfo(do.call(CoxBoost::CoxBoost, pars), info)
}

#' @export
predictLearner.surv.cv.CoxBoost = function(.learner, .model, .newdata, ...) {
  info = mlr:::getTrainingInfo(.model)
  .newdata = as.matrix(mlr:::fixDataForLearner(.newdata, info))
  as.numeric(mlr::predictLearner(.model$learner.model, newdata = .newdata, type = "lp"))
}
