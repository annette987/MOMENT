#' @export
makeRLearner.surv.randomForestSRC_VS = function() {
  makeRLearnerSurv(
    cl = "surv.randomForestSRC_VS",
    package = c("survival", "randomForestSRC"),
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      ParamHelpers::makeDiscreteLearnerParam(id = "bootstrap", default = "by.root",
        values = c("by.root", "by.node", "none")),
      ParamHelpers::makeIntegerLearnerParam(id = "mtry", lower = 1L),
      ParamHelpers::makeIntegerLearnerParam(id = "nodesize", lower = 1L, default = 3L),
      ParamHelpers::makeIntegerLearnerParam(id = "nodedepth", default = -1L),
      ParamHelpers::makeDiscreteLearnerParam(id = "splitrule", default = "logrank",
        values = c("logrank", "logrankscore", "random")),
      ParamHelpers::makeIntegerLearnerParam(id = "nsplit", lower = 0L, default = 0L,
        requires = quote(splitrule != "random")), # nsplit is ignored and internally set to 1 for splitrule = "random"
      ParamHelpers::makeLogicalLearnerParam(id = "split.null", default = FALSE),
      ParamHelpers::makeDiscreteLearnerParam(id = "importance", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, `TRUE` = TRUE, "none", "permute", "random",
          "anti", "permute.ensemble", "random.ensemble", "anti.ensemble")),
      ParamHelpers::makeDiscreteLearnerParam(id = "na.action", default = "na.impute",
        values = c("na.omit", "na.impute"), when = "both"),
      # FIXME default for na.action in rfsrc() is na.omit
      ParamHelpers::makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
      ParamHelpers::makeUntypedLearnerParam(id = "ntime"), # can be a single integer with number of time points or a numeric vector of time values
      ParamHelpers::makeDiscreteLearnerParam(id = "proximity", default = FALSE, tunable = FALSE,
        values = list("inbag", "oob", "all", `TRUE` = TRUE, `FALSE` = FALSE)),
      ParamHelpers::makeIntegerLearnerParam(id = "sampsize", lower = 1L,
        requires = quote(bootstrap == "by.root")),
      ParamHelpers::makeDiscreteLearnerParam(id = "samptype", default = "swr", values = c("swr", "swor"),
        requires = quote(bootstrap == "by.root")),
      ParamHelpers::makeNumericVectorLearnerParam(id = "xvar.wt", lower = 0),
      ParamHelpers::makeLogicalLearnerParam(id = "forest", default = TRUE, tunable = FALSE),
      ParamHelpers::akeDiscreteLearnerParam(id = "var.used", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, "all.trees", "by.tree")),
      ParamHelpers::makeDiscreteLearnerParam(id = "split.depth", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, "all.trees", "by.tree")),
      ParamHelpers::makeIntegerLearnerParam(id = "seed", upper = 0L, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE, when = "both"), # is currently ignored
      ParamHelpers::makeLogicalLearnerParam(id = "membership", default = TRUE, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "statistics", default = FALSE, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "tree.err", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(na.action = "na.impute"),
    properties = c("missings", "numerics", "factors", "ordered", "weights", "oobpreds", "featimp"),
    name = "Random Forest",
    short.name = "rfsrc",
    note = '`na.action` has been set to `"na.impute"` by default to allow missing data support.',
    callees = "rfsrc"
  )
}

#' @export
trainLearner.surv.randomForestSRC_VS = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  randomForestSRC::var.select(f, data = mlr::getTaskData(.task, subset = .subset), case.wt = .weights, ...)
}

#' @export
predictLearner.surv.randomForestSRC_VS = function(.learner, .model, .newdata, ...) {
  randomForestSRC::predict(.model$learner.model, newdata = .newdata, membership = FALSE, ...)$predicted
}
