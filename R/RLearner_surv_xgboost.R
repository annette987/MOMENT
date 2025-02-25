#' @importFrom mlr makeRLearner
#' @importFrom mlr trainLearner
#' @importFrom mlr predictLearner

#' @export
makeRLearner.surv.xgboost = function() {
  mlr::makeRLearnerSurv(
    cl = "surv.xgboost",
    package = "xgboost",
    par.set = ParamHelpers::makeParamSet(
      # we pass all of what goes in 'params' directly to ... of xgboost
      # ParamHelpers::makeUntypedLearnerParam(id = "params", default = list()),
      ParamHelpers::makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
      ParamHelpers::makeUntypedLearnerParam(id = "watchlist", default = NULL, tunable = FALSE),
      ParamHelpers::makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      ParamHelpers::makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      ParamHelpers::makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 0L),
      ParamHelpers::makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      ParamHelpers::makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      ParamHelpers::makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      ParamHelpers::makeNumericLearnerParam(id = "colsample_bylevel", default = 1, lower = 0, upper = 1),
      ParamHelpers::makeNumericLearnerParam(id = "colsample_bynode", default = 1, lower = 0, upper = 1),
      ParamHelpers::makeIntegerLearnerParam(id = "num_parallel_tree", default = 1L, lower = 1L),
      ParamHelpers::makeNumericLearnerParam(id = "lambda", default = 1, lower = 0),
      ParamHelpers::makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
      ParamHelpers::makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      ParamHelpers::makeUntypedLearnerParam(id = "objective", default = "reg:linear", tunable = FALSE),
      ParamHelpers::makeUntypedLearnerParam(id = "eval_metric", default = "rmse", tunable = FALSE),
      ParamHelpers::makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      ParamHelpers::makeNumericLearnerParam(id = "max_delta_step", lower = 0, default = 0),
      ParamHelpers::makeNumericLearnerParam(id = "missing", default = NA, tunable = FALSE, when = "both", special.vals = list(NA, NA_real_, NULL)),
      ParamHelpers::makeIntegerVectorLearnerParam(id = "monotone_constraints", default = 0, lower = -1, upper = 1),
      ParamHelpers::makeNumericLearnerParam(id = "tweedie_variance_power", lower = 1, upper = 2, default = 1.5, requires = quote(objective == "reg:tweedie")),
      ParamHelpers::makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      ParamHelpers::makeIntegerLearnerParam(id = "nrounds", lower = 1L),
      ParamHelpers::makeUntypedLearnerParam(id = "feval", default = NULL, tunable = FALSE),
      ParamHelpers::makeIntegerLearnerParam(id = "verbose", default = 1L, lower = 0L, upper = 2L, tunable = FALSE),
      ParamHelpers::makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE, requires = quote(verbose == 1L)),
      ParamHelpers::makeIntegerLearnerParam(id = "early_stopping_rounds", default = NULL, lower = 1L, special.vals = list(NULL), tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE),
      ParamHelpers::makeDiscreteLearnerParam(id = "sample_type", default = "uniform", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
      ParamHelpers::makeDiscreteLearnerParam(id = "normalize_type", default = "tree", values = c("tree", "forest"), requires = quote(booster == "dart")),
      ParamHelpers::makeNumericLearnerParam(id = "rate_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      ParamHelpers::makeNumericLearnerParam(id = "skip_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      ParamHelpers::makeNumericLearnerParam(id = "scale_pos_weight", default = 1),
      ParamHelpers::makeLogicalLearnerParam(id = "refresh_leaf", default = TRUE),
      ParamHelpers::makeDiscreteLearnerParam(id = "feature_selector", default = "cyclic", values = c("cyclic", "shuffle", "random", "greedy", "thrifty")),
      ParamHelpers::makeIntegerLearnerParam(id = "top_k", default = 0, lower = 0),
      ParamHelpers::makeDiscreteLearnerParam(id = "predictor", default = "cpu_predictor", values = c("cpu_predictor", "gpu_predictor")),
      ParamHelpers::makeUntypedLearnerParam(id = "updater"), # Default depends on the selected booster
      ParamHelpers::makeNumericLearnerParam(id = "sketch_eps", default = 0.03, lower = 0, upper = 1),
      ParamHelpers::makeLogicalLearnerParam(id = "one_drop", default = FALSE, requires = quote(booster == "dart")),
      ParamHelpers::makeDiscreteLearnerParam(id = "tree_method", default = "auto", values = c("auto", "exact", "approx", "hist", "gpu_hist"), requires = quote(booster != "gblinear")),
      ParamHelpers::makeDiscreteLearnerParam(id = "grow_policy", default = "depthwise", values = c("depthwise", "lossguide"), requires = quote(tree_method == "hist")),
      ParamHelpers::makeIntegerLearnerParam(id = "max_leaves", default = 0L, lower = 0L, requires = quote(grow_policy == "lossguide")),
      ParamHelpers::makeIntegerLearnerParam(id = "max_bin", default = 256L, lower = 2L, requires = quote(tree_method == "hist")),
      ParamHelpers::makeUntypedLearnerParam(id = "callbacks", default = list(), tunable = FALSE)
    ),
    par.vals = list(nrounds = 1L, verbose = 0L),
    properties = c("numerics", "weights", "featimp", "missings"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` and `verbose` to `0` by default.",
    callees = "xgboost"
  )
}

#' @export
trainLearner.surv.xgboost = function(.learner, .task, .subset, .weights = NULL,  ...) {
  parlist = list(...)

  if (is.null(parlist$objective))
  {
    parlist$objective = "survival:cox"
		parlist$eval_metric = "cox-nloglik"
  }

  data = mlr::getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "surv")

#  info = mlr:::getFixDataInfo(data$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
#  data$data = as.matrix(mlr:::fixDataForLearner(data$data, info))
	time = data$target[, 1L]
  status = data$target[, 2L]
  survtime <- ifelse(status == 1, time, -time)
	parlist$data = xgboost::xgb.DMatrix(data = data.matrix(data$data), label = survtime)
                                                                                                                  
  if (!is.null(.weights))
    xgboost::setinfo(parlist$data, "weight", .weights)

  if (is.null(parlist$watchlist))
    parlist$watchlist = list(train = parlist$data)

  do.call(xgboost::xgb.train, parlist)
}

#' @export
predictLearner.surv.xgboost = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  res = mlr::predictLearner(m, newdata = data.matrix(.newdata), ...)
	return(res)
}

