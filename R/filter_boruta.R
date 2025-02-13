#' Custom mlr filter for Boruta
#'
#' A custom mlr filter that uses Boruta to select important features
#' This function registers the "boruta.filter" filter to be used with
#' makeFilterWrapper and other mlr filter functions.
#'
#' \itemize{
#'   \item target str; what is the target variable in the task object (default: 'Class')
#'   \item pValue float; see Boruta docs (default: 0.01)
#'   \item maxRuns int; see Boruta docs (default: 200)
#'   \item hostHistory bool; see Boruta docs (default: FALSE)
#'   \item withTentative bool; keep tentative features (default: TRUE)
#'   \item verbose bool; list features selected? (default: FALSE)
#'   \item mustKeep vector; features that cannot be filtered (default: NULL)
#'   \item threads int; number of threads to use for Boruta (default: 1)
#' }
#' @return Nothing, but "boruta.filter" filter will be registered
#' @export


mlr::makeFilter(
    name = "boruta",
    desc = "Uses boruta for feature selection",
    pkg = "Boruta",
    supported.tasks = c("classif", "regr", "surv"),
    supported.features = c("numerics", "factors", "ordered"),
    fun = function(task, nselect, get_imp = Boruta::getImpRfZ, pValue = 0.05, maxRuns = 200,
                   withTentative = FALSE, verbose = 0, mustKeep = NULL, ...) {

		 # boruta run
			desc = getTaskDesc(task)
			if (desc$type == "surv") {
				data = mlr::getTaskData(task, target.extra = FALSE)
				targets = mlr::getTaskTargetNames(task)
				Y = Surv(data[,targets[1]], data[,targets[2]])
				X = data[, !names(data) %in% c(targets, "ID")]
			} else {
				data = mlr::getTaskData(task, target.extra = TRUE)
				Y = data$target
				X = data$data
			}
	
      boruta_res = Boruta::Boruta(x = X, 
													y = Y,
													pValue = pValue,
													getImp = get_imp,
													doTrace = 0,
													task = task,
													nrounds = 200)
 																	
      # selected features
			to_keep = Boruta::getSelectedAttributes(boruta_res, withTentative = withTentative)
      if(!is.null(mustKeep)){
        to_keep = union(to_keep, mustKeep)
      }
			
      if (length(to_keep) == 0){
        warning('Boruta selected 0 features! Using 2 random features')
        to_keep = sample(colnames(X), 2)
      } else if (length(to_keep) == 1){
			  warning('Boruta selected only 1 feature! Choosing 1 more at random')
        to_keep = c(to_keep, sample(colnames(X[!colnames(X) %in% to_keep]), 1))
			}
			
      # feature importance (arbitrary)
      imp = rep(100, length(to_keep))
      names(imp) = to_keep
      return(imp)
    }
)


getImpSHAP<-function(x, y, task, ...){
	td = getTaskDesc(task)
  parlist = list(...)
  nc = length(td$class.levels)
  nlvls = length(td$class.levels)

  if (is.null(parlist$objective)) {
    parlist$objective = if (nlvls == 2L) "binary:logistic" else "multi:softprob"
  }

  # if we use softprob or softmax as objective we have to add the number of classes 'num_class'
  if (parlist$objective %in% c("multi:softprob", "multi:softmax")) {
    parlist$num_class = nc
  }

  label = match(as.character(y), td$class.levels) - 1

  # recode to 0:1 so that for the binary case the positive class translates to 1 (https://github.com/mlr-org/mlr3learners/issues/32)
  # task.data$target is guaranteed to have the factor levels in the right order
  label = nlvls - as.integer(y)
  parlist$data = xgboost::xgb.DMatrix(data = data.matrix(x), label = label)
  xgb_model = do.call(xgboost::xgb.train, parlist)
	 
  #A busing the fact that Boruta disallows attributes with names starting from "shadow"
	x$shadow.Boruta.decision<-y
	unified <- unify(xgb_model, data.matrix(x))
	treeshap1 <- treeshap::treeshap(unified,  x, verbose = 0)
	return(colMeans(abs(treeshap1$shaps)))
}

getImpRFSRC<-function(x, y, ntree=500, ...) {
	 if (inherits(y, "Surv")) {
			x$shadow.Boruta.timeToEvent <- y[, "time"]
			x$shadow.Boruta.status <- y[,"status"]
			if (is.logical(x["status"])) {
				x["status"] = as.numeric(x["status"])
			}
			btask = makeSurvTask(id = "boruta_rfsrc", data = x, target = c("timeToEvent", "status"))
			res = randomForestSRC::rfsrc(mlr::getTaskFormula(btask),
																	 data = x,
																	 importance="permute")
			return (res$importance)
	 } else {
			x$shadow.Boruta.decision <- y
			# NB - Make a task and pass the task. Classif or regression - how do I know which one?
			return (randomForestSRC::rfsrc( 
																		data = x, 
																		importance="TRUE", ...)$importance)
	}
}

