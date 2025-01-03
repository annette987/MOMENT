#' Custom mlr filter for VSURF
#'
#' A custom mlr filter that uses VSURF to select important features
#' This function registers the "vsurf.filter" filter to be used with
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
#' @return Nothing, but "vsurf.filter" filter will be registered
#' @export

library(VSURF)

mlr::makeFilter(
    name = "vsurf",
    desc = "Uses vsurf for feature selection",
    pkg = "VSURF",
    supported.tasks = c("classif", "regr"),
    supported.features = c("numerics", "factors", "ordered"),
    fun = function(task, nselect, nfor.thresh = 20, ...) {

			data = getTaskData(task, target.extra = TRUE)
			Y = data$target
			X = data$data	
      vsurf_out = VSURF(X, Y, nfor.thres = nfor.thresh, verbose = FALSE)
																	
      # selected features
      to_keep = vsurf_out$varselect.interp
#     to_keep = vsurf_out$varselect.pred
#			if (length(to_keep) < 2) {
#				to_keep = vsurf_out$varselect.thres
#			}
			
      # feature importance (arbitrary)
      imp = vsurf_out$imp.mean.dec[1:length(to_keep)]
      names(imp) = colnames(X)[to_keep]
      return(imp)
    }
)
