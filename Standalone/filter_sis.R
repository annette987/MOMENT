#' Custom mlr filter for SIS (Sure Independence Screening)
#'
#' A custom mlr filter that uses SIS to select important features
#' This function registers the "sis.filter" filter to be used with
#' makeFilterWrapper and other mlr filter functions.
#'
#' \itemize{

#' }
#' @return Nothing, but "sis.filter" filter will be registered
#' @export

library(SIS)

mlr::makeFilter(
    name = "sis",
    desc = "Uses sure independence screening (sis) for feature selection",
    pkg = "SIS",
    supported.tasks = c("classif", "regr", "surv"),
    supported.features = c("numerics", "factors", "ordered"),
    fun = function(task, nselect, ...) {
      print('Starting SIS feature selection')
			desc = getTaskDesc(task)
			if (desc$type == "surv") {
				data = getTaskData(task, target.extra = FALSE)
				data[,"status"] = as.integer(data[,"status"])
				Y = Surv(data[,"timeToEvent"], data[,"status"])
				X = data[, !names(data) %in% c("timeToEvent", "status", "ID")]
			} else {
				data = getTaskData(task, target.extra = TRUE)
				Y = data$target
				X = data$data
			}

			sis_res = SIS::SIS(x = data.matrix(X), 
										y = Y,
										penalty = "lasso", 
										tune = "cv",
										nfolds = 5,
										varISIS = 'vanilla',
										standardize = FALSE)
#			print(sis_res$ix)
#			print(sis_res$sis.ix0)
			names(sis_res$coef.est) = names(X)[sis_res$ix]
			print(sis_res$coef.est)
      return(sis_res$coef.est)
    }
)
