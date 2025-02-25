#' @title Filters: R6 class representing the custom mlr filters used by the models
#'
#' @description
#' Register the custom filters with mlr
#'
#' @details
#' Three custom filters are provided. 
#' Filter boruta uses the Boruta method to select important features.
#' Filter randomForestSRC uses randomForestSRC to select important features.
#' Filter \dQuote{randomForestSRC_var.select} uses the minimal depth variable
#' selection proposed by Ishwaran et al. (2010) (`method = "md"`) or a
#' variable hunting approach (`method = "vh"` or `method = "vh.vimp"`).
#' The minimal depth measure is the default.
#' The randomForestSRC and randomForestSRC_var.select were deleted from mlr.
#' The filters are registered so they can be used 
#' with makeFilterWrapper and other mlr filter functions.
#'
#' @name Filters
NULL

Filters = R6::R6Class("Filters", 
	public = list(
	
    #' @description 
		#' Create the custom filters.
    #' @return Nothing but the filters will be registered
		#' @export
		initialize = function() {
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

			mlr::makeFilter( # nolint
				name = "randomForestSRC_importance",
				desc = "Importance of random forests fitted in package 'randomForestSRC'. Importance is calculated using argument 'permute'.",
				pkg = "randomForestSRC",
				supported.tasks = c("classif", "regr", "surv"),
				supported.features = c("numerics", "factors", "ordered"),
				fun = function(task, nselect, method = "permute", ...) {
					assertChoice(method, choices = c("permute", "random", "anti", "permute.ensemble", "random.ensemble", "anti.ensemble"))
					im = randomForestSRC::rfsrc(getTaskFormula(task), data = getTaskData(task), proximity = FALSE,
						forest = FALSE, importance = method, ...)$importance
					if (inherits(task, "ClassifTask")) {
						ns = rownames(im)
						y = im[, "all"]
					} else {
						ns = names(im)
						y = unname(im)
					}
					setNames(y, ns)
				}
			)

			mlr::makeFilter( # nolint
				name = "randomForestSRC_var.select",
				desc = "Minimal depth of / variable hunting via method var.select on random forests fitted in package 'randomForestSRC'.",
				pkg = "randomForestSRC",
				supported.tasks = c("classif", "regr", "surv"),
				supported.features = c("numerics", "factors", "ordered"),
				fun = function(task, nselect, method = "md", conservative = "medium", ...) {
					# method "vh.imp" is not supported as it does return values to rank features on
					assert_choice(method, c("md", "vh"))
					im = randomForestSRC::var.select(getTaskFormula(task), getTaskData(task),
						method = method, verbose = FALSE, conservative = "medium", 
						...)
					
			#		im$varselect[, "depth"] = -1L * im$varselect[, "depth"]
					if (im$modelsize == 0) {
						warning("No variables selected. Keeping top 2.")
						im$varselect[3:nrow(im$varselect), "depth"] = NA
					} else {
						im$varselect[setdiff(rownames(im$varselect), im$topvars), "depth"] = NA
					}
					setNames(im$varselect[, "depth"], rownames(im$varselect))
				}
			)
		},

    #' @description 
		#' Method to obtain feature inportance scores using Shapley values.
		#' For use with the boruta custom filter
		#' @param x Data frame of predictors including shadows
		#' @param y The response vector
		#' @param task The mlr task
		#' @param ... Parameters passed to the underlying call
    #' @return Feature importance scores
		#' @export
		getImpSHAP = function(x, y, task, ...) {
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
		},

    #' @description 
		#' Method to obtain feature inportance scores using randomForestSRC.
		#' For use with the boruta custom filter
		#' @param x Data frame of predictors including shadows
		#' @param y The response vector
		#' @param ntree The number of trees to use in the random forest.
		#' @param ... Parameters passed to the underlying call
     #' @return Feature importance scores
		#' @export
		getImpRFSRC = function(x, y, ntree=500, ...) {
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
	)
)

