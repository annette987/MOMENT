#' Custom mlr filter for randomForestSRC
#'
#' A custom mlr filter that uses randomForestSRC to select important features
#' This function registers the "randomForestSRC" filter to be used with
#' makeFilterWrapper and other mlr filter functions.
#'
#' @return Nothing, but "randomForestSRC" filter will be registered
#' @export


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
  })


# randomForestSRC_var.select ----------------

#' Filter \dQuote{randomForestSRC_var.select} uses the minimal depth variable
#' selection proposed by Ishwaran et al. (2010) (`method = "md"`) or a
#' variable hunting approach (`method = "vh"` or `method = "vh.vimp"`).
#' The minimal depth measure is the default.
#'
#' @rdname makeFilter
#' @name makeFilter

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
			print("No variables selected. Keeping top 2.")
			im$varselect[3:nrow(im$varselect), "depth"] = NA
		} else {
			im$varselect[setdiff(rownames(im$varselect), im$topvars), "depth"] = NA
		}
    setNames(im$varselect[, "depth"], rownames(im$varselect))
  })

