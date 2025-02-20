####################################################################################
# NORMALISATION
# CPO to run various normalisation methods within the CV loop
# Only apply normalisation to the numeric columns (non-boolean)
#
####################################################################################


is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

norm_minMax = function(x) {
	return((x - min(x, na.rm = TRUE)) /(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

normaliseData = function(data, method = "STAND", epsilon = 1e100) {
	if (ncol(data) == 0) {
		final = data
	} else {
		col_order = colnames(data)[1:ncol(data)]
		numeric_cols = unlist(lapply(data, function(x) {is.numeric(x) && !is.factor(x) && !all(x %in% c(0,1))}))
		
		if (length(numeric_cols) == 0) {
			final = data
		} else {
			# Use drop = FALSE in case numeric_cols is of length 1
			
			if (method == "MINMAX") {
					norm_dat = as.data.frame(apply(data[, numeric_cols, drop = FALSE], 2, norm_minMax))
			} else if (method == "LOGT") {
					if (all(data[, numeric_cols, drop = FALSE] > 0)) { 
						norm_dat = log(data[, numeric_cols, drop = FALSE] + 1)
					} else {
						norm_dat = data[, numeric_cols, drop = FALSE]
					}
			} else if (method == "STAND") {
					norm_dat = scale(as.matrix(data[, numeric_cols, drop = FALSE]), center = TRUE, scale = TRUE)
					norm_dat[!sapply(norm_dat, is.finite)] = 0
			} else if (method == "ZSCORE") {
					norm_dat = as.matrix(sapply(data[, numeric_cols, drop = FALSE], function(x) {x - mean(x, na.rm = TRUE) / (sd(x, na.rm = TRUE) + epsilon)}))
					norm_dat[!sapply(norm_dat, is.finite)] = 0
			} else if (method == "MEDIAN") {
					norm_dat = sapply(data[, numeric_cols, drop = FALSE], function(x) {x - median(x, na.rm = TRUE) / (mad(x, na.rm = TRUE) + epsilon)})
					norm_dat[is.nan(norm_dat)] = 0
			} else if (method == "CPM") {
					rs = rowSums(data[, numeric_cols, drop = FALSE], na.rm = TRUE)
					norm_dat = data[, numeric_cols, drop = FALSE] / rs * 1000000
					norm_dat[is.nan(norm_dat)] = 0
			} else if (method == "CPM_LOGT") {
					rs = rowSums(data[, numeric_cols, drop = FALSE], na.rm = TRUE)
					norm_dat = log((data[, numeric_cols, drop = FALSE] / rs * 1000000) + 1)
					norm_dat[is.nan(norm_dat)] = 0
			} else if (method == "QUANTILE") {
					norm_dat = as.data.frame(preprocessCore::normalize.quantiles(data.matrix(data[, numeric_cols, drop = FALSE])))
					colnames(norm_dat) = colnames(data[, numeric_cols, drop = FALSE])
			} else if (method == "NONE") {
					norm_dat = data[, numeric_cols, drop = FALSE]
			} else {
				norm_dat = data
			}

			final = as.data.frame(norm_dat)
			if (sum(!numeric_cols) > 0) {
				final = cbind(data[!numeric_cols, drop = FALSE], final)
			}
			final = final[, col_order]
		}
	}
	return(final)
}

cpoNormalise = mlrCPO::makeCPOExtendedTrafo("normalise",
  mlrCPO::pSS(method = "STAND": character),
  dataformat = "df.features",
	properties.data = c("numerics", "factors", "ordered", "missings"),
	properties.adding = "missings",
  cpo.trafo = function(data, target, method) {
			final = normaliseData(data, method)
			control = list(method = method)
			return(final)
  }, 
	cpo.retrafo = function(data, control, method) {
			final = normaliseData(data, method)
			return(final)
  })


makePreprocWrapperNormalise = function(learner, method = "STAND") {
  trainfun = function(data, target, args = list(method)) {
			norm_dat = normaliseData(data, unlist(args), NULL)
			return(list("data" = norm_dat, "control" = args))
  }
  predictfun = function(data, target, args, control) {
			norm_dat = normaliseData(data, unlist(args), NULL)
			return(norm_dat)
  }
  mlr::makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = ParamHelpers::makeParamSet(
				ParamHelpers::makeDiscreteParam("method", values = c("NONE", "STAND", "LOGT", "MINMAX", "CPM", "CPM_LOGT", "QUANTILE", "ZSCORE"))
    ),
    par.vals = list(method = method)
	)
}
