#' @title Imputation methods for use with multi-modal modelling
#'
#' @description
#' Provides two imputation methods for adding imputation to the pre-processing pipeline: Mice and KNN.
#'
#' @details
#' Two methods are provided for adding imputation to the machine learning pipeline.
#' One uses mlrCPO to create a new Composable Preprocessing Operator (CPO) for imputation.
#' The other creates a preprocessing wrapper that can be added to a learner.
#'
#' @name Imputation
NULL

#' @noRd
get_minpc = function(num_feats) {
		if (num_feats < 100)
			minpc = 0.2
		else if (num_feats < 1000)
			minpc = 0.5
		else
			minpc = 0.9
		return (minpc)
}


#' @description 
#' Perform initialisation to set up the MICE method.
#' @param data (data.frame)\cr
#' The data with missing values.
#' @return A control object to be used in imputation.
#' @noRd
initMice = function(data) {
	minpc = get_minpc(ncol(data))
	
	# Remove constant and collinear features
	ini = mice::mice(data, maxit=0)
	outlist1 = as.character(ini$loggedEvents[, "out"])

	# Check to see if there is any missing data
	# If not don't call flux, (just return the data as it is)
	if (any(is.na(data))) {	
	 # Remove features with low outflux - limited predictive ability
		 fx = mice::flux(data)
		 outlist2 <- row.names(fx)[fx$influx > 0 & fx$outflux < 0.5]
		 if (length(outlist2) > 0 & length(outlist2) < ncol(data)) {
			 data2 <- data[, !names(data) %in% outlist2]
			 fx2 <- mice::flux(data2)
			 outlist3 <- row.names(fx2)[!is.nan(fx2$outflux) & (fx2$outflux < 0.5)]								 
			} else {
				outlist3 = c()
			}
			outlist <- unique(c(outlist1, outlist2, outlist3))
	} else {
		outlist = outlist1
	}
	
	if (length(outlist) == length(colnames(data))) {
		warning("Trying to exclude all predictors")
		outlist = outlist1
	}

  # some final processing
	predmat = mice::quickpred(data, minpuc = minpc, mincor = minpc, exclude = outlist)
	return(predmat)
}


#' @description 
#' Perform imputation using Multiple Imputation by Chained Equiations (MICE).
#' If MICE fails, KNN is used instead.
#' @param data (data.frame)\cr
#' The data with missing values.
#' @param control (object)\cr
#' The control object, which is used to ensure that the same actions are applied to both training and test data.
#' @return The data with missing values imputed .
#' @noRd
imputeMice = function(data, control) {
	if (!requireNamespace("mice", quietly = TRUE)) {
		stop("Package \'mice\' must be installed to perform mice imputation")
	}
	if (all(control == 0L)) {
		minpc = get_minpc(ncol(data))
		control = mice::quickpred(data, minpuc = minpc, mincor = minpc)
	}
	
	imputed = tryCatch({
			imp_data = mice::mice(data, m=5, method = "cart", pred = control, print = FALSE, remove_collinear = FALSE)
			return(mice::complete(imp_data))
	}, 
	error = function(cond) {
			warning(paste("imputeMice returned error: ", cond))
			warning("Using KNN imputation instead")
			return(imputeKNN(data))
		}
	)

	if (any(is.na(imputed))) {
		warning("Missing values after imputation")
		imputed = imputeKNN(data)
	}
	return(imputed)
} 


#' @description 
#' Perform imputation using KNN.
#' If KNN fails, mean value imputation is used instead.
#' @param data (data.frame)\cr
#' The data with missing values.
#' @return The data with missing values imputed .
#' @noRd
imputeKNN = function(data) {
	if (!requireNamespace("VIM", quietly = TRUE)) {
		stop("Package \'VIM\' must be installed to performmice KNN")
	}
	imputed = tryCatch({
			VIM::kNN(data, k = 3, trace = FALSE)	
	}, 
	error = function(cond) {
			warning(paste("VIM::kNN returned error: ", cond))
			warning("Using mean value imputation instead")
			mean_val <- colMeans(data, na.rm = TRUE)
			for(i in 1:length(colnames(data))) {
				data[,i][is.na(data[,i])] <- mean_val[i]
			}
			if (any(is.na(data))) {
				warning("Still NAs after mean imputation")
			}
			return(data)
	})
	
	if (any(is.na(imputed))) {
		warning("Missing values after imputation!")
	}
	return(imputed[colnames(data)])
}


#' @description 
#' Perform imputation using the requested method.
#' @param data (data.frame)\cr
#' The data with missing values.
#' @param impute_method (character)\cr
#' Method of imputation - \'MICE\' or \'KNN\'
#' @param control (object)\cr
#' The control object, which is used to ensure that the same actions are applied to both training and test data.
#' @return A list containing the data with missing values imputed and the control object.
#' @noRd
imputeData = function(data, impute_method = "MICE", control = NULL) {
	if (ncol(data) == 0) {
		control = matrix(0, 1, 1)
	} else {
		if (any(is.na(data))) {
			if (impute_method == "MICE") {
				if (is.null(control)) {
					control = initMice(data)
				}
				data = imputeMice(data, as.matrix(control))
			} else if (impute_method == "KNN") {
				control = matrix(0, ncol(data), ncol(data))
				data = imputeKNN(data)
			}
		} else {
			control = matrix(0, ncol(data), ncol(data))
		}	
	}
	return(list("data" = data, "control" = list(control)))
}		


#' @description 
#' Create a pre-processing object to perform imputation in the ML pipeline.
#' @param impute_method (character)\cr
#' Method of imputation - \'MICE\' or \'KNN\'
#' @examples
#' lrn <- mlr::makeLearner(cl = "classif.gbm", id = "test", predict.type = "prob")
#' lrn <- cpoImputeData("MICE") %>>% lrn
#' @return  A pre-processing object that  can be used in a pipeline to perform imputation.
#' @export
cpoImputeData = mlrCPO::makeCPOExtendedTrafo("imputeData", 
	mlrCPO::pSS(impute_method = "MICE": character),
	dataformat = "df.features",
	properties.data = c("numerics", "factors", "ordered", "missings"),
	properties.adding = c("missings"),
	cpo.trafo = function(data, target, impute_method) {
			imputed = imputeData(data, impute_method, NULL)
			control = unlist(imputed$control)
			return(imputed$data)
	},
	cpo.retrafo = function(data, control, impute_method) {
			control = matrix(unlist(control), ncol= ncol(data), byrow=FALSE)
			imputed = imputeData(data, impute_method, control)
			return(imputed$data)
	}
)


#' @description 
#' Create a pre-processing wrapper to perform imputation in the ML pipeline.
#' @param learner (character)\cr
#' The learner to which imputation should be added.
#' @param impute_method (character)\cr
#' Method of imputation - \'MICE\' or \'KNN\'
#' @return A pre-processing wrapper. The function can be used in a pipeline to perform imputation.
#' @examples
#' lrn <- mlr::makeLearner(cl = "classif.gbm", id = "test", predict.type = "prob")
#' lrn <- makePreprocWrapperImpute(lrn, "KNN")
#' @export
makePreprocWrapperImpute = function(learner, impute_method = "MICE") {
  trainfun = function(data, target, args = list(impute_method)) {			
			# Identify numerical features
			cns = colnames(data)
			nums = setdiff(cns[sapply(data, is.numeric)], target)
			
			x = data[, nums, drop = FALSE]
			imputed_train = imputeData(x, unlist(args), NULL)
			
			# Recombine the data
			data = data[, setdiff(cns, nums), drop = FALSE]
			data = cbind(data, imputed_train$data)			
			return(list("data" = data, "control" = imputed_train$control))
  }
  predictfun = function(data, target, args, control) {
			# Identify numerical features
			cns = colnames(data)
			nums = cns[sapply(data, is.numeric)]
			
			# Extract numerical features from the data set and call scale
			x = data[, nums, drop = FALSE]
			imputed_test = imputeData(x, unlist(args), control[[1]])
			
			# Recombine the data
			data = data[, setdiff(cns, nums), drop = FALSE]
			data = cbind(data, imputed_test$data)
			return(data)
  }
  mlr::makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = ParamHelpers::makeParamSet(
				ParamHelpers::makeDiscreteParam("impute_method", values = c("MICE", "KNN"))
    ),
    par.vals = list(impute_method = impute_method)
  )
}
