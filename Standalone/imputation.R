####################################################################################
# MICE IMPUTATION
# CPO to run imputation using MICE or KNN within the CV loop
# cpo.train creates the prediction matrix used by mice.
# cpo.retrafo applies the same prediction matrix to both the training and test sets
####################################################################################

library(mlrCPO)
library(VIM)

get_minpc = function(num_feats) {
		if (num_feats < 100)
			minpc = 0.2
		else if (num_feats < 1000)
			minpc = 0.5
		else
			minpc = 0.9
		print(paste0("minpc = ", minpc))
		return (minpc)
}

initMice = function(data) {
	print("In initMice")
	minpc = get_minpc(ncol(data))
	
	# Remove constant and collinear features
	ini = mice(data, maxit=0)
	print(ini$loggedEvents)
	outlist1 = as.character(ini$loggedEvents[, "out"])
#	print("Outlist1")
#	print(outlist1)

	# Check to see if there is any missing data
	# If not don't call flux, (just return the data as it is)
	if (any(is.na(data))) {	
		 print("NAs in data")
	 # Remove features with low outflux - limited predictive ability
		 fx = flux(data)
#		 print("Flux:")
#		 print(fx)
		 outlist2 <- row.names(fx)[fx$influx > 0 & fx$outflux < 0.5]
#		 print("Outlist2")
#		 print(outlist2)
		 if (length(outlist2) > 0 & length(outlist2) < ncol(data)) {
			 data2 <- data[, !names(data) %in% outlist2]
#			 print(dim(data2))
			 fx2 <- flux(data2)
			 outlist3 <- row.names(fx2)[!is.nan(fx2$outflux) & (fx2$outflux < 0.5)]								 
#			 print("Outlist3")
#				print(outlist3)
			} else {
				outlist3 = c()
			}
			outlist <- unique(c(outlist1, outlist2, outlist3))
#			outlist = outlist1
	} else {
		print("No NAs")
		outlist = outlist1
	}
	
	if (length(outlist) == length(colnames(data))) {
		print("Warning: trying to exclude all predictors")
		outlist = outlist1
	}
	print("Outlist")
	print(outlist)	

  # some final processing
	predmat = mice::quickpred(data, minpuc = minpc, mincor = minpc, exclude = outlist)
	return(predmat)
}


imputeMice = function(data, control) {
	print("In imputeMice")
	if (all(control == 0L)) {
		minpc = get_minpc(ncol(data))
		control = mice::quickpred(data, minpuc = minpc, mincor = minpc)
	}
	
	mice_success = TRUE
	imp_data = tryCatch({
			mice::mice(data, m=5, method = "cart", pred = control, print = FALSE, remove_collinear = FALSE)
#			mice::mice(data, m=5, method = "cart", pred = control, print = TRUE)
	}, 
	error = function(cond) {
			print(paste("imputeMice returned error: ", cond))
			mice_success = FALSE
		})
	if (mice_success) {
		imputed = mice::complete(imp_data)
	}

	if (!mice_success || any(is.na(imputed))) {
		print("Missing values after imputation - using KNN imputation instead!")
		imputed = imputeKNN(data)
	}
	return(imputed)
} 


imputeKNN = function(data) {
	imputed = tryCatch({
			VIM::kNN(data, k = 3, trace = FALSE)	
	}, 
	error = function(cond) {
			print(paste("VIM::kNN returned error: ", cond))
	})
	
	if (any(is.na(imputed))) {
		print("Missing values after imputation - using mean imputation instead!")
		mean_val <- colMeans(data, na.rm = TRUE)
		for(i in colnames(data))
			data[,i][is.na(data[,i])] <- mean_val[i]	
		imputed = data
	}
	return(imputed[colnames(data)])
}


imputeData = function(data, impute_method = "MICE", control = NULL) {
#	print(paste0("In imputeData: ", impute_method))
	if (ncol(data) == 0) {
		control = matrix(0, 1, 1)
	} else {
		if (any(is.na(data))) {
			print("Imputing")
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
#			print("Nothing to impute")
			control = matrix(0, ncol(data), ncol(data))
		}	
	}
	return(list("data" = data, "control" = list(control)))
}		


cpoImputeData = makeCPOExtendedTrafo("imputeData", # nolint
	pSS(impute_method = "MICE": character),
	dataformat = "df.features",
	properties.data = c("numerics", "factors", "ordered", "missings"),
	properties.adding = c("missings"),
	cpo.trafo = function(data, target, impute_method) {
#			print("cpoImputeData - cpo.trafo")
#			print(impute_method)
#			print(colnames(data))
			imputed = imputeData(data, impute_method, NULL)
			control = unlist(imputed$control)
			return(imputed$data)
	},
	cpo.retrafo = function(data, control, impute_method) {
#			print("cpoImputeData - cpo.retrafo")
#			print(impute_method)			
#			print(colnames(data))
			control = matrix(unlist(control), ncol= ncol(data), byrow=FALSE)
			imputed = imputeData(data, impute_method, control)
			return(imputed$data)
	}
)


makePreprocWrapperImpute = function(learner, impute_method = "MICE") {
#	print("In makePreprocWrapperImpute")
  trainfun = function(data, target, args = list(impute_method)) {
#			print("Imputing training data")
			
			# Identify numerical features
			cns = colnames(data)
			nums = setdiff(cns[sapply(data, is.numeric)], target)
#			print("Numeric columns")
#			print(nums)
			
			x = data[, nums, drop = FALSE]
			imputed_train = imputeData(x, unlist(args), NULL)
			
			# Recombine the data
			data = data[, setdiff(cns, nums), drop = FALSE]
			data = cbind(data, imputed_train$data)			
			return(list("data" = data, "control" = imputed_train$control))    ### Triple list for KNN?
  }
  predictfun = function(data, target, args, control) {
#			print("Imputing test data")
			
			# Identify numerical features
			cns = colnames(data)
			nums = cns[sapply(data, is.numeric)]
#			print("Numeric columns")
#			print(nums)

			
			# Extract numerical features from the data set and call scale
			x = data[, nums, drop = FALSE]
			imputed_test = imputeData(x, unlist(args), control[[1]])
			
			# Recombine the data
			data = data[, setdiff(cns, nums), drop = FALSE]
			data = cbind(data, imputed_test$data)
			return(data)
  }
  makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = makeParamSet(
			makeDiscreteParam("impute_method", values = c("MICE", "KNN"))
    ),
    par.vals = list(impute_method = impute_method)
  )
}
