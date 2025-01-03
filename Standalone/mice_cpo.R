####################################################################################
# MICE IMPUTATION
# CPO to run imputation using MICE within the CV loop
# cpo.train creates the prediction matrix used by mice.
# cpo.retrao applies the same prediction matrix to both the training and test sets
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
		return (minpc)
}

cpoMice = makeCPOExtendedTrafo("mice", # nolint
						 dataformat = "df.features",
						 properties.data = c("numerics", "factors", "ordered", "missings"),
						 properties.adding = c("missings"),
						 cpo.trafo = function(data, target) {
							print("In mice cpo.trafo")
							minpc = get_minpc(ncol(data))
							
							# Remove constant and collinear features
							ini = mice(data, maxit=0)
							outlist1 = as.character(ini$loggedEvents[, "out"])

							# Check to see if there is any missing data
							# If not don't call flux, (just return the data as it is)
							if (any(is.na(data))) {	
							 # Remove features with low outflux - limited predictive ability
								 fx = flux(data)
								 outlist2 <- row.names(fx)[fx$outflux < 0.5]
								 data2 <- data[, !names(data) %in% outlist2]
								 fx2 <- flux(data2)
								 outlist3 <- row.names(fx2)[!is.nan(fx2$outflux) && fx2$outflux < 0.5]								 
								 outlist <- unique(c(outlist1, outlist2, outlist3))
							} else {
								outlist = outlist1
							}
							print(paste0("Length of outlist = ", length(outlist)))
							if (length(outlist) == length(colnames(data))) {
								print("Trying to exclude all predictors")
								outlist = outlist1
							}									

							predmat = mice::quickpred(data, minpuc = minpc, mincor = minpc, exclude = outlist)
							if (all(predmat == 0L)) {
								print("Predictor matrix is all zeroes - nothing to impute")
								imputed = data
							} else {
								 imp_data = tryCatch({
										mice::mice(data, m=5,  method="cart", pred=predmat, seed = 23109, print = FALSE, remove_collinear = FALSE)
								 }, 
								 error = function(cond) {
										print(paste("trafo mice returned error: ", cond))
										imp_data = data
									})

								 imputed = mice::complete(imp_data)
							}
							control = as.matrix(predmat)
							print(paste0("Any missing: ", any(is.na(imputed))))
							print(which(is.na(imputed)))
							return(imputed)
						}, 
						cpo.retrafo = function(data, control) {
								print("In mice cpo.retrafo")
								if (!any(is.na(data))) {
									 # No missing data - no need to do any imputation
									 print("No missing data")
									 reimputed = data
								} else {
									if (all(control == 0L)) {
										print("Predictor matrix is all zeroes")
										minpc = get_minpc(ncol(data))
										control = mice::quickpred(data, minpuc = minpc, mincor = minpc)
									}
									
									reimp_data = tryCatch({
											mice::mice(data, m=5, method="cart", pred=control, print = FALSE, remove_collinear = FALSE)
									}, 
									error = function(cond) {
											print(paste("retrafo mice returned error: ", cond))
											reimp_data = data	
									})
									reimputed = mice::complete(reimp_data)
									print(paste0("Any missing: ", any(is.na(reimputed))))
									print(which(is.na(reimputed)))
								}
							 return(reimputed)
						 }
)


cpoKNN = makeCPOExtendedTrafo("knn", # nolint
						 dataformat = "df.features",
						 properties.data = c("numerics", "factors", "ordered", "missings"),
						 properties.adding = "missings",
						 cpo.trafo = function(data, target) { 
							
							if (!any(is.na(data))) {
									# No missing data - no need to do any imputation
									print("No missing data")
									imputed = data
							} else {
								imputed = tryCatch({
										VIM::kNN(data, k = 5)
								 }, 
								 error = function(cond) {
										print(paste("trafo kNN returned error: ", cond))
										imputed = data
									})
							}

							control = NULL
							return(imputed[colnames(data)])
						}, 
						cpo.retrafo = function(data, control) {
								if (!any(is.na(data))) {
									 # No missing data - no need to do any imputation
									 print("No missing data")
									 reimputed = data
								} else {
									reimputed = tryCatch({
											VIM::kNN(data, k = 5)
									 }, 
									 error = function(cond) {
											print(paste("trafo kNN returned error: ", cond))
											reimputed = data
										})
								}
								return(reimputed[colnames(data)])
						 }
)



# Use this function if you want to run mice without using mlrCPO
imputeMice = function(data) {
		minpc = get_minpc(ncol(data))
		
		# Remove constant and collinear features
		ini = mice(data, maxit=0)
		outlist1 = as.character(ini$loggedEvents[, "out"])

		# Check to see if there is any missing data
		# If not don't call flux, (just return the data as it is)
		if (any(is.na(data))) {	
		 # Remove features with low outflux - limited predictive ability
			 fx = flux(data)
			 outlist2 <- row.names(fx)[fx$outflux < 0.5]
			 data2 <- data[, !names(data) %in% outlist2]
			 fx2 <- flux(data2)
			 outlist3 <- row.names(fx2)[!is.nan(fx2$outflux) && fx2$outflux < 0.5]								 
			 outlist <- unique(c(outlist1, outlist2, outlist3))
		} else {
			outlist = outlist1
		}

		if (length(outlist) == length(colnames(data))) {
			print("Trying to exclude all predictors")
			outlist = outlist1
		}									

		predmat = mice::quickpred(data, minpuc = minpc, mincor = minpc, exclude = outlist)
		if (all(predmat == 0L)) {
#			print("Predictor matrix is all zeroes - nothing to impute")
			imputed = data
		} else {
			 imp_data = tryCatch({
					mice::mice(data, m=5,  method="cart", pred=predmat, seed = 23109, print = FALSE, remove_collinear = FALSE)
			 }, 
			 error = function(cond) {
					print(paste("mice returned error: ", cond))
					imp_data = data
				})
			
			 imputed = mice::complete(imp_data)
		}
		return(imputed)
} 

	