####################################################################################
# MICE IMPUTATION
# CPO to run imputation using MICE within the CV loop
# cpo.train creates the prediction matrix used by mice.
# cpo.retrafo applies the same prediction matrix to both the training and test sets
####################################################################################

library(mice)

get_minpc = function(num_feats) {
		if (num_feats < 100)
			minpc = 0.2
		else if (num_feats < 1000)
			minpc = 0.5
		else
			minpc = 0.8
#		print(paste0("minpc = ", minpc))
		return (minpc)
}

imputeMice = function(data) {
#		print("In imputeMice")
		minpc = get_minpc(ncol(data))
		
		# Remove constant and collinear features
		ini = mice(data, maxit=0)
		outlist1 = as.character(ini$loggedEvents[, "out"])

		# Check to see if there is any missing data
		# If not don't call flux, (just return the data as it is)
		if (any(is.na(data))) {	
#		 print("Missings")							 
		 # Remove features with low outflux - limited predictive ability
			 fx = flux(data)
			 outlist2 <- row.names(fx)[fx$outflux < 0.5]
			 data2 <- data[, !names(data) %in% outlist2]
			 fx2 <- flux(data2)
			 outlist3 <- row.names(fx2)[!is.nan(fx2$outflux) && fx2$outflux < 0.5]								 
			 outlist <- unique(c(outlist1, outlist2, outlist3))
		} else {
			print("NO MISSING DATA")
			outlist = outlist1
		}
#		print(outlist)
		if (length(outlist) == length(colnames(data))) {
			print("Trying to exlcude all predictors")
			outlist = outlist1
		}									

		predmat = mice::quickpred(data, minpuc = minpc, mincor = minpc, exclude = outlist)
		if (all(predmat == 0L)) {
			print("Predictor matrix is all zeroes - nothing to impute")
			imputed = data
		} else {
			 imp_data = tryCatch({
					mice::mice(data, m=5,  method="cart", pred=predmat, seed = 23109, print = FALSE)
			 }, 
			 error = function(cond) {
					print(paste("mice returned error: ", cond))
					imp_data = data
				})
			
			 imputed = mice::complete(imp_data)
		}
		return(imputed)
} 

	