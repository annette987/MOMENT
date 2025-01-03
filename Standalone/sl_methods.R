# outline for SuperLearner methods
# these should always have class 'SL.method'
#
# The SuperLearner method is a coupling of the estimation algorithm for the algorithm weights (coefficients) and the model to combine the algorithms
#
# 2 parts need to be included:
#   1) compute coefficients
#   2) compute predictions


method.multi.AUC <- function(optim_method = "L-BFGS-B", bounds = c(0, Inf), normalize = TRUE) {
  print("In method.multi.AUC")
  print(optim_method)
  if (is.null(optim_method)) {
    stop("Please supply an optim method")
  }
	if (!(optim_method %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"))) {
		stop("supplied 'optim_method' value not supported")
	}
	
	out <- list(
		require = 'pROC',
		# computeCoef is a function that returns a list with two elements:
		# 1) coef: the weights (coefficients) for each algorithm
		# 2) cvRisk: the V-fold CV risk for each algorithm
		computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose,
													 errorsInLibrary = NULL,
													 ...) {
			.cvRisk_AUC <- function(par, Z, Y) {
				# Calculate cv Risk, which is 1 - cvAUC (rank loss);
				# This is the general loss function that gets fed into optim as the "fn" argument
				# par is the weight/coef vector for ensemble in Super Learner.
			  print("In .cvRisk_AUC")
			  predictions <- crossprod(t(Z[, par != 0, drop = FALSE]), par[par != 0])
				print(class(predictions))
				print(predictions)
				print(class(Y))
				print(Y)
				# Now calculate cv risk (this is what we want to minimize)
				cvRisk <- 1 - pROC::multiclass.roc(predictor = predictions, response = Y, levels = levels(as.factor(Y)))$auc
				print(paste0("cvRisk = ", cvRisk))
				return(cvRisk)
			}

			print("In computeCoef")
			print(Z)
			coef_init <- rep(1 / ncol(Z), ncol(Z))
			names(coef_init) <- libraryNames

			# Any algorithms with NA cvRisk will be restricted to 0 coefficient.
			# Otherwise algorithms with NA risk and all NA predictions can still receive
			# a positive coefficient. This does not bode well for this optimization
			# algorithm but we will handle anyway.
			if (sum(errorsInLibrary) > 0) {
				if (verbose) {
					cat("Removing failed learners:",
							paste(libraryNames[errorsInLibrary], collapse = ", "), "\n")
				}

				# Also update initial coefficients so that NA learners are set to 0.
				coef_init <- rep(1 / sum(!errorsInLibrary), ncol(Z))
				coef_init[errorsInLibrary] = 0
			}

			# optim function selects the value for par that minimizes .cvRisk_AUC (ie. rank loss)
			print("Calling optim")
			res <- optim(par = coef_init,
									 fn = .cvRisk_AUC,
									 Z = Z,
									 Y = Y,
									 method = optim_method,
									 lower = bounds[1],
									 upper = bounds[2])
			print(res)
			if (res$convergence != 0) {
				warning(paste("optim didn't converge when estimating the super learner coefficients, reason (see ?optim): ", res$convergence, " optim message: ", res$message))
			}
			coef <- res$par
			if (anyNA(coef)) {
				warning("Some algorithms have weights of NA, setting to 0.")
				coef[is.na(coef)] <- 0
			}
			if (!sum(coef) > 0) warning("All algorithms have zero weight", call. = FALSE)
			if (normalize) coef <- coef / sum(coef)

			print("Calling multiclass.roc again - don't know why")
			auc <- apply(Z, 2, function(x) pROC::multiclass.roc(predictor = x, response = Y))
			cvRisk <- 1 - auc  # rank loss
			print(cvRisk)

			names(coef) <- libraryNames
			out <- list(cvRisk = cvRisk, coef = coef, optimizer = res)
			return(out)
		},
		# computePred is a function that takes the weights and the predicted values from each algorithm in the library 
		# and combines them based on the model to output the super learner predicted values
		computePred = function(predY, coef, control, ...) {
			if (sum(coef != 0) == 0) {
				stop("All metalearner coefficients are zero, cannot compute prediction.")
			}
			out <- crossprod(t(predY[, coef != 0, drop = F]), coef[coef != 0])
			return(out)
		}
	)

  invisible(out)
}

