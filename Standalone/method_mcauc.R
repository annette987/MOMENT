method.MCAUC <- function(nlopt_method = NULL, optim_method = "L-BFGS-B",
                       bounds = c(0, Inf), normalize = TRUE) {
  # Contributed by Erin LeDell
	# Modified by Annette Spooner for multi-class data
	#
  if (!is.null(nlopt_method) && !is.null(optim_method)) {
    stop("Please supply either a nlopt or optim method; one of these must be set to NULL.")
  }

  if (!is.null(optim_method)) {
    if (!(optim_method %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"))) {
      stop("supplied 'optim_method' value not supported")
    }
    out <- list(
      require = 'cvAUC',
      # computeCoef is a function that returns a list with two elements:
      # 1) coef: the weights (coefficients) for each algorithm
      # 2) cvRisk: the V-fold CV risk for each algorithm
      computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose,
                             errorsInLibrary = NULL,
                             ...) {
        .cvRisk_AUC <- function(par, Z, Y, folds = NULL) {
          # Calculate cv Risk, which is 1 - cvAUC (rank loss);
          # This is the general loss function that gets fed into optim as the "fn" argument
          # par is the weight/coef vector for ensemble in Super Learner.
          predictions <- crossprod(t(Z[, par != 0, drop = FALSE]), par[par != 0])
					print(predictions)
          # Now calculate cv risk (this is what we want to minimize)
          # Might change this to AUC only since we are currently not using folds arg...
          cvRisk <- 1 - cvAUC::cvAUC(predictions = predictions, labels = Y, folds = folds)$cvAUC
          return(cvRisk)
        }

        coef_init <- rep(1 / ncol(Z), ncol(Z))
        names(coef_init) <- libraryNames

        # Don't need this currently.
        #lower_bounds = rep(bounds[1], ncol(Z))
        #upper_bounds = rep(bounds[2], ncol(Z))

        # Any algorithms with NA cvRisk will be restricted to 0 coefficient.
        # Otherwise algorithms with NA risk and all NA predictions can still receive
        # a positive coefficient. This does not bode well for this optimization
        # algorithm but we will handle anyway.
        if (sum(errorsInLibrary) > 0) {
          if (verbose) {
            cat("Removing failed learners:",
                paste(libraryNames[errorsInLibrary], collapse = ", "), "\n")
          }
          # Setting upper_bounds to 0 causes optim() to error out.
          # But this part isn't actually necessary.
          #upper_bounds[errorsInLibrary] = 0

          # Also update initial coefficients so that NA learners are set to 0.
          coef_init <- rep(1 / sum(!errorsInLibrary), ncol(Z))
          coef_init[errorsInLibrary] = 0
        }

        # optim function selects the value for par that minimizes .cvRisk_AUC (ie. rank loss)
        res <- optim(par = coef_init,
                     fn = .cvRisk_AUC,
                     Z = Z,
                     Y = Y,
                     folds = NULL,
                     method = optim_method,
                     lower = bounds[1],
                     upper = bounds[2])
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

        auc <- apply(Z, 2, function(x) cvAUC::AUC(predictions = x, labels = Y))
        # If we update the getCoef function in SL to include 'folds' we can use the below auc instead
        # auc <- apply(Z, 2, function(x) cvAUC(x, labels=Y, folds=validRows)$cvAUC)
        cvRisk <- 1 - auc  # rank loss

        names(coef) <- libraryNames
        out <- list(cvRisk = cvRisk, coef = coef, optimizer = res)
        return(out)
      },
      # computePred is a function that takes the weights and the predicted values from each algorithm in the library and combines them based on the model to output the super learner predicted values
      computePred = function(predY, coef, control, ...) {
        if (sum(coef != 0) == 0) {
          stop("All metalearner coefficients are zero, cannot compute prediction.")
        }
        out <- crossprod(t(predY[, coef != 0, drop = F]), coef[coef != 0])
        return(out)
      }
    )
#  } else if (length(nlopt_method) > 0) {
  } else if (!is.null(nlopt_method)) {
    nlopt_global <- c("NLOPT_GN_DIRECT",
                      "NLOPT_GN_DIRECT_L",
                      "NLOPT_GN_DIRECT_L_RAND",
                      "NLOPT_GN_DIRECT_NOSCAL",
                      "NLOPT_GN_DIRECT_L_NOSCAL",
                      "NLOPT_GN_DIRECT_L_RAND_NOSCAL",
                      "NLOPT_GN_ORIG_DIRECT",
                      "NLOPT_GN_ORIG_DIRECT_L",
                      "NLOPT_GN_CRS2_LM",
                      "NLOPT_GN_ISRES")
    nlopt_local <- c("NLOPT_LN_PRAXIS",
                     "NLOPT_LN_COBYLA",
                     "NLOPT_LN_NEWUOA_BOUND",
                     "NLOPT_LN_NELDERMEAD",
                     "NLOPT_LN_SBPLX",
                     "NLOPT_LN_BOBYQA")
    #if (length(intersect(nlopt_method, c(nlopt_global, nlopt_local))) == 0) {
    if (!(nlopt_method %in% c(nlopt_global, nlopt_local))) {
      stop("supplied 'nlopt_method' value not supported")
    }
    out <- list(
      require = c('cvAUC', 'nloptr'),
      # computeCoef is a function that returns a list with two elements:
      # 1) coef: the weights (coefficients) for each algorithm
      # 2) cvRisk: the V-fold CV risk for each algorithm
      computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose, ...) {
        .cvRisk_AUC <- function(par, Z, Y){
          # Calculate cv Risk, which is 1-cvAUC (rank loss);
          # This is the general loss function that gets fed into optim as the "fn" argument
          # par is the weight/coef vector for ensemble in Super Learner
          predictions <- crossprod(t(Z), par)  #cv predicted SL values
          # Now calculate cv risk (this is what we want to minimize)
          cvRisk <- 1 - cvAUC::cvAUC(predictions = predictions, labels = Y, folds = NULL)$cvAUC
          return(cvRisk)
        }
        coef_init <- rep(1/ncol(Z), ncol(Z))
        names(coef_init) <- libraryNames
        # nloptr function selects the value for par that minimizes .cvRisk_AUC (ie. rank loss)
        res <- nloptr::nloptr(x0 = coef_init,
                      eval_f = .cvRisk_AUC,
                      lb = rep(bounds[1], ncol(Z)),
                      ub = rep(bounds[2], ncol(Z)),
                      #eval_g_ineq = .constraint_ineq,
                      #eval_g_eq = .constraint_eq,
                      opts = list(algorithm = nlopt_method, xtol_rel = 1e-08),
                      Z = Z,
                      Y = Y)
        if (res$status < 1 || res$status > 4) {
      warning(res$message)
    }
        coef <- res$solution
        if (anyNA(coef)) {
          warning("Some algorithms have weights of NA, setting to 0.")
          coef[is.na(coef)] <- 0
        }
        if (!sum(coef) > 0) warning("All algorithms have zero weight", call. = FALSE)
        if (normalize) coef <- coef/sum(coef)
        auc <- apply(Z, 2, function(x) cvAUC::AUC(predictions = x, labels = Y))
        ## If we update the getCoef function in SL to include 'folds' we can use the below auc instead
        ## auc <- apply(Z, 2, function(x) cvAUC(x, labels=Y, folds=validRows)$cvAUC)
        cvRisk <- 1 - auc  # rank loss
        names(coef) <- libraryNames
        out <- list(cvRisk = cvRisk, coef = coef, optimizer = res)
        return(out)
      },
      # computePred is a function that takes the weights and the predicted values from each algorithm in the library and combines them based on the model to output the super learner predicted values
      computePred = function(predY, coef, control, ...) {
        out <- crossprod(t(predY), coef)
        return(out)
      }
    )
  } else {
    stop("Please supply an nlopt or optim method.")
  }
  invisible(out)
}