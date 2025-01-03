#--------------------------------------------------------
# MODELS
# Extract the feature names and scores from the different model types
#--------------------------------------------------------

# Convert the custom importance data from each model into a uniform format -
# a data.frame with one row per feature and one column per class, plus an 'all' column
# The 'all' column may contain zeroes if it is not possible to get this data

getFeatImpScores = function(mod, class_names = NULL)
{	
	imp_data = NULL
	if (is.null(mod)) {
		cat(paste("=======> Model is null!!\n"))		
	} else if (inherits(mod, "WrappedModel") && isFailureModel(mod)) {
		cat(paste("=======> Model is a FailureModel!!!\n"))
		cat(mlr::getFailureModelMsg(mod))		
	} else if (inherits(mod, "coxph")) {
		pvalues = summary(mod)$coefficients[,5]
		imp_data = pvalues
	} else if (inherits(mod, "CoxBoost")) {
		ind = which(mod$coefficients[mod$stepno+1, ]!=0, arr.ind = T)
		imp_data = mod$coefficients[mod$stepno+1, ind]
		names(imp_data) = mod$xnames[ind]
		print(imp_data)		
	} else if (inherits(mod, "glmboost")) {
		coef.nonzero = coef(mod)
		imp_data  = coef.nonzero[2:length(coef.nonzero)]						# 1st column is Intercept - ignore this
	} else if (inherits(mod, "cv.glmnet")) {
		coef.min = coef(mod, s = mod$lambda.min)

		# For binary class problems coef.min is a single dgCMatrix
		# For multi-class problems coef.min is a list of dgCMatrix, one per class
		if (class(coef.min) == "dgCMatrix") {
			coef.min = coef.min[!rownames(coef.min) %in% c("(Intercept)"), ]
			active.min = which(as.matrix(coef.min) != 0)
			imp_data = coef.min[active.min]
		} else  {
			imp_data = as.data.frame(do.call(cbind, lapply(coef.min, as.matrix)))
			imp_data = imp_data[!rownames(imp_data) %in% c("(Intercept)"), ]
			colnames(imp_data) = class_names[1:ncol(imp_data)]
			imp_data['all'] = rep(0, nrow(imp_data))
		}
	} else if (inherits(mod, "rfsrc")) {
		imp_data = mod$importance
	} else if (inherits(mod, "randomForest")) {
		imp_data = mod$importance
	} else if (inherits(mod, "ranger")) {
		# N.B. This one needs to use local.importance but mlr doesn't allow it!
		ind = which(mod$variable.importance != 0)
		imp_data = mod$variable.importance[ind]
	} else if (inherits(mod, "xgb.Booster")) {
		if (mod$params$booster == BOOSTER_LINEAR) {
			imp_col = "Weight"
		} else {
			imp_col = "Gain"
		}
		if (mod$params$booster == BOOSTER_LINEAR) {
			imp = xgboost::xgb.importance(model = mod)
			if ('Class' %in% colnames(imp)) {
				imp_data = as.data.frame(imp %>% 
										tidyr::pivot_wider(names_from = "Class", 
																			 values_from = "Weight", 
																				names_prefix = "Class"))
				colnames(imp_data) = c("Feature", class_names[1:(length(class_names))])
			} else {
				imp_data = as.data.frame(imp)
				colnames(imp_data) = c('Feature', 'all')
			}
			
			rownames(imp_data) = imp_data$Feature
			imp_data$Feature = NULL
		} else {
			imp = xgboost::xgb.importance(model = mod)
			imp_data = as.data.frame(imp)[, c("Feature", imp_col)]
			colnames(imp_data) = c("Feature", "all")
			rownames(imp_data) = imp_data$Feature
			imp_data$Feature = NULL
		}
	} else if (inherits(mod, "svm")) {
			imp <- t(mod$coefs) %*% mod$SV                   		# weight vectors
			imp <- apply(imp, 2, function(v){sqrt(sum(v^2))})  	# weight
			imp <- sort(imp, decreasing = T)
			imp_data = data.frame("all" = imp)
	} else if (inherits(mod, "gbm")) {
			imp = gbm::summary.gbm(mod, method = gbm::relative.influence)
			colnames(imp) = c("var", "all")
			imp_data = data.frame('all' = imp[, "all"], row.names = imp[, "var"])
	} else if (inherits(mod, "rpart")) {
			imp_data = data.frame('all' = mod$variable.importance)
	} else if (inherits(mod, "naiveBayes")) {
	} else {
		print(paste0("Unknown model class: ", class(mod)))
	}

	return(imp_data)
}