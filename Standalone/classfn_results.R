classfn_results = function(res, unwrap = TRUE)
{
	class_names = c(get_class_names(res$pred), "all")
	names = list()
	scores = list()
	for (cls in class_names) {
		names[[cls]] = list()
		scores[[cls]] = list()
	}

	for (i in 1:length(res$models)) {
		imp_data = NULL
		mod = getLearnerModel(res$models[[i]], more.unwrap = unwrap)
		
		# Convert the custom importance data from each model into a uniform format -
		# a data.frame with one row per feature and one column per class
		if (is.null(mod)) {
			cat(paste("=======> model i =", i, "is null!!\n"))
		} else if (inherits(mod, "WrappedModel") && isFailureModel(mod)) {
			cat(paste("=======> model i =", i, "is a FailureModel!!!\n"))
			cat(getFailureModelMsg(mod))
		} else if (inherits(mod, "cv.glmnet")) {
			coef.min = coef(mod, s = mod$lambda.min)
			imp_data = as.data.frame(do.call(cbind, lapply(coef.min, as.matrix)))
			colnames(imp_data) = get_class_names(res$pred)
		} else if (inherits(mod, "rfsrc")) {
			imp_data = as.data.frame(mod$importance)
		} else if (inherits(mod, "ranger")) {
			# N.B. This one needs to use local.importance but mlr doesn't allow it!
			imp_data = as.data.frame(mod$variable.importance)
			colnames(imp_data) = c("all")
		} else if (inherits(mod, "xgb.Booster")) {
			imp = xgboost::xgb.importance(model = mod)
			if (mod$params$booster == BOOSTER_LINEAR) {
				imp_data = imp %>% 
										pivot_wider(names_from = "Class", 
																values_from = "Weight", 
																names_prefix = "Class")
				imp_data = as.data.frame(imp_data)
				colnames(imp_data) = c("Feature", get_class_names(res$pred))
			} else {
				imp_data = as.data.frame(imp)[,c("Feature", "Gain")]
				colnames(imp_data) = c("Feature", "all")
			}
			rownames(imp_data) = imp_data[, "Feature"]
		} else if (inherits(mod, "ksvm")) {
		}
		
		if (!is.null(imp_data)) {
			for (cls in class_names) {	
				if (cls %in% colnames(imp_data)) {
					class_df = imp_data[cls]
					active.min = which(as.matrix(class_df) != 0)
					if (length(active.min) > 1) {
						if (inherits(mod, "cv.glmnet"))
							active.min = active.min[2:length(active.min)]  #1st value is Intercept - ignore this
						scores[[cls]][[i]] = as.vector(class_df[active.min, cls])
						mat_names = as.vector(rownames(class_df)[active.min])
						if (length(names[[cls]]) < i) {
							names[[cls]][[i]] = mat_names
						} else {
							names[[cls]][[i]] = unique(c(names[[cls]][[i]], mat_names))
						}
					}
				}
			}
		}
	}		
	return(list("names" = names, "scores" = scores))
}