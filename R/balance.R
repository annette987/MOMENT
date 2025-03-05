#' @title Balance data using a pre-processing wrapper
#' @description 
#' Create a pre-processing wrapper to balance classes in the ML pipeline using the Synthetic Minority Oversampling Technique (SMOTE).
#' To implement SMOTE for a multi-class problem, balancing is performed on each class.
#' @param learner (character)\cr
#' The learner to which balancing should be added.
#' @param target_name (character)\cr
#' The name of teh target variable in the data.
#' @param bal_method (character)\cr
#' Method of balancing - \'SMOTE\' or \'OVERSAMPLE\'
#' @return A pre-processing wrapper. The function can be used in a pipeline to perform balancing.
#' @examples
#' lrn <- mlr::makeLearner(cl = "classif.gbm", id = "test", predict.type = "prob")
#' lrn <- makePreprocWrapperBalanceMC(lrn, "ID", "SMOTE")
#' @export
makePreprocWrapperBalanceMC = function(learner, target_name, bal_method) {
  trainfun = function(data, target, args = list(target_name, bal_method)) {
			# Convert any integer columns to numeric
			col_order = colnames(data)[1:ncol(data)]
			numeric_cols = unlist(lapply(data, is.numeric))
			if (sum(numeric_cols) > 0) {
				num_dat = as.data.frame(lapply(data[, numeric_cols, drop = FALSE], as.numeric))
				if (sum(!numeric_cols) > 0)
				{
					data = cbind(data[, !numeric_cols, drop = FALSE], num_dat)
				} else {
					data = num_dat
				}
			}
			data = data[, col_order]
			
			class_sizes = table(data[, target_name])
			max_class_size = max(class_sizes)
			min_class_size = min(class_sizes)
			max_class_name = names(which.max(class_sizes))
			max_ind = which(data[, target_name] == max_class_name)
			balanced_dat = data[max_ind, ]
			
			for (cls in names(class_sizes)) {
				if (class_sizes[cls] < max_class_size) {
					cls_ind = which(data[, target_name] == cls)
					binary_task = mlr::makeClassifTask(id = cls, 
																						 data = rbind(data[max_ind, ], data[cls_ind, ]), 
																						 target = target_name)
					rate = round(class_sizes[max_class_name] / class_sizes[cls], 1)
					nn = ifelse(min_class_size < 5, min_class_size, 5)
					
					if (bal_method == "SMOTE") {
							bal_task = mlr::smote(binary_task, rate = rate, nn = nn)
					} else if (bal_method == "OVERSAMPLE") {
							bal_task = mlr::oversample(binary_task, rate)
					}
					bal_dat = mlr::getTaskData(bal_task, target.extra = FALSE)
					bal_ind = which(bal_dat[, target_name] == cls)
					balanced_dat = rbind(balanced_dat, bal_dat[bal_ind, ])
				} else if (cls != max_class_name) {
					cls_ind = which(data[, target_name] == cls)
					balanced_dat = rbind(balanced_dat, data[cls_ind, ])
				}
			}			
			return(list("data" = balanced_dat, "control" = list(bal_method)))
  }
	
  predictfun = function(data, target, args, control) {
			return(data)
  }
	
  mlr::makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = ParamHelpers::makeParamSet(
							ParamHelpers::makeCharacterParam("target_name"),
							ParamHelpers::makeDiscreteParam("bal_method", values = c("SMOTE", "OVERSAMPLE"))
    ),
    par.vals = list(target_name = target_name, bal_method = bal_method)
  )
}
