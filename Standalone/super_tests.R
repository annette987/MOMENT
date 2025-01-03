library(mlr)
library(mice)
library(Boruta)
library(readxl)
library(SuperLearner)
library(ggplot2)
library(ROCR)


getArgs = function(...) return(list(...))

super_tests = function(config, res_index, cache_dir, big = FALSE, ovr_class = NULL, subset = NULL, balance = FALSE) 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print(Sys.time())
	configureMlr(show.learner.output = TRUE, on.learner.error = 'warn', on.par.without.desc = 'warn')	
	
	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	tasks = create_tasks(data_dir, config, config$targetVar, ovr_class, big)
	prob_method = ifelse(decision == "prob", "prob", "response")
	learners = create_learners(config, base_learners, base_filters, prob_method, balance, cache_dir)
	if (!is.null(subset)) {
		tasks = tasks[subset]
		learners = learners[subset]
	}

	resamp = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
	ri_outer = makeResampleInstance(resamp, tasks[[1]])
	dataset = getTaskData(tasks[[1]])
	classes = unique(dataset[, config$targetVar])

	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------

	coeffs  = data.frame(stringsAsFactors=FALSE)
	cvrisk  = data.frame(stringsAsFactors=FALSE)
	aucvals = data.frame(stringsAsFactors=FALSE)
	sl_predictions = data.frame(stringsAsFactors=FALSE)
	lib_predictions = data.frame(stringsAsFactors=FALSE)
	
	reps = ri_outer$desc$iters
	vr = ri_outer$test.inds
	names(vr) = 1:reps
	ctrl = list(V = reps, validRows = vr)
	
	#--------------------------------------------------------------------------
	# CREATE LIBRARY of LEARNERS
	#--------------------------------------------------------------------------
	sl_learners = list("SL.randomForest", "SL.ranger", "SL.gbm", "SL.xgboost")

#	sl_learners = list()	
#	for (i in 1:nrow(learners)) {
#		print(paste0("Learner: ", learners[i,]$Learner))
#		if (length(learners[i,]$Params) > 0) {
#			pars = eval(parse(text=sprintf("getArgs(%s)", learners[i,]$Params)))
#		}
#		lrn = create.Learner(learners[i,]$Learner, params = list(pars))
#		if (length(learners[i,]$Screening) > 0 && !is.na(learners[i,]$Screening)) {
#				sl_learners[[i]] = c(lrn$names, learners[i,]$Screening)
#		} else {
#			sl_learners[[i]] = lrn$names
#		}
#	}	
#	print(sl_learners)

	#-----------------------------------------------------------------------------------------------------------------------------
	# RUN SUPER LEARNER USING ONE VS REST STRATEGY
	#-----------------------------------------------------------------------------------------------------------------------------

	for (i in 1:reps) {
		class_names = unique(dataset[, config$targetVar])
		for (cls in class_names) {
			print(cls)
			
			# Convert target variable to One-vs-all approach
			dataset_ovr = dataset
			z = as.factor(dataset_ovr[, config$targetVar])
			levels(z)[levels(z) != cls] = "REST"

			Y = as.numeric(z) - 1
			X = dataset_ovr[ , !(names(dataset_ovr) %in% config$targetVar)]
			
			# Create training and test sets
			X_train = X[ri_outer$train.inds[[i]], ]
			Y_train = Y[ri_outer$train.inds[[i]]]
			X_test = X[ri_outer$test.inds[[i]], ]
			Y_test = Y[ri_outer$test.inds[[i]]]
			
			# Impute missing data using mice in training and test sets
			control = matrix(0, 4, 4)
			X_train = imputeMice(X_train, control)
			X_test = imputeMice(X_test, control)
#			print(paste0("Missing values in X_train: ", any(is.na(X_train))))
#			print(paste0("Missing values in X_test: ", any(is.na(X_train))))

			#############################
			#What about normalisation and other pre-proc steps???
			##############################
			
			# Run Superlearner
			res = SuperLearner(Y = Y_train, X = X_train, 
														SL.library = sl_learners,
														cvControl = list(V = 5), 
														method = "method.AUC", 
														family = binomial(),
														verbose = FALSE)
			print(res)
		
			coeffs = rbind(coeffs, c(cls, res$coef), stringsAsFactors=FALSE)
			cvrisk = rbind(cvrisk, c(cls, res$cvRisk), stringsAsFactors=FALSE)
			sl_predictions = rbind(sl_predictions, c(cls, res$SL.predict), stringsAsFactors=FALSE)
			lib_predictions = rbind(lib_predictions, c(cls, res$library.predict), stringsAsFactors=FALSE)			
			
			pred = predict(res, X_test, onlySL = TRUE)
			pred_rocr = ROCR::prediction(pred$pred, Y_test)
			auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
			aucvals[i, cls] = auc
		}
	}
	colnames(coeffs) = c("Class", sl_learners)
	colnames(cvrisk) = c("Class", sl_learners)
	cvrisk <- cbind(cvrisk[,1], 1 - as.data.frame(sapply(cvrisk[,-1], as.numeric)))
	print("Coefficients:")
	print(coeffs)
	print("CVRisk:")
	print(cvrisk)
	print("AUC values")
	print(aucvals)
  return(list("coeffs" = coeffs, "cvrisk" = cvrisk, "auc" = aucvals, "slpred" = sl_predictions, "libpred" = lib_predictions))
}
