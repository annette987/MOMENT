library(mlr)
library(mlrCPO)
library(checkmate)
library(BBmisc)
library(mice)
library(Boruta)
library(stats)
library(data.table)
library(dplyr)
library(tidyr)

source("const.R")
source("config.R")
source("imputation.R")
source("normalisation.R")
source("exploration.R")
source("roc_measures.R")
source("perf_measures.R")
source("filter_boruta.R")
source("filter_sis.R")
source("performance_classfn.R")
source("predictions_classfn.R")
source("stability.R")
source("features2.R")
source("learners.R")
source("feature_selectors.R")
source("data_helpers.R")


classfn_tests = function(config, dataset, result_file, cache_dir, active_learners = LRN_ALL, run_stability = FALSE, impute = TRUE, normalisation = "CPM", rpt = 0) 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print(Sys.time())
	print(paste0("Active Learners: 0x", as.hexmode(active_learners)))
	configureMlr(show.learner.output = TRUE, on.learner.error = 'warn', on.par.without.desc = 'warn')	
	
	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	tasks = create_tasks(data_dir, config, config$targetVar, big)
	learners = create_learners(config, base_learners, base_filters, "prob", cache_dir)
	resamp = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
	ri = makeResampleInstance(resamp, tasks[[1]])
	dat = getTaskData(tasks[[1]])
	classes = unique(dat[, config$targetVar])

	#-----------------------------------------------------------------------------------------------------------------------------
	# PRE-PROCESSING
	#-----------------------------------------------------------------------------------------------------------------------------
	
	#Remove the patient ID
	dataset = dataset[ , !(names(dataset) %in% c("ID"))]
	
	# Drop features that have > 50% missing values or <= 10% non-zero values
	count_na = as.data.frame(colSums(is.na(dataset)))
	drops_na = rownames(count_na)[which(count_na > nrow(dataset)/2)]
	count_nz = as.data.frame(colSums(dataset != 0, na.rm = TRUE) / nrow(dataset)*100)
	drops_nz = rownames(count_nz)[which(count_nz < 10)]
	drops = c(drops_na, drops_nz)
	dataset = as.data.frame(dataset[ , !(names(dataset) %in% drops)])
	print(paste0("Dropping ", length(drops), " features that have too many missing or zero values"))
	
	# Make the task
	print(paste0("Target var: ", target_var))
	classfn.task = makeClassifTask(id = task_id, data = dataset, target = target_var)
	ohe.task = classfn.task  %>>% cpoDummyEncode(reference.cat = TRUE)	# So that we know the  number of features after one-hot encoding and their names 

	# Count the number of features and report
	task.names = getTaskFeatureNames(ohe.task)
	print(paste0("Number of features: ", getTaskNFeats(classfn.task)))
	print(paste0("Number of features after encoding: ", getTaskNFeats(ohe.task)))
	
	num_features = getTaskNFeats(ohe.task)
	if (num_features == 0) {
		print(Sys.time())
		print("Returning ...")
		return(NULL)
	}
	
	#-----------------------------------------------------------------------------------------------------------------------------
	# DATA STRUCTURES TO COLLECT RESULTS
	#-----------------------------------------------------------------------------------------------------------------------------	
	perf   = Performance$new(prob.measures)	
	predns = Prediction$new(ohe.task)
	feats  = Features$new(list(ohe.task))
	stab   = Stability$new(list(ohe.task))
	tune   = Tuning$new()
	
	#--------------------------------------------------------
	# TUNING LIMITS
	#--------------------------------------------------------
	psqrt = round(sqrt(num_features))
	tune_lowerlim = 5
#	tune_upperlim = round(num_features/3)
	tune_upperlim = round(num_features/2)
	if (tune_upperlim < tune_lowerlim) {
		tune_upperlim = num_features
		tune_lowerlim = 0
	}
	print(paste0("Tuning limits: Lower - ", tune_lowerlim, " Upper - ", tune_upperlim))
#	tune_upperlim = min(round(num_features/2), MAX_FEATURES)
#	tune_seq = seq(from = tune_lowerlim, to = tune_upperlim, by = 5)
#	ctrl = makeTuneControlRandom(maxit = 5)
	ctrl = makeTuneControlRandom(maxit = 10)
#	ctrl = makeTuneControlGrid(resolution = 5L)
	   	
	#-----------------------------------------------------------------------------------------------------------------------------
	# RESAMPLING STRATEGIES
	#-----------------------------------------------------------------------------------------------------------------------------
	if (run_stability) NUM_ITERS = NUM_ITERS_STAB
	inner = makeResampleDesc("RepCV", reps = NUM_ITERS, folds = NUM_FOLDS, stratify = TRUE)	# Benchmarking: 5-fold CV repeated 5 times
	outer = makeResampleDesc("RepCV", reps = NUM_ITERS, folds = NUM_FOLDS, stratify = TRUE)	# Benchmarking: 5-fold CV repeated 5 times
	single = makeResampleDesc("CV", iters = NUM_ITERS, stratify = TRUE)											# Single run of 5-fold CV
	tuning = makeResampleDesc("CV", iters = NUM_ITERS_TUNE, stratify = TRUE)								# Tuning: 5-fold CV, no repeats

	model_results2 = function(task, learner)
	{
		imp = generateFeatureImportanceData(task = task, learner = learner, 
																				method = "permutation.importance", 
																				measure = "mmce", nmc=10)
		imp$res
	}

	model_results = function(res, model_id, result_file, unwrap = TRUE)
	{
		predns$save(res$pred)
		predns$write(result_file)
		
	#		class_names = c(get_class_names(res$pred), "all")
		class_names = get_class_names(res$pred)
		print(class_names)
		model_name = strsplit(model_id, "-")[[1]][1]
		names = list()
		scores = list()

		for (i in 1:length(res$models)) {
			mod = getLearnerModel(res$models[[i]], more.unwrap = TRUE)
			
			# Save tuning results, if any
			if (inherits(mod, "TuneModel")) {
				print(mod$learner.model$opt.result$x)
				tune$save(mod$learner.model$opt.result$x)
			}
			
			# Get feature importance scores
			imp_data = getFeatImpScores(mod, class_names)
			
			# Format names and scores into lists per class
			if (!is.null(imp_data)) {
				for (cls in class_names) {
					if (cls %in% colnames(imp_data)) {
						class_df = imp_data[cls]
						active.min = which(as.matrix(class_df) != 0)

						if (is.null(scores[[cls]])) {
							scores[[cls]] = list()
							names[[cls]] = list()
						}
						scores[[cls]][[i]] = class_df[[cls]]
						setNames(scores[[cls]][[i]], rownames(class_df))
						if (inherits(mod, "cv.glmnet")) {
							scores[[cls]][[i]] = scores[[cls]][[i]][2:length(scores[[cls]][[i]])]  #1st value is Intercept - ignore this
						}
						names[[cls]][[i]] = rownames(class_df)
					}
				}
			}
			
			perf$save(res, prob.measures)
			feats$save(model_name, names, scores, length(res$models))
			stab$save(model_name, names, getTaskNFeats(classfn.task))
		}

		perf$write(result_file)
		feats$write(result_file)
		stab$write(result_file)
		tune$write(result_file)
	}

	
	# Run one base method - no feature selection	
	run_learner = function(baselrn, resamp, result_file, impute, normalisation) 
	{
		writeLines(paste0("\nBASE METHOD: ", baselrn$name, " ", baselrn$class))
		mc_lrn = do.call(makeLearner, args = append(list("cl" = baselrn$class, "id" = baselrn$name, "predict.type" = "prob"), baselrn$args))
		if (!is.null(baselrn$tune_params)) {
			mc_lrn = makeTuneWrapper(mc_lrn, resampling = tuning, par.set = baselrn$tune_params, measures = prob.measures, control = ctrl, show.info = TRUE)
		}
		mc_lrn = cpoNormalise(normalisation) %>>% mc_lrn
		if (impute) {
			mc_lrn = cpoMice() %>>% mc_lrn
		}
		mc_lrn = cpoDummyEncode(reference.cat = TRUE)	%>>% mc_lrn	
		
### N.B. There is a bug in package mlrCPO that causes the name of the first cpo attached to a learner to be added twice.
###		See https://github.com/mlr-org/mlrCPO/issues/77
###		When this is fixed, the extra .scale (below) can be removed)
###		If tuning, then not adding two cpos together - they are separated by tuning.
#			if (!is.null(baselrn$tune_params))
#				model_id = paste0(model_id, '.mice') 
#			else
#				model_id = paste0(model_id, '.scale.mice') 
###	
#		print(paste0("Model name: ", model_id))

		extract_func = ifelse(is.null(baselrn$tune_params), getFilteredFeatures, getTuneResult)
		res = resample(learner = mc_lrn, task = classfn.task, measures = prob.measures, resampling = resamp, models = TRUE, extract = getFilteredFeatures)

		model_results(res, baselrn$name, result_file, TRUE)
		rmc = ROCMultiClass$new()
		rmc$calculate(baselrn$name, res$pred)
		rmc$write(result_file)
		rmc$plot(baselrn$name, baselrn$name, result_file)
	}


	#------------------------------------------------------------------------------------------------------------------------------
	# FEATURE SELECTION - FILTER METHODS
	#------------------------------------------------------------------------------------------------------------------------------
	params_tuning = makeParamSet(makeIntegerParam("fw.abs", lower = tune_lowerlim, upper = tune_upperlim))
	params_untuned = makeParamSet(makeDiscreteParam("fw.perc", values = c(1.0)))
	
	#Filters
	base_filters = list(
		"boruta" = list("method" = "boruta",
										 "code" = LRN_FS_BORUTA,
										 "prefix" = "Boruta",
										 "args" = list(get_imp = getImpRfZ, pValue = 0.05, maxRuns = 200, withTentative = TRUE, verbose = 1, mustKeep = NULL)),
		"lefse"  = list("method" = "lefse",
										 "code" = LRN_FS_LEFSE,
										 "prefix" = "LeFSe",
										 "args" = list()),
		"ranger_permutation" = list("method" = "ranger_permutation",
									"code" = LRN_FS_RF_PERMUTE,
									"prefix" = "Ranger",
									"args" = list(num.trees = 1000, splitrule = "gini")),
		"randomForestSRC_importance" = list("method" = "randomForestSRC_importance",
										  "code" = LRN_FS_RF_VARIMP,
										  "prefix" = "RF_VarImp",
											"args" = list(ntree = 1000, nsplit = 10, mtry = psqrt, nodesize = 3)),
		"randomForestSRC_mindepth" = list("method" = "randomForestSRC_var.select",
									"code" = LRN_FS_RF_MINDEPTH,
									"prefix" = "RF_MinDepth",
									"args" = list(metho = "md", ntree = 1000, nsplit = 10, nodesize = 3)),
		"sis" = list("method" = "sis",
										 "code" = LRN_FS_SIS,
										 "prefix" = "SIS",
										 "args" = list(family="gaussian", penalty = "lasso", tune = "cv", nfolds = 2, varISIS = 'vanilla', standardize = FALSE))
	)

	#Run a filter on a base learner
	#Base methods should be tuned as well. Can I use above values?
	run_filter = function(baselrn, basefilt, resamp, result_file) 
	{
#		set.seed(24601, "L'Ecuyer")	
		model_name = paste(basefilt$prefix, baselrn$name, sep = "_")
		lrn = do.call(makeLearner, args = append(list("cl" = baselrn$class, "id" = model_name, "predict.type" = "prob"), baselrn$args))

		if (!(basefilt$method %in% c("randomForestSRC_mindepth", "boruta", "sis"))) {
			filter_args = list("learner" = lrn, "fw.method" = basefilt$method, fw.perc = 0.1, "cache" = cache_dir)
			filt = do.call(makeFilterWrapper, args = c(filter_args, basefilt$args))
#			filt = makeTuneWrapper(filt, resampling = tuning, par.set = params_tuning, measures = prob.measures, control = ctrl, show.info = TRUE)
		} else {
			filter_args = list("learner" = lrn, "fw.method" = basefilt$method, "fw.perc" = 1.0, "cache" = cache_dir)
			filt = do.call(makeFilterWrapper, args = c(filter_args, basefilt$args))
		}
		
		filt = cpoNormalise(normalisation) %>>% filt
		if (impute) {
			filt = cpoMice() %>>% filt
		}
		filt = cpoDummyEncode(reference.cat = TRUE)	%>>% filt	

		writeLines(paste("\n\nFILTER: ", basefilt$method))
		writeLines(paste0("Model name: ", model_name))
		res = resample(learner = filt, task = classfn.task, measures = prob.measures, resampling = resamp, models = TRUE, extract = getFilteredFeatures)							
		model_results(res, model_name, result_file, TRUE)
		rmc = ROCMultiClass$new()
		rmc$calculate(model_name, res$pred)
		rmc$write(result_file)
		rmc$plot(model_name, model_name, result_file)
	}
	
	#------------------------------------------------------------------------------------------------------------------------------
	# RUN THE CLASSIFICATION TESTS ON KATANA
	#------------------------------------------------------------------------------------------------------------------------------	
	
	rs_strategy = if (rpt > 0) single else outer
	if (substring(Sys.info()['nodename'], 1, 1) == 'k' || is.null(active_learners)) {
		pbs_index = as.integer(Sys.getenv("PBS_ARRAY_INDEX"))
		print(paste0("pbs_index = ", pbs_index))
		
		datalist = binarise(classfn_type, dataset, target_var)
		for (data in datalist) {
			if (pbs_index <= NUM_ALGORITHMS) {
				run_learner(base_learners[[pbs_index]], rs_strategy, result_file, impute, normalisation)
			} else {
				model_index = ((pbs_index-1) %% NUM_ALGORITHMS) + 1
				fs_index = ((pbs_index-1) %/% NUM_ALGORITHMS)
				print(paste0("model_index = ", model_index))
				print(paste0("fs_index = ", fs_index))
				
				if (fs_index <= NUM_FEATSEL) {
					run_filter(base_learners[[model_index]], base_filters[[fs_index]], rs_strategy, result_file)
				}
			}
		}
	} 
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}