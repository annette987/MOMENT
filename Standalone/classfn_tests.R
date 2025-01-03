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
library(VSURF)

source("const.R")
source("config.R")
source("imputation.R")
source("normalisation.R")
source("exploration.R")
source("roc_measures.R")
source("perf_measures.R")
source("filter_boruta.R")
source("filter_sis.R")
source("filter_vsurf.R")
source("filter_rfsrc.R")
source("RLearner_classif_imputedRF.R")
source("RLearner_classif_ggbm.R")
source("performance_classfn.R")
source("predictions_classfn.R")
source("stability.R")
source("mm_features.R")
source("learners.R")
source("feature_selectors.R")
source("data_helpers.R")
source("tuning.R")
	
	
#-----------------------------------------------------------------------------------------------------------------------------
# MODELING
#-----------------------------------------------------------------------------------------------------------------------------
model_results = function(res, task, model_id, result_file, predns, perf, feats, stab, tune)
{
	print(paste0("In model_results: ", model_id))
	result_file = paste0(result_file, "_", model_id)	
	predns$save(res$pred)
	predns$write(result_file)
	
	class_names = get_class_names(res$pred)
	for (i in 1:length(res$models)) 
	{
		raw_mod = getLearnerModel(res$models[[i]], more.unwrap = TRUE)
		
		# Save tuning results, if any
		if (inherits(raw_mod, "TuneModel")) {
			print(raw_mod$learner.model$opt.result$x)
			tune$save(raw_mod$learner.model$opt.result$x)
		}
		
		# Get feature importance scores
		imp_data = getFeatImpScores(raw_mod, class_names)
		not_selected = setdiff(getTaskFeatureNames(task), res$extract[[i]])
		if (length(not_selected) > 0) {
			imp_data[not_selected, ] = 0
		}
		
		perf$save(res)
		feats$save_multiclass(model_id, imp_data, task$task.desc$id, i)
	}

	perf$write(result_file)
	feats$write(result_file)
	stab$save(model_id, res$extract, getTaskNFeats(task))
	stab$write(result_file)
}

# Run one base method - no feature selection	
run_learner = function(idx, tasks, config, baselrn, resamp, balance, result_file, cache_dir, predns, perf, feats, stab, tune) 
{
	writeLines(paste0("\nBASE METHOD: ", baselrn))
	cfg_lrn = add_learner(config, baselrn)
	learners = create_learners(cfg_lrn, base_learners, base_filters, "prob", balance, cache_dir)
	res = resample(learner = learners[[idx]], task = tasks[[idx]], measures = classfn.measures, resampling = resamp, models = TRUE, extract = getFilteredFeatures)
	model_results(res, tasks[[idx]], baselrn, result_file, predns, perf, feats, stab, tune)

	rmc = ROCMultiClass$new()
	rmc$calculate(baselrn, res$pred)
	rmc$calc_mc_roc(as.factor(res$pred$data$truth), as.factor(res$pred$data$response))
	rmc$write(result_file, baselrn)
	rmc$plot(baselrn, baselrn, result_file)
}


#Run a filter on a base learner
#Base methods should be tuned as well.
run_filter = function(idx, tasks, config, baselrn, basefilt, resamp, balance, result_file, cache_dir, predns, perf, feats, stab, tune) 
{
	print("In run_filter")
	cfg_lrn = add_featsel(config, baselrn, basefilt)
	model_name = paste0(baselrn, "_", basefilt)
	writeLines(paste("\n\nMODEL: ", model_name))
		
	learners = create_learners(cfg_lrn, base_learners, base_filters, "prob", balance, cache_dir, model_name = model_name)	
	res = resample(learner = learners[[idx]], task = tasks[[idx]], measures = classfn.measures, resampling = resamp, models = TRUE, extract = getFilteredFeatures)							
	res$pred$data$ID = rownames(res$pred$data)
	model_results(res, tasks[[idx]], model_name, result_file, predns, perf, feats, stab, tune)
	
	rmc = ROCMultiClass$new()
	rmc$calculate(model_name, res$pred)
	rmc$calc_mc_roc(as.factor(res$pred$data$truth), as.factor(res$pred$data$response))
	rmc$write(result_file, model_name)
	rmc$plot(model_name, model_name, result_file)
}


classfn_tests = function(config, res_index, cache_dir, big = FALSE, ovr_class = NULL, subset = NULL, concat = FALSE, balance = FALSE, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE) 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print(Sys.time())
	configureMlr(show.learner.output = TRUE, on.learner.error = 'warn', on.par.without.desc = 'warn')
	set.seed(24601, "L'Ecuyer")	

# Shouldn't need thses lines - try deleting	
	acc.na = setAggregation(acc, test.mean_narm)
#	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	prob.measures = list(acc.na, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	tasks = create_tasks(data_dir, config, TASK_CLASSIF, ovr_class, big, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
	print(paste0("Number of tasks = ", length(tasks)))
	if (concat) {
		tasks = concat_tasks(tasks)
		if (length(config$baseModels) > 1) {
			config$baseModels[2:length(config$baseModels)] = NULL
		}
	}
	
	for (idx in 1:length(tasks)) {
		print(paste0("Task = ", idx))
		dat = getTaskData(tasks[[idx]])
		classes = unique(dat[, config$targetVar])
		ohe.task = tasks[[idx]]  %>>% cpoDummyEncode(reference.cat = TRUE)	# So that we know the  number of features after one-hot encoding and their names 

		# Count the number of features and report
		task.names = getTaskFeatureNames(ohe.task)
		print(paste0("Number of features: ", getTaskNFeats(tasks[[idx]])))
		print(paste0("Number of features after encoding: ", getTaskNFeats(ohe.task)))
		
		num_features = getTaskNFeats(ohe.task)
		if (num_features == 0) {
			print(Sys.time())
			print("Returning ...")
			return(NULL)
		}
		
		#--------------------------------------------------------
		# TUNING LIMITS
		#--------------------------------------------------------
		psqrt = round(sqrt(num_features))
		tune_lowerlim = 5
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
		resamp = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
		ri = makeResampleInstance(resamp, task = tasks[[1]])
		
		#-----------------------------------------------------------------------------------------------------------------------------
		# DATA STRUCTURES TO COLLECT RESULTS
		#-----------------------------------------------------------------------------------------------------------------------------	
		perf   = Performance$new(classfn.measures)	
		predns = Prediction$new(ohe.task)
		feats  = MM_Features$new(tasks)
		stab   = Stability$new(classes)
		tune   = Tuning$new()	

		#------------------------------------------------------------------------------------------------------------------------------
		# RUN THE CLASSIFICATION TESTS ON KATANA
		#------------------------------------------------------------------------------------------------------------------------------	
		
		if (substring(Sys.info()['nodename'], 1, 1) == 'k' || is.null(active_learners)) {
			pbs_index = as.integer(Sys.getenv("PBS_ARRAY_INDEX"))
			print(paste0("pbs_index = ", pbs_index))
			print(paste0("Modality: ", config$dataSets[[idx]]$modality))
			result_file = paste(config$resultFile, config$dataSets[[idx]]$modality, res_index, sep = "_")
			
			if (pbs_index <= NUM_ALGORITHMS) {
				run_learner(idx, tasks, config, names(base_learners[pbs_index]), ri, balance, result_file, cache_dir, predns, perf, feats, stab, tune)
			} else {
				model_index = ((pbs_index-1) %% NUM_ALGORITHMS) + 1
				fs_index = ((pbs_index-1) %/% NUM_ALGORITHMS)
				print(paste0("model_index = ", model_index, ", fs_index = ", fs_index))
				
				if (fs_index <= NUM_FEATSEL) {
					run_filter(idx, tasks, config, names(base_learners[model_index]), names(base_filters[fs_index]), ri, balance, result_file, cache_dir, predns, perf, feats, stab, tune)
				}
			}
		}
	}
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}