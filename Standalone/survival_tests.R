library(survival)
library(Hmisc)
library(mlr)
library(mlrCPO)
library(pryr)
library(checkmate)
library(BBmisc)
library(mice)
library(Boruta)
library(stats)
library(mboost)


source("const.R")
source("config.R")
source("imputation.R")
source("normalisation.R")
source("data_helpers.R")
source("feature_selectors.R")
source("mm_features.R")
source("filter_boruta.R")
source("learners.R")
source("models.R")
source("performance_classfn.R")
source("perf_measures.R")
source("stability.R")
source("tuning.R")
source("RLearner_surv_CoxBoost.R")
source("RLearner_surv_cv.CoxBoost.R")
source("RLearner_surv_randomForestSRC.R")
source("RLearner_surv_xgboost.R")


#-----------------------------------------------------------------------------------------------------------------------------
# MODELING
#-----------------------------------------------------------------------------------------------------------------------------
model_results_surv = function(res, task, model_id, result_file, perf, feats, stab, tune)
{
	print("In model_results_surv")
	result_file = paste0(result_file, "_", model_id)

	feature_sets = list()
	for (i in 1:length(res$models)) {
		raw_mod = getLearnerModel(res$models[[i]], more.unwrap = TRUE)
		if (inherits(raw_mod, "character")) {		
			feature_sets[[i]] = NULL
		} else {
			# Save tuning results, if any
			if (inherits(raw_mod, "TuneModel")) {
				print(raw_mod$learner.model$opt.result$x)
				tune$save(raw_mod$learner.model$opt.result$x)
			}
			
			# Get feature importance scores
			imp_data = getFeatImpScores(raw_mod)
#			print(imp_data)
			wrapped_mod = getLearnerModel(res$models[[i]], more.unwrap = FALSE)
			selected = getFilteredFeatures(wrapped_mod)
#			print("Selected Features:")
#			print(selected)
			not_selected = setdiff(getTaskFeatureNames(task), selected)
#			print(paste0("Selected: ", length(selected), ", not selected: ", length(not_selected)))
#			if (length(not_selected) > 0) {
#				imp_data[not_selected] = 0
#			}
			
#			if (!is.null(res$extract[[i]])) {
#				not_selected = setdiff(getTaskFeatureNames(task), res$extract[[i]])
#				print("Features not selected:")
#				print(not_selected)
#				if (length(not_selected) > 0) {
#					imp_data[not_selected] = 0
#				}
#			}
			feature_sets[[i]] = names(imp_data)		
			perf$save(res)
			feats$save(model_id, imp_data, task$task.desc$id, i)
		}
	}

	perf$write(result_file)
	feats$write(result_file)
	stab$save(model_id, feature_sets, getTaskNFeats(task))
	stab$write(result_file)
	tune$write(result_file)
}

plot_survival = function(res, model_id, result_file)
{
	print(paste0("In plot_survival: ", model_id))
	plot_file = paste0(result_file, "_", model_id)
	jpeg(paste0(plot_file,  "_plot.jpg"))
	par(cex.main = 2.5)
	par(cex.lab = 1.5)
	par(cex.axis = 1.5)

	for (i in 1:length(res$models)) {
		raw_mod = getLearnerModel(res$models[[i]], more.unwrap = TRUE)
#		print("Raw model:")
#		print(summary(raw_mod))
#		print(str(raw_mod))
		
		if (inherits(raw_mod, "coxph")) {
			surv_curves = survfit(raw_mod)
#			print("Survfit output:")
#			print(str(surv_curves))
			plot(surv_curves, xlab = "Days", ylab = "C-Index", ylim = c(0.0, 1.0) )
		} else if (inherits(raw_mod, "rfsrc")) {
			plot.survival(raw_mod)
		}
	}
	dev.off()
}

# Run one base method - no feature selection	
run_learner_surv = function(idx, task, config, baselrn, resamp, balance, result_file, cache_dir, perf, feats, stab, tune) 
{
	writeLines(paste0("\nBASE METHOD: ", baselrn))
	cfg_lrn = add_learner(config, baselrn, base_learners_surv)
	learners = create_learners(cfg_lrn, base_learners_surv, base_filters, "response", balance, cache_dir)
	
#	res = resample(learner = learners[[idx]], task = tasks[[idx]], measures = surv.measures, resampling = resamp, models = TRUE, extract = getFilteredFeatures)
	res = resample(learner = learners[[1]], task = task, measures = surv.measures, resampling = resamp, models = TRUE)
	model_results_surv(res, task, baselrn, result_file, perf, feats, stab, tune)
	plot_survival(res, baselrn, result_file)
}


#Run a filter on a base learner
#Base methods should be tuned as well.
run_filter_surv = function(idx, task, config, baselrn, basefilt, resamp, balance, result_file, cache_dir, perf, feats, stab, tune) 
{
	cfg_lrn = add_featsel(config, baselrn, basefilt, base_learners_surv, base_filters)
	model_name = paste0(baselrn, "_", basefilt)
	writeLines(paste("\n\nMODEL: ", model_name))
	
	learners = create_learners(cfg_lrn, base_learners_surv, base_filters, "response", balance, cache_dir, model_name = model_name)	
#	res = resample(learner = learners[[idx]], task = tasks[[idx]], measures = surv.measures, resampling = resamp, models = TRUE, extract = getFilteredFeatures)								
	res = resample(learner = learners[[1]], task = task, measures = surv.measures, resampling = resamp, models = TRUE)	
	res$pred$data$ID = rownames(res$pred$data)
	model_results_surv(res, task, model_name, result_file, perf, feats, stab, tune)
	print("Formula:")
	print(getTaskFormula(task))
	plot_survival(res, model_name, result_file)
}


survival_tests = function(config, res_index, cache_dir, big = FALSE, ovr_class = NULL, subset = NULL, concat = FALSE, balance = FALSE, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE) 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
	print(Sys.time())
	print(paste0("Time var = ", config$timeVar))
	print(paste0("Status var = ", config$statusVar))
	configureMlr(show.learner.output = TRUE, on.learner.error = 'stop', on.par.without.desc = 'warn')	

	#------------------------------------------------------------------------------------------------------------------------------
	# BASE LEARNERS
  #------------------------------------------------------------------------------------------------------------------------------
	cox_filt.lrn = makeLearner(cl="surv.coxph", id = "perf.cox", predict.type="response")
	
	#-----------------------------------------------------------------------------------------------------------------------------
	# THE TASK - I.E. DATSET
	#-----------------------------------------------------------------------------------------------------------------------------
	tasks = create_tasks(data_dir, config, TASK_SURV, ovr_class, big, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
	if (concat) {
		tasks = concat_tasks(tasks, TASK_SURV)
		print(names(tasks))
		if (length(config$baseModels) > 1) {
			config$baseModels[2:length(config$baseModels)] = NULL
		}
	}
	d = getTaskData(tasks[[1]], target.extra = TRUE)
	ohe.task = tasks[[1]]  %>>% cpoDummyEncode(reference.cat = TRUE)	# So that we know the  number of features after one-hot encoding and their names 

	# Count the number of features and report
	task.names = getTaskFeatureNames(ohe.task)
	print(paste0("Number of features: ", getTaskNFeats(tasks[[1]])))
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
	inner = makeResampleDesc("RepCV", reps = NUM_ITERS, folds = NUM_FOLDS, stratify = TRUE)	# Benchmarking: 5-fold CV repeated 5 times
	outer = makeResampleDesc("RepCV", reps = NUM_ITERS, folds = NUM_FOLDS, stratify = TRUE)	# Benchmarking: 5-fold CV repeated 5 times
	single = makeResampleDesc("CV", iters = NUM_ITERS, stratify = TRUE)											# Single run of 5-fold CV
	tuning = makeResampleDesc("CV", iters = NUM_ITERS_TUNE, stratify = TRUE)								# Tuning: 5-fold CV, no repeats
	stabsel = makeResampleDesc("Subsample", iters = NUM_ITERS, split = 1/2, stratify = TRUE)	# Stability selection: 100 iterations of subsampling 
	
	#-----------------------------------------------------------------------------------------------------------------------------
	# DATA STRUCTURES TO COLLECT RESULTS
	#-----------------------------------------------------------------------------------------------------------------------------	
	print("Initialising data structures")
	perf   = Performance$new(surv.measures)	
	feats  = MM_Features$new(list("concat" = ohe.task))
	stab   = Stability$new(NULL)
	tune   = Tuning$new()
	
	#------------------------------------------------------------------------------------------------------------------------------
	# RUN THE CLASSIFICATION TESTS ON KATANA
	#------------------------------------------------------------------------------------------------------------------------------		
	if (substring(Sys.info()['nodename'], 1, 1) == 'k' || is.null(config$activeLrn)) {
		pbs_index = as.integer(Sys.getenv("PBS_ARRAY_INDEX"))
		print(paste0("pbs_index = ", pbs_index))
		result_file = paste(config$resultFile, res_index, sep = "_")
		
		if (pbs_index <= NUM_ALGORITHMS) {
			run_learner_surv(1, ohe.task, config, names(base_learners_surv[pbs_index]), outer, balance, result_file, cache_dir, perf, feats, stab, tune)
		} else {
			model_index = ((pbs_index-1) %% NUM_ALGORITHMS) + 1
			fs_index = ((pbs_index-1) %/% NUM_ALGORITHMS)
			print(paste0("model_index = ", model_index, ", fs_index = ", fs_index))
			
			if (fs_index <= NUM_FEATSEL) {
				run_filter_surv(1, ohe.task, config, names(base_learners_surv[model_index]), names(base_filters[fs_index]), outer, balance, result_file, cache_dir, perf, feats, stab, tune)
			}
		}
	}
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}