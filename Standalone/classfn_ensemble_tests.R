library(mlr)
library(mlrCPO)
library(checkmate)
library(BBmisc)
library(data.table)
library(testthat)
library(stats)
library(cluster)
library(dendextend)
library(dynamicTreeCut)
library(mice)
library(dplyr)


ENS_SEP = "-"

if (Sys.info()['nodename'] == 'nemesis') {
	code_dir = "./Code/"
} else if (substring(Sys.info()['nodename'], 1, 1) == 'k') {
	code_dir = "./"
} else {
	code_dir = "./"
}
print(code_dir)
setwd(code_dir)

source("const.R")
source("config.R")
source("mice_cpo.R")
source("normalisation.R")
source("roc_measures.R")
source("perf_measures.R")
source("stability.R")
source("performance_classfn.R")
source("filter_boruta.R")
source("filter_lefse.R")
source("predictions_classfn.R")
source("agg_thresh.R")
source("models.R")
source("R_code_mapdp_counts.R")
source("clustMapDP.R")
source("threshold.R")
source("FilterHomogeneous.R")
source("FilterEmbedded.R")
source("filter_boruta.R")


classfn_ensemble_tests = function(fs_ind, task_id, dataset, result_file, target_var, active_learners = LRN_ALL, run_stability=FALSE, impute = TRUE, normalisation = "CPM", ens_type = "hom", rpt = 0, clustering = TRUE, split = 4) 
{   
  #--------------------------------------------------------------------------
  # INITIALISATION
  #--------------------------------------------------------------------------
	print(paste0("Clustering = ", clustering))
	print(fs_index)
	print(Sys.time())
	configureMlr(show.learner.output = TRUE, show.info = TRUE, on.learner.error='warn', on.par.without.desc='warn')

  #-----------------------------------------------------------------------------------------------------------------------------
  # PRE-PROCESSING
  #-----------------------------------------------------------------------------------------------------------------------------

	# CPO to add random probes to the data within the CV loop
	cpoProbe = makeCPORetrafoless("probe",
							 dataformat = "df.all",
							 cpo.trafo = function(data, target) {
#									Must shuffle row-wise to maintain the correlation between the shadows!
									shuffled =  as.data.frame(data[sample(nrow(data)), grepl("^shadow_", names(data))])  # Shadows shuffled row-wise to maintain correlations between them
									data = cbind(data[, !grepl("^shadow_", names(data))], shuffled)
									return(data)
	})
	
	 add_probes = function(alldata) {
			data = alldata[,!names(alldata) %in% c(target_var)]
			categ = lapply(data, function(x) length(unique(na.omit(x))))
			shadows = data.frame(lapply(data[,categ > 2], sample))
			colnames(shadows) = paste("shadow", colnames(shadows), sep = "_")
			return(cbind(alldata, shadows))
		}	

	
  #-----------------------------------------------------------------------------------------------------------------------------
  # THE TASK - I.E. DATSET
  #-----------------------------------------------------------------------------------------------------------------------------
	
	#Remove the patient ID
	dataset = dataset[ , !(names(dataset) %in% c("ID"))]
	
	# Drop features that have > 50% missing values or <= 10% non-zero values
	count_na = as.data.frame(colSums(is.na(dataset)))
	drops_na = rownames(count_na)[which(count_na > nrow(dataset)/2)]
	count_nz = as.data.frame(colSums(dataset != 0, na.rm = TRUE) / nrow(dataset)*100)
	drops_nz = rownames(count_nz)[which(count_nz <= 10)]
	drops = c(drops_na, drops_nz)
	dataset = as.data.frame(dataset[ , !(names(dataset) %in% drops)])
	print(paste0("Dropping ", length(drops), " columns that have too many missing or zero values:"))
	
	# Make the task
	classfn.task = makeClassifTask(id = task_id, data = dataset, target = target_var)
	ohe.task = classfn.task  %>>% cpoDummyEncode(reference.cat = TRUE)	# So that we know the  number of features after one-hot encoding and their names 

	# Count the number of features and report
	task.names = getTaskFeatureNames(ohe.task)
	print(task.names)
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
	perf  = Performance$new(prob.measures)
	predns = Prediction$new(ohe.task)
	feats = Features$new(list(ohe.task))
	stab  = Stability$new(list(ohe.task))
	
	#-----------------------------------------------------------------------------------------------------------------------------
  # GLOBAL VARS AND DATA STRUCTURES TO COLLECT RESULTS - PLUS FUNCTIONS TO OPERATE ON THEM
  #-----------------------------------------------------------------------------------------------------------------------------	
  psqrt = round(sqrt(num_features))
	print(paste0("Number of features in task = ", getTaskNFeats(classfn.task)))
	   	
  #--------------------------------------------------------
  # TUNING LIMITS
  #--------------------------------------------------------
	tune_lowerlim = 5
	tune_upperlim = round(num_features/3)
	if (tune_upperlim < tune_lowerlim) tune_upperlim = num_features
#	tune_upperlim = min(round(num_features/2), MAX_FEATURES)
#	tune_seq = seq(from=tune_lowerlim, to=tune_upperlim, by=5)	
	ctrl = makeTuneControlRandom(maxit = NUM_ITERS_TUNE)
	
source("learners.R")
source("feature_selectors.R")
     
    
  #-----------------------------------------------------------------------------------------------------------------------------
  # RESAMPLING STRATEGIES
  #-----------------------------------------------------------------------------------------------------------------------------
	if (run_stability) NUM_ITERS = NUM_ITERS_STAB
	inner = makeResampleDesc("RepCV", reps=NUM_ITERS, folds=NUM_FOLDS, stratify=TRUE)	  # Benchmarking: 5-fold CV repeated 5 times
	outer = makeResampleDesc("RepCV", reps=NUM_ITERS, folds=NUM_FOLDS, stratify=TRUE)	  # Benchmarking: 5-fold CV repeated 5 times
	tuning = makeResampleDesc("CV", iters=NUM_ITERS, stratify=TRUE)											# Tuning: 5-fold CV, no repeats
	stabsel = makeResampleDesc("Subsample", iters=NUM_ITERS_STAB, split=1/2, stratify=TRUE)	# Stability selection: 100 iterations of subsampling
	boot = makeResampleDesc("Bootstrap", iters = NUM_BOOTSTRAP_SAMPLES, stratify = TRUE)	# Bootstrapping 
 	subsamp = makeResampleDesc("Subsample", iters = NUM_BOOTSTRAP_SAMPLES, split = 2/3, stratify = TRUE)	# Subsampling - don't use - causes problems for OATS

  #------------------------------------------------------------------------------------------------------------------------------
  # FEATURE SELECTION - FILTER METHODS
  #------------------------------------------------------------------------------------------------------------------------------
#	params = makeParamSet(makeNumericParam("fw.perc", lower=0.01, upper=0.5))
#	params = makeParamSet(makeIntegerParam("fw.abs", lower=tune_lowerlim, upper=tune_upperlim))
#	params_untuned = makeParamSet(makeDiscreteParam("fw.perc", values=c(1.0)))
#	params = makeParamSet(makeDiscreteParam("fw.abs", values=tune_seq))
#	ctrl = makeTuneControlRandom(maxit=min(NUM_ITERS_TUNE, length(tune_seq)))
	ctrl = makeTuneControlRandom(maxit=NUM_ITERS_TUNE)
#	ctrl = makeTuneControlGrid(resolution=20L)

  #------------------------------------------------------------------------------------------------------------------------------
	# Base Filters
  #------------------------------------------------------------------------------------------------------------------------------
	
	rf.lrn = makeLearner(cl="classif.randomForest", id = "perf.rf", predict.type="response")
	featsel_uni_fse = list("filt_uni" = list("univariate.fse" = list(perf.learner=rf.lrn, filename=result_file)))
  featsel_ranger_fse = list("filt_ranger" = list("ranger.fse" = list(num.trees=5000, splitrule="gini", min.node.size=10, filename=result_file)))
	featsel_rf_fse = list("filt_rf_varimp" = list("rfsrc.importance.fse" = list(ntree = 1000, nsplit = 10, mtry = psqrt, nodesize=3, filename=result_file)))
	featsel_rfmd_fse = list("filt_rf_md" = list("rfsrc.importance.fse" = list(metho="md", ntree=1000, nsplit=10, nodesize=3, filename=result_file)))
	featsel_ridge_fse = list("filt_ridge" = list("cv.glmnet.fse" = list(alpha = 0, nfolds = 5L, filename=result_file)))
	featsel_elasticnet_fse = list("filt_elasticnet" = list("cv.glmnet.fse" = list(alpha = 0.5, nfolds = 5L, filename=result_file)))
	featsel_lasso_fse = list("filt_lasso" = list("cv.glmnet.fse" = list(alpha = 1, nfolds = 5L, filename=result_file)))
	featsel_xgbtree_fse = list("filt_xgbtree" = list("XGBoost.fse" = list(booster = "gbtree", nrounds = 300, max_depth = 3, eta = 0.28, lambda = 0.2, filename=result_file)))
	featsel_xgblin_fse = list("filt_xgblinear" = list("XGBoost.fse" = list(booster = "gblinear", lambda = 3.3, filename=result_file)))
	
	featsel_fse = c(featsel_uni_fse, featsel_ranger_fse, featsel_rf_fse, featsel_rfmd_fse, featsel_ridge_fse, featsel_elasticnet_fse, featsel_lasso_fse, featsel_xgbtree_fse, featsel_xgblin_fse)
	featsel_filters_fse = c(featsel_uni_fse, featsel_ranger_fse, featsel_rf_fse, featsel_rfmd_fse, featsel_ridge_fse, featsel_xgblin_fse)
	featsel_sparse_fse = c(featsel_elasticnet_fse, featsel_lasso_fse, featsel_xgbtree_fse, featsel_xgblin_fse)	
							 
   	
#------------------------------------------------------------------------------------------------------------------	
# RESULTS
#------------------------------------------------------------------------------------------------------------------	

	get_results = function(res, method, suffix) {
		classfn_res = classfn_results(res, TRUE)
		perf$save(method, res)
		feats$save(method, classfn_res$names, classfn_res$scores, length(res$models))
		stab$save(method, classfn_res$names, getTaskNFeats(classfn.task))
		perf$write(result_file, method)				
		feats$write(result_file, suffix)
		stab$write(result_file, suffix)
	}

	model_results = function(res, model_id, result_file, suffix = "", unwrap = TRUE)
	{

#		predns$save(res$pred)
#		predns$write(result_file)
		
	#		class_names = c(get_class_names(res$pred), "all")
		class_names = get_class_names(res$pred)
		model_name = strsplit(model_id, "-")[[1]][1]
		names = list()
		scores = list()

		for (i in 1:length(res$models)) {
			mod = getLearnerModel(res$models[[i]], more.unwrap = TRUE)
			
			# Save tuning results, if any
#			if (inherits(mod, "TuneModel")) {
#				print(mod$learner.model$opt.result$x)
#				tune$save(mod$learner.model$opt.result$x)
#			}
			
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
#		tune$write(result_file)
	}

	
# Step 1 - Test the stability and accuracy of each individual feature selector, without ensembling.
#        - This doesn't do any tuning as you can't tune the hyperparameters of filters in mlr2.
#				 - Do tuning first and manually plug in values into this code.
#------------------------------------------------------------------------------------------------------------------	
	test_individual = function(feat_selectors, resamp, base_learners, thresholds, impute = TRUE, normalisation = "STAND")
	{		
		print("Individual feature selectors")
		for (lrn in base_learners) {
			for (i in 1:length(feat_selectors)) {
				for (thresh in thresholds) {
					method = paste(names(feat_selectors[i]), 'Individual', thresh$cutoff, thresh$auto_thresh, lrn$id, se = ENS_SEP)
					print(method)
					using_probes = length(grep("^probe_*", thresh$auto_thresh)) > 0
					fw  = makeFilterWrapper(learner = lrn, fw.method = "H-Individual", cache = FALSE, fw.perc = thresh$cutoff, 
																		fw.base.methods = NULL, fw.abs = NULL, fw.threshold = NULL, fw.fun = NULL,
																		autothresh = thresh$auto_thresh, filename = result_file,
																		base.method = names(feat_selectors[[i]]), base.args = feat_selectors[[i]])

					if (using_probes) {
						probe_data = add_probes(dataset)
						task = makeClassifTask(id = task_id, data = probe_data, target = target_var)
						fw = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.001) %>>% cpoDummyEncode(reference.cat = TRUE) %>>% cpoProbe() %>>%  fw
					} else {
						task = classfn.task
						fw = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.001) %>>% cpoDummyEncode(reference.cat = TRUE)  %>>%  fw
					}					
		
					fw = cpoNormalise(normalisation) %>>% fw
					if (impute) 
						fw = cpoMice() %>>% fw
					res = resample(learner = fw, task = task, resampling = resamp, measures = prob.measures, models = TRUE, extract = getFilteredFeatures)							
					model_results(res, method, result_file, "ind")
					rmc = ROCMultiClass$new()
					rmc$calculate(method, res$pred)
					rmc$write(result_file)
					rmc$plot(method, "Ensemble FS", result_file)
#					for (i in 1:length(res$extract)) {
#						write_raw_features(result_file, "ind", setNames(rep(1, length(res$extract[[i]])), res$extract[[i]]))
#					}
				}
			}
		}
  }

#Step 2 - Test the stability and accuracy of the homogeneous ensemble filters
# 			- Create a homogeneous filter from each individual feature selector
#------------------------------------------------------------------------------------------------------------------
	
	test_homogeneous_ens = function(feat_selectors, r_inner, r_outer, base_learners, thresholds, impute = TRUE, normalisation = "STAND")
	{	
		print("\n\nHomogeneous Ensembles\n")
		for (lrn in base_learners) {
			lrn = cpoScale() %>>% lrn
			for (i in 1:length(feat_selectors)) {
				for (thresh in thresholds) {
					method = paste(names(feat_selectors[i]), thresh$agg, thresh$cutoff, thresh$auto_thresh, lrn$id, sep = ENS_SEP)
					print(method)
					using_probes = length(grep("^probe_*", thresh$auto_thresh)) > 0
					fw  = makeFilterWrapper(learner = lrn, fw.method = thresh$agg, cache = FALSE, fw.perc = thresh$cutoff,
																	fw.base.methods = NULL, fw.abs = NULL, fw.threshold = NULL, fw.fun = NULL,
																	resamp = r_inner, autothresh = thresh$auto_thresh, filename = result_file, 
																	base.method = names(feat_selectors[[i]]), base.args = feat_selectors[[i]], hom = TRUE)
					if (using_probes) {
						probe_data = add_probes(dataset)
						task = makeClassifTask(id = task_id, data = probe_data, target = target_var)
#						fw = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.001) %>>% cpoDummyEncode(reference.cat = TRUE) %>>% cpoProbe() %>>%  fw
#						fw = cpoDummyEncode(reference.cat = TRUE) %>>% cpoProbe() %>>%  fw
						fw = cpoDummyEncode(reference.cat = TRUE) %>>% fw
					} else {
						task = classfn.task
#						fw = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.001) %>>% cpoDummyEncode(reference.cat = TRUE)  %>>%  fw
						fw = cpoDummyEncode(reference.cat = TRUE)  %>>%  fw
					}
					
					if (impute) 
						fw = cpoMice() %>>% fw
					res = resample(learner = fw, task = task, resampling = r_outer, measures = prob.measures, models = TRUE, extract = getFilteredFeatures)
					model_results(res, method, result_file, "hom")
					rmc = ROCMultiClass$new()
					rmc$calculate(method, res$pred)
					rmc$write(result_file)
					rmc$plot(method, "Ensemble FS", result_file)
				}
			}
		}
	}


#Step 3  - Test the stability and accuracy of the heterogeneous ensemble using all filters, one of each
#------------------------------------------------------------------------------------------------------------------
	test_heterogeneous_ens = function(feat_selectors, feat_args, aggregators, resamp, base_learners, cut_offs) {
		cat("\n\nHeterogeneous Ensemble - Fixed\n")
		
		for (lrn in base_learners) {
			lrn = cpoScale() %>>% lrn
			for (agg in aggregators) {
				for (ss in cut_offs) {
					method = paste(agg, ss, threshold_type, lrn$id, sep = ENS_SEP)
					fw  = makeFilterWrapper(learner = lrn, fw.method = agg, fw.perc = ss, cache = FALSE,
																	fw.base.methods = feat_selectors, more.args = feat_args)
					fw = cpoMice() %>>% cpoDropConstants(ignore.na = TRUE, abs.tol = 0.01) %>>% cpoDummyEncode(reference.cat = TRUE) %>>% fw
					res = resample(learner = fw, task = classfn.task, resampling = resamp, measures = prob.measures, models = TRUE, extract = getFilteredFeatures)	
					get_results(res, method, "het")
				}
			}
		}
	}

# Return results of adding one new method to the existing ensemble
# No point using oarallel processing as each step depends on the step before
#------------------------------------------------------------------------------------------------------------------
	increment_ensemble = function(curr_ens, feat_selectors, next_method, base_learner, thresh, resample_inst, impute) {
	# Add the next method to the existing ensemble
		print(paste("Incrementing ensemble with ", next_method))
		curr_ens = append(curr_ens, feat_selectors)
		print("Current ensemble:")
		print(names(curr_ens))
		ens_size = length(curr_ens)
		names(curr_ens)[ens_size] = next_method  # Do I need this?
		
	# Make a filter wrapper with this ensemble and call resample on it to get ensemble performance
		base_args = list()
		base_methods = list()
		for (k in 1:ens_size) {
			base_args = c(base_args, curr_ens[[k]])
			base_methods = c(base_methods, names(curr_ens[[k]]))
		}
		base_methods = unlist(base_methods)
		print("Base methods: ")
		print(base_methods)
		
		base_learner = cpoScale() %>>% base_learner
		using_probes = length(grep("^probe_*", thresh$auto_thresh)) > 0
		if (!any(duplicated(base_methods))) {
			rd_inner = makeResampleDesc("Bootstrap", iters = ens_size, stratify = TRUE)
			filt  = makeFilterWrapper(learner = base_learner, fw.method = thresh$agg, cache = FALSE, fw.perc = thresh$cutoff,
																	fw.base.methods = NULL, fw.abs = NULL, fw.threshold = NULL, fw.fun = NULL,
																	resamp = rd_inner, autothresh = thresh$auto_thresh, filename = result_file,
																	base.method = base_methods, base.args = base_args, hom = FALSE)
			if (using_probes) {
				probe_data = add_probes(dataset)
				task = makeClassifTask(id = task_id, data = probe_data, target = target_var)
				filt = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.01) %>>% cpoDummyEncode(reference.cat = TRUE) %>>% cpoProbe() %>>%  filt
			} else {
				task = classfn.task
				filt = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.01) %>>% cpoDummyEncode(reference.cat = TRUE)  %>>%  filt
			}
			
			if (impute) 
				filt = cpoMice() %>>% filt
			res = resample(learner = filt, task = task, resampling = resample_inst, measures = prob.measures, models = TRUE, extract = getFilteredFeatures)
			return(res)
		} else {
			return(NA)
		}
	}

#Step 4 - Create a heterogeneous ensemble using greedy forward stepwise selection
#------------------------------------------------------------------------------------------------------------------
	test_stepwise_ensemble = function(feat_selectors, resample_inst, thresholds, learners, alpha, decision = "stab", impute = TRUE)
	{    
		print("Heterogeneous Stepwise Ensemble")		
		
	  for (lrn in learners) {
			for (i in 1:length(feat_selectors)) {
				for (thresh in thresholds) {
					print(paste0("Creating new stepwise ensemble with aggregator ", thresh$agg, " and cutoff ", thresh$cutoff))
					size = 1 
					curr_value = 0
					ensemble = list()		
					building_ensemble = TRUE
					
					while (building_ensemble)
					{
						#Initialise next iteration
						ensemble_res = list()

						# Add filters to the ensemble one at a time. Keep the filter that improves the performance/stability the most.
						for (i in 1:length(feat_selectors)) {
							next_method = names(feat_selectors[i])
							ensemble_res[[next_method]] = increment_ensemble(ensemble, feat_selectors[i], next_method, lrn, thresh, resample_inst, impute)
						}
						
						if (all(is.na(ensemble_res))) {
							print("Finished building")
							building_ensemble = FALSE
							print(paste0("Final ensemble: ", names(ensemble)))
						} else {
							best_value = 0
							for (i in 1:length(feat_selectors)) {
								print(paste0("Method = ", names(ensemble_res[i])))
								print(paste0("Best value = ", best_value))
#								print(ensemble_res[[i]])

								if (!is.na(ensemble_res[[i]])) {
									if (decision == "perf") {
										if (best_value < ensemble_res[[i]]$aggr['cindex.test.mean_narm']) {
											best_value = ensemble_res[[i]]$aggr['cindex.test.mean_narm']
											best_method = names(ensemble_res[i])
										}
									} else if (decision == "stab") {
										new_stab = calculate(ensemble_res[[i]]$extract, length(ensemble_res[[i]]$extract), getTaskNFeats(classfn.task))
										if (best_value < new_stab[IND_STAB_CONS]) { 
											best_value = new_stab[IND_STAB_CONS]
											best_method = names(ensemble_res[i])
										}
									} else if (decision == "mean") {
										new_stab = calculate(ensemble_res[[i]]$extract, length(ensemble_res[[i]]$extract), getTaskNFeats(classfn.task))
										print(paste0("new_stab = ", new_stab[IND_STAB_CONS]))

										new_perf = ensemble_res[[i]]$aggr['cindex.test.mean_narm']
										print(paste0("new_perf = ", new_perf))

#										hmean = 2 / ((1/new_stab[IND_STAB_CONS]) + (1/new_perf))
										mean_val = (new_stab[IND_STAB_CONS] + new_perf) / 2.0
										print(paste0("mean_val = ", mean_val))
										
										if (best_value < mean_val) { 
											best_value = mean_val
											best_method = names(ensemble_res[i])
										}
									}
									print(paste0("Best value = ", best_value))
									print(paste0("Best method = ", best_method))
								}
							}
											
							#Compare performance of new ensemble with previous and update if better
							if (best_value >= (curr_value - alpha)) {
								print(paste0("Adding ", best_method, " to the ensemble"))
								size = size + 1
								ens_name = paste0("ensemble", size, ENS_SEP, thresh$agg, ENS_SEP, lrn$id, ENS_SEP, thresh$cutoff)
								feats = save_features(feats, ens_name, ensemble_res[[best_method]]$extract, NULL, length(ensemble_res[[best_method]]$extract))
								stab = save_stability(stab, ens_name, feats$featnames, getTaskNFeats(classfn.task))
								perf = save_performance(perf, ens_name, ensemble_res[[best_method]])					
								write_stability(stab, result_file, "het")
								write_performance(perf, result_file, "het")
								write_features(feats, result_file, "het")
								
								curr_value = best_value
								ensemble[size] = feat_selectors[best_method]
								names(ensemble)[size] = best_method
							}
							else {
								print("Finished building")
								building_ensemble = FALSE
								print(paste0("Final ensemble: ", names(ensemble)))
							}
						}
						rm(ensemble_res)
					}
				}
			}
		}
		return(ensemble)
	} 

  set.seed(11162, "L'Ecuyer")  
	alpha = 0.01
	ri = makeResampleInstance(desc = outer, task = classfn.task)

	fs_index = as.integer(fs_ind)	# Passed in
	pbs_index = as.integer(Sys.getenv("PBS_ARRAY_INDEX"))
	agg_index = ((pbs_index - 1) %/% NUM_THRESHOLDS) + 1
	thresh_index = ((pbs_index - 1) %% NUM_THRESHOLDS) + 1
	print(paste("FS index = ", fs_index, ", PBS array index = ", pbs_index, ", agg_index = ", agg_index, ", thresh_index = ", thresh_index))

	ridge.lrn = makeLearner(cl="classif.cvglmnet", id = "Ridge", predict.type="prob", alpha = 0, nfolds=5)
	rf.lrn = makeLearner(cl="classif.randomForest", id = "rf", predict.type="prob")
#	xgb.lrn = makeLearner(cl="classif.xgboost", id = "xgb", predict.type="prob", booster = "gbtree", nrounds = 300, max_depth = 3, eta = 0.28, lambda = 0.2, filename=result_file)
	xgb.lrn = makeLearner(cl="classif.xgboost", id = "xgb", predict.type="prob", booster = "gbtree", eta=0.001, max_depth=5, gamma=3, subsample=0.75, colsample_bytree=1, filename=result_file)
	model_list = list(xgb.lrn)

	if (ens_type == "het")
		test_stepwise_ensemble(featsel_fse, agg_thresh_het[1], model_list, alpha, "stab", impute)
	else if (ens_type == "hom")
		test_homogeneous_ens(featsel_fse[fs_index], boot, ri, model_list, agg_thresh[pbs_index], impute)
	else if (ens_type == "ind") {
		if (fs_index %in% c(1, 2, 3, 5, 11, 12))
			test_individual(featsel_fse[fs_index], ri, model_list, agg_thresh_ind_filter[pbs_index], impute) 
		else
			test_individual(featsel_fse[fs_index], ri, model_list, agg_thresh_ind_sparse[1], impute)
	}
	

#---------------------------------------------------------------------------------------
# Clean up
#---------------------------------------------------------------------------------------    

  cat("\n\n")
  print(warnings())
  print(Sys.time())
}
