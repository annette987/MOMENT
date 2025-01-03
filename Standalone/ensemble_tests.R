library(survival)
#library(survivalsvm)
library(Hmisc)
library(mlr)
library(mlrCPO)
library(pryr)
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

source(paste(code_dir, "stability.R", sep=""))
source(paste(code_dir, "performance.R", sep=""))
source(paste(code_dir, "features.R", sep=""))
source(paste(code_dir, "models.R", sep=""))
source(paste(code_dir, "R_code_mapdp_counts.R", sep=""))
source(paste(code_dir, "clustMapDP.R", sep=""))
source(paste(code_dir, "threshold.R", sep=""))
source(paste(code_dir, "FilterHomogeneous.R", sep=""))
source(paste(code_dir, "FilterEmbedded.R", sep=""))
source(paste(code_dir, "RLearner_surv_xgboost.R", sep=""))


ensemble_tests = function(fs_ind, task_id, dataset, result_file, time_var, status_var, active_learners = LRN_ALL, run_stability=FALSE, impute = TRUE, ens_type = "hom", rpt = 0, clustering = TRUE, split = 4) 
{   
  #--------------------------------------------------------------------------
  # INITIALISATION
  #--------------------------------------------------------------------------
  print(fs_index)
	print(Sys.time())
	configureMlr(show.learner.output = TRUE, show.info = TRUE, on.learner.error='warn', on.par.without.desc='warn')
	
	#------------------------------------------------------------------------------------------------------------------------------
	# BASE LEARNERS
  #------------------------------------------------------------------------------------------------------------------------------
	cox_filt.lrn = makeLearner(cl="surv.coxph", id = "perf.cox", predict.type="response")
	cox_mod.lrn = makeLearner(cl="surv.coxph", id = "CoxPH", predict.type="response")
	ridge.lrn = makeLearner(cl="surv.cvglmnet", id = "Ridge", predict.type="response", alpha = 0, nfolds=5)
	elasticnet.lrn  = makeLearner(cl="surv.cvglmnet", id = "ElasticNet", predict.type="response", alpha = 0.5, nfolds=5)
	lasso.lrn  = makeLearner(cl="surv.cvglmnet", id = "Lasso", predict.type="response", alpha = 1, nfolds=5)
	cboostcv.lrn = makeLearner(cl="surv.cv.CoxBoost", id = "CoxBoost", predict.type="response")
	
	models = list(cox_mod.lrn, lasso.lrn, cboostcv.lrn)
	models_cox = list(cox_mod.lrn)
	models_ridge = list(ridge.lrn)
	models_cb = list(cboostcv.lrn)

  #-----------------------------------------------------------------------------------------------------------------------------
  # PRE-PROCESSING
  #-----------------------------------------------------------------------------------------------------------------------------

	# CPO to run imputation using mice within the CV loop
	cpoMice = makeCPO("mice", # nolint
							 dataformat = "df.features",
							 properties.data = c("numerics", "factors", "ordered", "missings"),
							 properties.adding = "missings",
							 cpo.train = function(data, target) {
 
#								print("*** cpoMice: cpo.train ***")
								# Remove constant and collinear features
								 ini = mice(data, maxit=0)
								 outlist1 = as.character(ini$loggedEvents[, "out"])
#								 print(outlist1)
#								 print(as.character(ini$loggedEvents))		
								 
								#Remove columns that are of little predictive value (See Flexible Imputation of Missind Data by Steph van Buren, Ch 4 & 9.1)
								 fx = flux(data)
								 outlist2 <- row.names(fx)[fx$outflux < 0.5]
								 data2 <- data[, !names(data) %in% outlist2]
								 fx2 <- flux(data2)
								 outlist3 <- row.names(fx2)[fx2$outflux < 0.5]
								 
								 outlist <- unique(c(outlist1, outlist2, outlist3))
#								 print("Mice outlist")
#								 print(outlist)
								 data2 <- data[, !names(data) %in% outlist]
								 
								 inlist = c("Age", "Sex")
								 predmat = mice::quickpred(data, minpuc = 0, mincor = 0.1, exclude = outlist, include = inlist, method = "kendall")
#								 print(predmat)
								 return(predmat)
							 }, 
							 cpo.retrafo = function(data, control) {
#									print("*** cpoMice: cpo.retrafo ***")
									imp_data = tryCatch({
										mice::mice(data, m=5,  method="cart", pred=control, remove.constant = TRUE, seed = 23109, print = FALSE)
									}, 
									error = function(cond) {
											print(paste("mice::mice returned error: ", cond))
											print(data)
											return(data)	
									})
									
									imputed = mice::complete(imp_data)
									return(imputed)
							}
	)


	# CPO to add random probes to the data within the CV loop
	cpoProbe = makeCPORetrafoless("probe",
							 dataformat = "df.all",
							 cpo.trafo = function(data, target) {
#									shuffled =  as.data.frame(lapply(data[, grepl("^shadow_", names(data))], sample))  # Shadow columns shuffled independently
#									Must shuffle row-wise to maintain the correlation between the shadows!
									shuffled =  as.data.frame(data[sample(nrow(data)), grepl("^shadow_", names(data))])  # Shadows shuffled row-wise to maintain correlations between them
									print(shuffled)
									data = cbind(data[, !grepl("^shadow_", names(data))], shuffled)
									print(data)
									return(data)
	})
	
	 add_probes = function(alldata) {
			data = alldata[,!names(alldata) %in% c(time_var, status_var)]
			categ = lapply(data, function(x) length(unique(na.omit(x))))
	#		shadows = data.frame(lapply(data[,categ != 2], sample))
			shadows = data.frame(lapply(data[,categ > 2], sample))
			colnames(shadows) = paste("shadow", colnames(shadows), sep = "_")
			return(cbind(alldata, shadows))
		}
	
	# Handle correlated features by clustering them - take one per cluster or use PCA? or oermute as a group
	# Find_clusters is run once on the whole dataset
	find_clusters = function(full_data, cor_method, dist_method, link_method) {		
		# Impute missing data
		target_data = full_data[,c(time_var, status_var)]
		full_data = full_data[ , !(names(full_data) %in% c("ID", status_var, time_var, "current_uti"))]
		outlist1 = list()
		if (impute) {
#			print("Imputing ... ")
			ini = mice(full_data, maxit = 0)
			outlist1 = as.character(ini$loggedEvents[, "out"])
#			print("Outlist:")
#			print(outlist1)
#			print(ini$loggedEvents)
			predmat = mice::quickpred(full_data, minpuc = 0, mincor = 0.4, exclude = outlist1)
			imp_data = mice::mice(full_data, m=5,  method="cart", pred=predmat, seed = 23109, printFlag=FALSE)
			imputed = mice::complete(imp_data)
		} else {
			imputed = full_data
		}
		
		# Standardise the numerical data
		factor_vars = names(imputed)[ sapply(imputed, is.factor) ]
#		print("Factors:")
#		print(factor_vars)
		to_remove = c(factor_vars, outlist1) # Don't scale constant vars as result will be NaN (divide by 0 variance)
		x = scale(imputed[,!(names(imputed) %in% to_remove)], scale = TRUE, center = TRUE) %>% as.matrix
#		print(x)

		# Cluster the data
		print("Clustering ...")
		sp = cor(x, method = cor_method)
		d = dist(sp, method = dist_method)
		hc = hclust(d, method = link_method)

		# Get names in clusters
		dend = as.dendrogram(hc)
		featnames = labels(dend)
		clusters = cutreeDynamic(hc, distM = as.matrix(d), method = "hybrid", cutHeight = 3, deepSplit = split, minClusterSize = 1, pamStage = FALSE)
		n_clusters <- length(unique(clusters) - (0 %in% clusters))
		print(paste0("Number of clusters = ", n_clusters))
		
		# Get univariate scores
		clust.task = makeSurvTask(id = task_id, data = cbind(imputed, target_data), target = c(time_var, status_var))
		fv = generateFilterValuesData(clust.task, method = "univariate.model.score", nselect = ncol(full_data), more.args = list(perf.learner=cox_filt.lrn))
		ranked = fv$data[, rank := frankv(fv$data, cols = "value", order = 1L, na.last = "keep", ties.method = "first")]
		setorderv(ranked, c("rank"), order = -1L, na.last = TRUE)
#		print(as.data.frame(ranked))
	
		# Choose "best" feature from cluster
		final_feats = c()
#		print("Clusters:")
		for (i in 1:n_clusters) {
			print(paste0("Cluster: ", i))
		  idx = which(clusters == i)
			if (length(idx) > 0) {
				clust = ranked[idx,]
				print(clust$name)
				print(featnames[idx])
				selected = clust$name[which(clust$value == max(clust$value))]
				if (length(selected) > 1)
					selected = selected[1]
		  	print(selected)
				final_feats = union(final_feats, selected)
#		  	final_feats = c(final_feats, featnames[idx[1]])	
			}
		}		
		return(final_feats)
	}
	
	# clusters are ordered by value
	feats_from_clusters = function(clusters, posn) {
		final_feats = c()
		for (i in 1:length(clusters)) {
			if (posn <= nrow(clusters[i]))
				selected = clusters[i]$name[posn]
			else
				selected = NA
#			print(selected)
			final_feats = union(final_feats, selected)
		}
		return(final_feats)
	}
	
	remove_clusters = function(clusters, feat) {
	}

	
  #-----------------------------------------------------------------------------------------------------------------------------
  # THE TASK - I.E. DATSET
  #-----------------------------------------------------------------------------------------------------------------------------
	
	# for ADNI data, remove class "labelled"
	ind = which(sapply(dataset, function(y) inherits(y, "labelled")))
	if (length(ind) > 0)
		dataset[ind] = lapply(dataset[ind], as.numeric)

	if (clustering) {	
		final_feats = find_clusters(dataset, "spearman", "euclidean", "complete")
		final_feats = c(time_var, status_var, final_feats)
		print("Features after clustering: ")
		print(final_feats)
		dataset = dataset[ , (names(dataset) %in% final_feats)]
	} else {
		dataset = dataset[ , !(names(dataset) %in% c("ID"))]
	}

	surv.task = makeSurvTask(id = task_id, data = dataset, target = c(time_var, status_var))
	print(paste0("Target names: ", getTaskTargetNames(surv.task)))
	print(paste0("Number of features: ", getTaskNFeats(surv.task)))
	num_features = getTaskNFeats(surv.task)
	ohe.task = surv.task  %>>% cpoDummyEncode()	# So that we know the  number of features after one-hot encoding and their names 
	print(paste0("Number of features after one hot encoding: ", getTaskNFeats(ohe.task)))
	task.names = getTaskFeatureNames(ohe.task)
	print(task.names)
	if (num_features == 0) {
		print(Sys.time())
		print("Returning ...")
		return(NULL)
	}
	print(paste0("Task size = ", getTaskSize(ohe.task)))
	dat = getTaskData(ohe.task)
	print(paste0("Number of rows in data: ", nrow(dat)))
	
	#-----------------------------------------------------------------------------------------------------------------------------
  # GLOBAL VARS AND DATA STRUCTURES TO COLLECT RESULTS - PLUS FUNCTIONS TO OPERATE ON THEM
  #-----------------------------------------------------------------------------------------------------------------------------	
  psqrt = round(sqrt(num_features))
	print(paste0("Number of features in task = ", getTaskNFeats(surv.task)))

	folds = ifelse(task_id == "OATS", NUM_FOLDS_TUNE, NUM_FOLDS)
	print(paste0("Number of folds = ", folds))
	if (rpt == 0)
		perf  = new_performance(folds * NUM_ITERS)
	else
		perf  = new_performance(folds)
	feats = new_features(ohe.task)
	stab  = new_stability()
	
  #--------------------------------------------------------
  # TUNING LIMITS
  #--------------------------------------------------------
	tune_lowerlim = 5
	tune_upperlim = round(num_features/3)
	if (tune_upperlim < tune_lowerlim) tune_upperlim = num_features
#	tune_upperlim = min(round(num_features/2), MAX_FEATURES)
#	tune_seq = seq(from=tune_lowerlim, to=tune_upperlim, by=5)	
	ctrl = makeTuneControlRandom(maxit = NUM_ITERS_TUNE)
	     
  #-----------------------------------------------------------------------------------------------------------------------------
  # PERFORMANCE MEASURES
  #-----------------------------------------------------------------------------------------------------------------------------
	test.mean_narm = makeAggregation(
		id = "test.mean_narm",
		name = "Test mean with NA removed",
		properties = "req.test",
		fun = function(task, perf.test, perf.train, measure, group, pred) mean(perf.test, na.rm = TRUE)
	)
	
	test.sd_narm = makeAggregation(
		id = "test.sd_narm",
		name = "Test sd with NA removed",
		properties = "req.test",
		fun = function(task, perf.test, perf.train, measure, group, pred) sd(perf.test, na.rm = TRUE)
	)
	
	testgroup.mean_narm = makeAggregation(
		id = "testgroup.mean",
		name = "Test group mean with NA removed",
		properties = "req.test",
		fun = function(task, perf.test, perf.train, measure, group, pred) {
			mean(vnapply(split(perf.test, group), mean), na.rm = TRUE)
		})

	testgroup.sd_narm = makeAggregation(
		id = "testgroup.sd",
		name = "Test group standard  with NA removed",
		properties = "req.test",
		fun = function(task, perf.test, perf.train, measure, group, pred) {
			sd(BBmisc::vnapply(split(perf.test, group), mean), na.rm = TRUE)
		})
		
	cindex.na = setAggregation(cindex, test.mean_narm)
	cindex.sdna = setAggregation(cindex, test.sd_narm)
	cindex_grp.na = setAggregation(cindex, testgroup.mean_narm)
	cindex_grp.sdna = setAggregation(cindex, testgroup.sd_narm)
	cindex.uno.na = setAggregation(cindex.uno, test.mean_narm)
	cindex.uno.sdna = setAggregation(cindex.uno, test.sd_narm)
	surv.measures = list(cindex.na, cindex.sdna, cindex_grp.na, cindex_grp.sdna, cindex.uno.na, cindex.uno.sdna)
    
  #-----------------------------------------------------------------------------------------------------------------------------
  # RESAMPLING STRATEGIES
  #-----------------------------------------------------------------------------------------------------------------------------
	if (run_stability) NUM_ITERS = NUM_ITERS_STAB
	inner = makeResampleDesc("RepCV", reps=NUM_ITERS, folds=NUM_FOLDS, stratify=TRUE)	  # Benchmarking: 5-fold CV repeated 5 times
	outer = makeResampleDesc("RepCV", reps=NUM_ITERS, folds=NUM_FOLDS, stratify=TRUE)	  # Benchmarking: 5-fold CV repeated 5 times
	outer_oats = makeResampleDesc("RepCV", reps=NUM_ITERS, folds=NUM_FOLDS_TUNE, stratify=TRUE)	  # Benchmarking: 3-fold CV repeated 5 times
	tuning = makeResampleDesc("CV", iters=NUM_ITERS, stratify=TRUE)											# Tuning: 5-fold CV, no repeats
	tuning_oats = makeResampleDesc("CV", iters = NUM_FOLDS_TUNE, stratify = TRUE)								# Tuning: 2-fold CV, no repeats
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

 
	#-----------------------------------------------------------------------------------------------------------------------------
	# BASE LEARNERS
	#-----------------------------------------------------------------------------------------------------------------------------	

	base_learners = list(
		"coxph" = list("class" = "surv.coxph",
									 "code" = LRN_COX,
									 "name" = "CoxPH",
									 "tune_params" = NULL,
									 "args" = NULL),
		"lasso" = list("class" = "surv.cvglmnet",
									 "code" = LRN_LASSO,
									 "name" = "Lasso",
									 "tune_params" = NULL,
									 "args" = list(alpha = 1, nfolds = 5)),
		"ridge" = list("class" = "surv.cvglmnet",
									 "code" = LRN_RIDGE,
									 "name" = "Ridge",
									 "tune_params" = NULL,
									 "args" = list(alpha = 0, nfolds = 5)),
		"elasticnet" = list("class" = "surv.cvglmnet",
									 "code" = LRN_ELASTICNET,
									 "name" = "ElasticNet",
									 "tune_params" = NULL,
									 "args" = list(alpha = 0.5, nfolds = 5)),
		"coxboost" = list("class" = "surv.cv.CoxBoost",
									"code" = LRN_COXBOOST,
									"name" = "CoxBoost",
									"tune_params" = NULL,
									"args" = NULL)
	)

	#-----------------------------------------------------------------------------------------------------------------------------
	# TUNING PARAMETERS
	#-----------------------------------------------------------------------------------------------------------------------------	

	ctrl = makeTuneControlRandom(maxit = NUM_ITERS_TUNE)
	
	xgbtree_params = makeParamSet(
		makeIntegerParam("nrounds", lower = 10, upper = 25),
		makeIntegerParam("max_depth", lower = 1, upper = 10),
		makeNumericParam("eta", lower = .01, upper = .4)
	)	
	
	xgblinear_params = makeParamSet(
		makeNumericParam("lambda", lower = 0, upper = 50)
	)
	
	tuning_params = list("filt_xgbtree" = xgbtree_params, "filt_xgblinear" = xgblinear_params)

  #------------------------------------------------------------------------------------------------------------------------------
	# Base Filters
  #------------------------------------------------------------------------------------------------------------------------------
	
	featsel_uni = list("filt_uni" = list("univariate.model.score" = list(perf.learner=cox_filt.lrn)))
	featsel_var = list("filt_var" = list("variance" = list()))
  featsel_ranger = list("filt_ranger" = list("ranger_permutation" = list(num.trees=1000, splitrule="maxstat", min.node.size=2)))
	featsel_rf = list("filt_rf_varimp" = list("randomForest_importance" = list(ntree = 1000, nsplit = 10, mtry = psqrt, nodesize=3)))
	featsel_rfmd = list("filt_rf_md" = list("randomForestSRC_var.select" = list(metho="md", ntree=1000, nsplit=10, nodesize=3, splitrule="logrank")))
	featsel_ridge = list("filt_ridge" = list("cv.glmnet" = list(alpha = 0, nfolds = 5L, filename=result_file)))
	featsel_elasticnet = list("filt_elasticnet" = list("cv.glmnet" = list(alpha = 0.5, nfolds = 5L, filename=result_file)))
	featsel_lasso = list("filt_lasso" = list("cv.glmnet" = list(alpha = 1, nfolds = 5L, filename=result_file)))
	featsel_glmboost = list("filt_glmboost" = list("glmboost" = list(filename=result_file)))
	featsel_cb = list("filt_coxboost" = list("cv.CoxBoost" = list(filename=result_file)))
#	featsel_xgbtree = list("filt_xgbtree" = list("XGBoost" = list(booster = "gbtree", nrounds = 20, max_depth = 10, eta = 1, filename=result_file)))
#	featsel_xgblin = list("filt_xgblinear" = list("XGBoost" = list(booster = "gblinear", lambda = 50, filename=result_file)))
	featsel_xgbtree = list("filt_xgbtree" = list("XGBoost" = list(booster = "gbtree", nrounds = 300, max_depth = 3, eta = 0.28, lambda = 0.2, filename=result_file)))
	featsel_xgblin = list("filt_xgblinear" = list("XGBoost" = list(booster = "gblinear", lambda = 3.3, filename=result_file)))

	featsel_uni_fse = list("filt_uni" = list("univariate.fse" = list(perf.learner=cox_filt.lrn, filename=result_file, perf.threshold = 0.0)))
	featsel_var_fse = list("filt_var" = list("variance.fse" = list(filename=result_file, perf.threshold = 0.0)))
  featsel_ranger_fse = list("filt_ranger" = list("ranger.fse" = list(num.trees=5000, splitrule="maxstat", min.node.size=10, filename=result_file, perf.threshold = 1.0)))
	featsel_rf_fse = list("filt_rf_varimp" = list("rfsrc.importance.fse" = list(ntree = 1000, nsplit = 10, mtry = psqrt, nodesize=3, filename=result_file, perf.threshold = 1.0)))
	featsel_rfmd_fse = list("filt_rf_md" = list("rfsrc.importance.fse" = list(metho="md", ntree=1000, nsplit=10, nodesize=3, splitrule="logrank", filename=result_file, perf.threshold = 1.0)))
	featsel_ridge_fse = list("filt_ridge" = list("cv.glmnet.fse" = list(alpha = 0, nfolds = 5L, filename=result_file, perf.threshold = 1.0)))
	featsel_elasticnet_fse = list("filt_elasticnet" = list("cv.glmnet.fse" = list(alpha = 0.5, nfolds = 5L, filename=result_file, perf.threshold = 1.0)))
	featsel_lasso_fse = list("filt_lasso" = list("cv.glmnet.fse" = list(alpha = 1, nfolds = 5L, filename=result_file, perf.threshold = 1.0)))
	featsel_glmboost_fse = list("filt_glmboost" = list("glmboost.fse" = list(filename=result_file, perf.threshold = 1.0)))
	featsel_cb_fse = list("filt_coxboost" = list("cv.CoxBoost.fse" = list(filename=result_file, perf.threshold = 1.0)))
#	featsel_xgbtree_fse = list("filt_xgbtree" = list("XGBoost.fse" = list(booster = "gbtree", nrounds = 20, max_depth = 10, eta = 1, filename=result_file, perf.threshold = 1.0)))
#	featsel_xgblin_fse = list("filt_xgblinear" = list("XGBoost.fse" = list(booster = "gblinear", lambda = 50, filename=result_file, perf.threshold = 1.0)))
	featsel_xgbtree_fse = list("filt_xgbtree" = list("XGBoost.fse" = list(booster = "gbtree", nrounds = 300, max_depth = 3, eta = 0.28, lambda = 0.2, filename=result_file, perf.threshold = 1.0)))
	featsel_xgblin_fse = list("filt_xgblinear" = list("XGBoost.fse" = list(booster = "gblinear", lambda = 3.3, filename=result_file, perf.threshold = 1.0)))
	
	featsel = c(featsel_uni, featsel_ranger, featsel_rf, featsel_rfmd, featsel_ridge, featsel_elasticnet, featsel_lasso, featsel_glmboost, featsel_cb, featsel_xgbtree, featsel_xgblin, featsel_var)
	featsel_fse = c(featsel_uni_fse, featsel_ranger_fse, featsel_rf_fse, featsel_rfmd_fse, featsel_ridge_fse, featsel_elasticnet_fse, featsel_lasso_fse, featsel_glmboost_fse, featsel_cb_fse, featsel_xgbtree_fse, featsel_xgblin_fse, featsel_var_fse)
	featsel_filters = c(featsel_uni, featsel_var, featsel_ranger, featsel_rf, featsel_rfmd, featsel_ridge, featsel_xgblin)
	featsel_filters_fse = c(featsel_uni_fse, featsel_var_fse, featsel_ranger_fse, featsel_rf_fse, featsel_rfmd_fse, featsel_ridge_fse, featsel_xgblin_fse)
	featsel_sparse = c(featsel_elasticnet, featsel_lasso, featsel_glmboost, featsel_cb, featsel_xgbtree, featsel_xgblin)
	featsel_sparse_fse = c(featsel_elasticnet_fse, featsel_lasso_fse, featsel_glmboost_fse, featsel_cb_fse, featsel_xgbtree_fse, featsel_xgblin_fse)
	
							 
  #------------------------------------------------------------------------------------------------------------------------------
	# Aggregators
  #------------------------------------------------------------------------------------------------------------------------------
	agg_mean = list("H-mean-rank")
	agg_value = list("H-mean-value")
	agg_rra = list("H-RRA")
	agg_freq = list("H-freq")
	agg_salience = list("H-salience")
	agg_cons = list("H-consrank")
	agg_union = list("H-union")
	agg_intersect = list("H-intersection")
	aggregators = list(agg_mean, agg_value, agg_freq, agg_rra)
	
	aggregators_rra = list("E-RRA")
	aggregators_wma = list("E-wma")
	aggregators_mean = list("E-mean")
	aggregators_freq = list("E-freq")
	agg_het = list("E-mean", "E-wma", "E-freq", "E-RRA")
	agg_het_sparse = list("E-freq", "E-union", "E-intersection")

							 
  #------------------------------------------------------------------------------------------------------------------------------
	# Thresholds
  #------------------------------------------------------------------------------------------------------------------------------
	cutoffs = list(log2(getTaskNFeats(surv.task))/getTaskNFeats(surv.task), 0.1, 0.25, 0.33, 0.5)
	cutoffs_auto = list(1.0)
	
	thresholds = list(
		list("auto_thresh" = "none", cutoff = cutoffs[[1]]),
		list("auto_thresh" = "none", cutoff = cutoffs[[2]]),
		list("auto_thresh" = "none", cutoff = cutoffs[[3]]),
		list("auto_thresh" = "mapdp", cutoff = cutoffs_auto[[1]]),
		list("auto_thresh" = "probe_best_agg", cutoff = cutoffs_auto[[1]]),
		list("auto_thresh" = "probe_best_raw", cutoff = cutoffs_auto[[1]]),
		list("auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]])
	)	
							 
  #------------------------------------------------------------------------------------------------------------------------------
	# Aggregator/Threshold Pairs
  #------------------------------------------------------------------------------------------------------------------------------
	agg_thresh = list(
		list("name" = "MR_fixed_log2n", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[1]]),
		list("name" = "MR_fixed_0.1", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[2]]),
		list("name" = "MR_fixed_0.25", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[3]]),
		list("name" = "MR_kde", "agg" = "H-mean-rank", "auto_thresh" = "kde", cutoff = cutoffs_auto[[1]]),
		list("name" = "MR_probe_best_agg", "agg" = "H-mean-rank", "auto_thresh" = "probe_best_agg", cutoff = cutoffs_auto[[1]]),
		list("name" = "MR_probe_mprobes", "agg" = "H-mean-rank", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
		list("name" = "MR_probe_mprobes_agg", "agg" = "H-mean-rank", "auto_thresh" = "probe_mprobes_agg_5", cutoff = cutoffs_auto[[1]]),
		list("name" = "MR_quantile50", "agg" = "H-mean-rank", "auto_thresh" = "quantile50", cutoff = cutoffs_auto[[1]]),
		list("name" = "MR_quantile25", "agg" = "H-mean-rank", "auto_thresh" = "quantile25", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_fixed_log2n", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs[[1]]),
		list("name" = "MV_fixed_0.1", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs[[2]]),
		list("name" = "MV_fixed_0.25", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs[[3]]),
		list("name" = "MV_kde", "agg" = "H-mean-value", "auto_thresh" = "kde", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_probe_best_agg", "agg" = "H-mean-value", "auto_thresh" = "probe_best_agg", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_probe_mprobes", "agg" = "H-mean-value", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_probe_mprobes_agg", "agg" = "H-mean-value", "auto_thresh" = "probe_mprobes_agg_5", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_quantile50", "agg" = "H-mean-value", "auto_thresh" = "quantile50", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_quantile25", "agg" = "H-mean-value", "auto_thresh" = "quantile25", cutoff = cutoffs_auto[[1]]),
		list("name" = "Freq_fixed_log2n", "agg" = "H-freq", "auto_thresh" = "none", cutoff = cutoffs[[1]]),
		list("name" = "Freq_fixed_0.1", "agg" = "H-freq", "auto_thresh" = "none", cutoff = cutoffs[[2]]),
		list("name" = "Freq_fixed_0.25", "agg" = "H-freq", "auto_thresh" = "none", cutoff = cutoffs[[3]]),
		list("name" = "Freq_kde", "agg" = "H-freq", "auto_thresh" = "kde", cutoff = cutoffs_auto[[1]]),
		list("name" = "Freq_probe_best_agg", "agg" = "H-freq", "auto_thresh" = "probe_best_agg", cutoff = cutoffs_auto[[1]]),
		list("name" = "Freq_probe_mprobes", "agg" = "H-freq", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
		list("name" = "Freq_probe_mprobes_agg", "agg" = "H-freq", "auto_thresh" = "probe_mprobes_agg_5", cutoff = cutoffs_auto[[1]]),
		list("name" = "Freq_quantile50", "agg" = "H-freq", "auto_thresh" = "quantile50", cutoff = cutoffs_auto[[1]]),
		list("name" = "Freq_quantile25", "agg" = "H-freq", "auto_thresh" = "quantile25", cutoff = cutoffs_auto[[1]]),
		list("name" = "RRA", "agg" = "H-RRA", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),
		list("name" = "MR_none", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_none", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),
		list("name" = "Freq_none", "agg" = "H-freq", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),
		list("name" = "Threshold", "agg" = "H-threshold", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),	
		list("name" = "Medrank", "agg" = "H-medrank", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),
		list("name" = "MR_quantile75", "agg" = "H-mean-rank", "auto_thresh" = "quantile75", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_quantile75", "agg" = "H-mean-value", "auto_thresh" = "quantile75", cutoff = cutoffs_auto[[1]]),
		list("name" = "Freq_quantile75", "agg" = "H-freq", "auto_thresh" = "quantile75", cutoff = cutoffs_auto[[1]]),
		list("name" = "MR_probe_mprobes_agg_10", "agg" = "H-mean-rank", "auto_thresh" = "probe_mprobes_agg_10", cutoff = cutoffs_auto[[1]]),	
		list("name" = "MV_probe_mprobes_agg_10", "agg" = "H-mean-value", "auto_thresh" = "probe_mprobes_agg_10", cutoff = cutoffs_auto[[1]]),
		list("name" = "MedThreshold", "agg" = "H-medthreshold", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),
		list("name" = "MR_fixed_0.33", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[4]]),
		list("name" = "MR_fixed_0.5", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[5]]),
		list("name" = "MV_fixed_0.33", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs[[4]]),
		list("name" = "MV_fixed_0.5", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs[[5]]),
		list("name" = "Freq_fixed_0.33", "agg" = "H-freq", "auto_thresh" = "none", cutoff = cutoffs[[4]]),
		list("name" = "Freq_fixed_0.5", "agg" = "H-freq", "auto_thresh" = "none", cutoff = cutoffs[[5]])
	)
	
	agg_thresh_fwer = list(
		list("name" = "MR_probe_mprobes", "agg" = "H-mean-rank", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
		list("name" = "MR_probe_mprobes_agg", "agg" = "H-mean-rank", "auto_thresh" = "probe_mprobes_agg", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_probe_mprobes", "agg" = "H-mean-value", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_probe_mprobes_agg", "agg" = "H-mean-value", "auto_thresh" = "probe_mprobes_agg", cutoff = cutoffs_auto[[1]]),
		list("name" = "Freq_probe_mprobes", "agg" = "H-freq", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
		list("name" = "Freq_probe_mprobes_agg", "agg" = "H-freq", "auto_thresh" = "probe_mprobes_agg", cutoff = cutoffs_auto[[1]])
	)	
		
	agg_thresh_freq = list(
		list("name" = "MR_freq",  "agg" = "H-mean-rank",  "auto_thresh" = "frequency", cutoff = cutoffs_auto[[1]]),
		list("name" = "MV_freq",  "agg" = "H-mean-value", "auto_thresh" = "frequency", cutoff = cutoffs_auto[[1]]),
		list("name" = "RRA_freq", "agg" = "H-RRA",        "auto_thresh" = "frequency", cutoff = cutoffs_auto[[1]])
	)
		
	agg_thresh_ind_sparse = list(
		list("name" = "ind",  "agg" = "",  "auto_thresh" = "none", cutoff = cutoffs_auto[[1]])
	)
		
	agg_thresh_ind_filter = list(
		list("name" = "ind_log2n",             "agg" = "", "auto_thresh" = "none",              cutoff = cutoffs[[1]]),
		list("name" = "ind_0.1",               "agg" = "", "auto_thresh" = "none",              cutoff = cutoffs[[2]]),
		list("name" = "ind_0.25",              "agg" = "", "auto_thresh" = "none",              cutoff = cutoffs[[3]]),
		list("name" = "ind_kde",               "agg" = "", "auto_thresh" = "kde",               cutoff = cutoffs_auto[[1]]),
		list("name" = "ind_0.33",              "agg" = "", "auto_thresh" = "none",              cutoff = cutoffs[[4]]),
		list("name" = "ind_0.5",               "agg" = "", "auto_thresh" = "none",              cutoff = cutoffs[[5]])
	)
			
	agg_thresh_het = list(
		list("name" = "het",  "agg" = "H-mean-rank",  "auto_thresh" = "none", cutoff = cutoffs[[2]])
	)
   	
#------------------------------------------------------------------------------------------------------------------	
# Wait for futures to be resolved before continuing to next step
#------------------------------------------------------------------------------------------------------------------	

	get_results = function(res, method, suffix) {
#		mod = getLearnerModel(res$models, more.unwrap = TRUE)
#		print(str(mod))
		perf = save_performance(perf, method, res)
		print("Final feature sets")
		print(res$extract)
		feats = save_features(feats, method, res$extract, NULL, length(res$models))
		stab = save_stability(stab, method, feats$featnames, getTaskNFeats(ohe.task))
		
		write_performance(perf, result_file, suffix)
		write_features(feats, result_file, suffix)
		write_stability(stab, result_file, suffix)
	}

	
# Step 1 - Test the stability and accuracy of each individual feature selector, without ensembling.
#        - This doesn't do any tuning as you can't tune the hyperparameters of filters in mlr2.
#				 - Do tuning first using survival code and manually plug in values into this code.
#------------------------------------------------------------------------------------------------------------------	
	test_individual = function(feat_selectors, resamp, base_learners, thresholds, impute = TRUE)
	{		
		print("Individual feature selectors")
		for (lrn in base_learners) {
			lrn = cpoScale() %>>% lrn
			for (i in 1:length(feat_selectors)) {
				for (thresh in thresholds) {
#					method = paste0(names(feat_selectors[i]), ENS_SEP, names(feat_selectors[[i]]), ENS_SEP, thresh$cutoff, ENS_SEP, thresh$auto_thresh, ENS_SEP, lrn$id)
					method = paste0(names(feat_selectors[i]), ENS_SEP, 'Individual', ENS_SEP, thresh$cutoff, ENS_SEP, thresh$auto_thresh, ENS_SEP, lrn$id)
					print(method)
					using_probes = length(grep("^probe_*", thresh$auto_thresh)) > 0
					fw  = makeFilterWrapper(learner = lrn, fw.method = "H-Individual", cache = FALSE, fw.perc = thresh$cutoff, 
																		fw.base.methods = NULL, fw.abs = NULL, fw.threshold = NULL, fw.fun = NULL,
																		autothresh = thresh$auto_thresh, filename = result_file,
																		base.method = names(feat_selectors[[i]]), base.args = feat_selectors[[i]])

					if (using_probes) {
						probe_data = add_probes(dataset)
						task = makeSurvTask(id = task_id, data = probe_data, target = c(time_var, status_var))
						fw = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.001) %>>% cpoDummyEncode() %>>% cpoProbe() %>>%  fw
					} else {
						task = surv.task
						fw = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.001) %>>% cpoDummyEncode()  %>>%  fw
					}					
		
					if (impute) 
						fw = cpoMice() %>>% fw
					res = resample(learner = fw, task = task, resampling = resamp, measures = surv.measures, models = TRUE, extract = getFilteredFeatures)							
					get_results(res, method, "ind")
					for (i in 1:length(res$extract)) {
						write_raw_features(result_file, "ind", setNames(rep(1, length(res$extract[[i]])), res$extract[[i]]))
					}
				}
			}
		}
  }

#Step 2 - Test the stability and accuracy of the homogeneous ensemble filters
# 			- Create a homogeneous filter from each individual feature selector
#------------------------------------------------------------------------------------------------------------------
	
	test_homogeneous_ens = function(feat_selectors, r_inner, r_outer, base_learners, thresholds, impute = TRUE)
	{	
		print("\n\nHomogeneous Ensembles\n")
		for (lrn in base_learners) {
			lrn = cpoScale() %>>% lrn
			for (i in 1:length(feat_selectors)) {
				for (thresh in thresholds) {
					print(paste0(thresh$agg, ", ", thresh$auto_thresh, ", ", thresh$cutoff))
					method = paste0(names(feat_selectors[i]), ENS_SEP, thresh$agg, ENS_SEP, thresh$cutoff, ENS_SEP, thresh$auto_thresh, ENS_SEP, lrn$id)
					print(method)
					using_probes = length(grep("^probe_*", thresh$auto_thresh)) > 0
					fw  = makeFilterWrapper(learner = lrn, fw.method = thresh$agg, cache = FALSE, fw.perc = thresh$cutoff,
																	fw.base.methods = NULL, fw.abs = NULL, fw.threshold = NULL, fw.fun = NULL,
																	resamp = r_inner, autothresh = thresh$auto_thresh, filename = result_file, 
																	base.method = names(feat_selectors[[i]]), base.args = feat_selectors[[i]], hom = TRUE)
					if (using_probes) {
						probe_data = add_probes(dataset)
						task = makeSurvTask(id = task_id, data = probe_data, target = c(time_var, status_var))
#						fw = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.001) %>>% cpoDummyEncode() %>>% cpoProbe() %>>%  fw
#						fw = cpoDummyEncode() %>>% cpoProbe() %>>%  fw
						fw = cpoDummyEncode() %>>% fw
					} else {
						task = surv.task
#						fw = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.001) %>>% cpoDummyEncode()  %>>%  fw
						fw = cpoDummyEncode()  %>>%  fw
					}
#					print(getTaskFeatureNames(surv.task))
					
					if (impute) 
						fw = cpoMice() %>>% fw
					res = resample(learner = fw, task = task, resampling = r_outer, measures = surv.measures, models = TRUE, extract = getFilteredFeatures)
					get_results(res, method, "hom")
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
					method = paste0(agg, ENS_SEP, ss, ENS_SEP, threshold_type, ENS_SEP, lrn$id)
					fw  = makeFilterWrapper(learner = lrn, fw.method = agg, fw.perc = ss, cache = FALSE,
																	fw.base.methods = feat_selectors, more.args = feat_args)
					fw = cpoMice() %>>% cpoDropConstants(ignore.na = TRUE, abs.tol = 0.01) %>>% cpoDummyEncode() %>>% fw
					res = resample(learner = fw, task = surv.task, resampling = resamp, measures = surv.measures, models = TRUE, extract = getFilteredFeatures)	
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
				task = makeSurvTask(id = task_id, data = probe_data, target = c(time_var, status_var))
				filt = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.01) %>>% cpoDummyEncode() %>>% cpoProbe() %>>%  filt
			} else {
				task = surv.task
				filt = cpoDropConstants(ignore.na = TRUE, abs.tol = 0.01) %>>% cpoDummyEncode()  %>>%  filt
			}
			
			if (impute) 
				filt = cpoMice() %>>% filt
			res = resample(learner = filt, task = task, resampling = resample_inst, measures = surv.measures, models = TRUE, extract = getFilteredFeatures)
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
										new_stab = calc_stability(ensemble_res[[i]]$extract, length(ensemble_res[[i]]$extract), getTaskNFeats(surv.task))
										if (best_value < new_stab[IND_STAB_CONS]) { 
											best_value = new_stab[IND_STAB_CONS]
											best_method = names(ensemble_res[i])
										}
									} else if (decision == "mean") {
										new_stab = calc_stability(ensemble_res[[i]]$extract, length(ensemble_res[[i]]$extract), getTaskNFeats(surv.task))
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
								stab = save_stability(stab, ens_name, feats$featnames, getTaskNFeats(surv.task))
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
#	ri = makeResampleInstance(desc = tuning, task = surv.task)
	if (task_id == "OATS")
		ri = makeResampleInstance(desc = outer_oats, task = surv.task)
	else
		ri = makeResampleInstance(desc = outer, task = surv.task)

	fs_index = as.integer(fs_ind)	# Passed in
	pbs_index = as.integer(Sys.getenv("PBS_ARRAY_INDEX"))
	agg_index = ((pbs_index - 1) %/% NUM_THRESHOLDS) + 1
	thresh_index = ((pbs_index - 1) %% NUM_THRESHOLDS) + 1
	print(paste("FS index = ", fs_index, ", PBS array index = ", pbs_index, ", agg_index = ", agg_index, ", thresh_index = ", thresh_index))

	file.create(paste0(result_file, "_ens.csv"))
	file.create(paste0(result_file, "_ind.csv"))

	if (ens_type == "het")
		test_stepwise_ensemble(featsel_fse, ri, agg_thresh_het[1], models_ridge, alpha, "stab", impute)
	else if (ens_type == "hom")
		test_homogeneous_ens(featsel_fse[fs_index], boot, ri, models_ridge, agg_thresh[pbs_index], impute)
	else if (ens_type == "ind") {
		if (fs_index %in% c(1, 2, 3, 5, 11, 12))
			test_individual(featsel_fse[fs_index], ri, models_ridge, agg_thresh_ind_filter[pbs_index], impute) 
		else
			test_individual(featsel_fse[fs_index], ri, models_ridge, agg_thresh_ind_sparse[1], impute)
	}
	

#---------------------------------------------------------------------------------------
# Clean up
#---------------------------------------------------------------------------------------    

  cat("\n\n")
  print(warnings())
  print(Sys.time())
}
