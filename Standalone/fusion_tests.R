library(mice)
library(mlr)
library(mlrCPO)
library(dplyr)
library(future)


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
source("performance_classfn.R")
source("stability.R")
source("mm_features.R")
source("learners.R")
source("feature_selectors.R")
source("data_helpers.R")


#
# Train each learner on each split of the CV resampling and make predictions
# No need to keep these trained models
# 
get_out_of_fold_predictions = function(tasks, learners, training_set, inner, target_var) 
{
		print("In get_out_of_fold_predictions")
		meta_data = list()		
		sub_task = subsetTask(tasks[[1]], subset = training_set)
		ri_inner = makeResampleInstance(inner, sub_task)
					
		for (rep in 1:1) {
			fold_responses = list()
			
			for (fold in 1:ri_inner$desc$iters) {
#				print(paste0("fold = ", fold))
				subset_idx = (rep - 1) * ri_inner$desc$iters + fold
				print(paste("Subset Index = ", subset_idx))
				predn_results = list()
				truth = NULL
			
				# Fire off training of each task in parallel
				model_futures = list()
				for (j in 1:length(tasks)) {
					task_idx = ifelse(length(tasks) == length(learners), j, 1L)	
					task_name = tasks[[task_idx]]$task.desc$id				
					print(paste0("Task idx = ", task_idx, ", Task name = ", task_name))
					sub_task = subsetTask(tasks[[task_idx]], subset = training_set)
					model_futures[[j]] = future(train(learner = learners[[j]], task = sub_task, subset = ri_inner$train.inds[[subset_idx]]))
				}
				
				# Wait for results
				resolve(model_futures)
				print("Models resolved")
				
				for (j in 1:length(model_futures)) {
					mod = value(model_futures[[j]])

					if (!is.null(mod)) {
						task_name = names(tasks)[[j]]	
						print(task_name)
#						raw_mod = getLearnerModel(mods, more.unwrap = TRUE)
#						feat_base[[j]]$save_multiclass(task_name, getFeatImpScores(raw_mod, classes), i, subset_idx)
						
#						print(paste0("Subsetting task ", j))
						dat = getTaskData(tasks[[j]])
						if (any(is.nan(dat))) {
							print("NaNs found!")
						}
						sub_task = subsetTask(tasks[[j]], subset = training_set)
						dat2 = getTaskData(sub_task)
						if (any(is.nan(dat2))) {
							print("NaNs found in dat2!")
						}
							
						print("Predicting")
						pred = predict(mod, task = sub_task, subset = ri_inner$test.inds[[subset_idx]])
						print(head(pred))
						pred_df = as.data.frame(pred$data[, grepl("prob.", colnames(pred$data))])
						print(head(pred_df))
						colnames(pred_df) = paste0(names(tasks)[[j]], ".", colnames(pred_df))
						pred_df[is.na(pred_df)] = 0
						print(head(pred_df))
						predn_results[[length(predn_results) + 1]] = pred_df
						if (is.null(truth))
						{
							truth = factor(pred$data$truth)
						}
					}
				}

				df = data.frame(predn_results)
				df[target_var] = truth
#				print(df)
				fold_responses[[length(fold_responses) + 1]] = df
			}
			
			meta_data[[rep]] = bind_rows(fold_responses)
			meta_data[[rep]] = meta_data[[rep]] %>% arrange(target_var)
		}
	
		meta = bind_rows(meta_data)
		return(meta)
}

#
# Train the base models on the outer training data set (i.e. 80% of whole dataset)
# Used later for evaluation of the base models
#
train_base_models = function(tasks, learners, training_set) 
{
	print("In train_base_models")
	model_futures = list()
	base_models = list()
	base_model_names = list()
	
	# Fire off training of each task in parallel
	for (i in 1:length(learners)) {
		task_idx = ifelse(length(tasks) == length(learners), i, 1L)
		model_futures[[i]] = future(train(learner = learners[[i]], task = tasks[[task_idx]], subset = training_set))
	}
	
	# Wait for results
#	print("Waiting...")
	resolve(model_futures)
#	print("All resolved")
	
	for (i in 1:length(model_futures)) {
		mod = value(model_futures[[i]])
		if (!is.null(mod)) {
			base_models[[length(base_models) + 1]] = mod		
			parts = strsplit(learners[[i]]$id, ".", fixed=TRUE)
			base_model_names = append(base_model_names, paste0(parts[[1]][1], "_", tasks[[task_idx]]$task.desc$id, "_", i))
		} else {
			print("MODEL FAILED")
		}
	}
	return(setNames(base_models, base_model_names))
}

#
# Train a learner on the results of the base learners
#
train_meta_learner = function(meta_learner, meta_data, target_var) 
{
	print("In train_meta_learner")

  meta_task = makeClassifTask(id = "MetaLearner", data = meta_data, target = target_var)
	metalrn = base_learners[[meta_learner]]
  lrn = do.call(makeLearner, args = append(list("cl" = metalrn$class, "id" = metalrn$name, "predict.type" = "prob"), metalrn$args))
	mod = train(lrn, meta_task)
	return(list("mod" = mod, "task" = meta_task))
}

#
# Evaluate base models on outer validation set
#
evaluate_base_models = function(tasks, learners, models, test_set, classes, roc_base, perf_base, stab_base)
{
#	print("In evaluate base_models")
	predn_futures = list()
	
	# Fire off prediction of each task in parallel
	for (i in 1:length(learners)) {
#		print(paste0("Model name: ", names(models)[[i]]))
#		print(paste0("Learner name: ", learners[[i]]$id))
		task_idx = ifelse(length(tasks) == length(learners), i, 1L)
		predn_futures[[i]] = future(predict(models[[i]], tasks[[task_idx]], subset = test_set))
	}
	
	# Wait for results
	resolve(predn_futures)
	
	for (i in 1:length(predn_futures)) {
		predn = value(predn_futures[[i]])	
#		roc_base[[i]]$calculate(names(models)[[i]], predn)
		roc_base[[i]]$calc(predn$data$truth, predn$data$response, classes)
		perf_base[[i]]$calculate(predn)
		stab_base[[i]]$save_features(getFilteredFeatures(models[[i]]))
	}
}

#
# Evaluate meta model
# Predict all base models on validation data and save predictions
# Predict meta learner on those predictions
evaluate_meta_learner = function(tasks, learners, base_models, meta_model, test_set, target_var)
{
	# Get predictions from base learners on each fold of validation data
#	print("In evaluate_meta_learner")
	meta_data = list()
	meta_data_names = list()

	for (i in 1:length(learners)) {
		task_idx = ifelse(length(tasks) == length(learners), i, 1L)
		task_name = tasks[[task_idx]]$task.desc$id				
		pred = predict(base_models[[i]], task = tasks[[task_idx]], subset = test_set)

		pred_df = as.data.frame(pred$data[, grepl("prob.", colnames(pred$data))])
		colnames(pred_df) = paste0(task_name, ".", colnames(pred_df))
		meta_data[[length(meta_data) + 1]] = pred_df
	}
	
	meta = data.frame(meta_data)
	meta[target_var] = factor(pred$data$truth)
#	print(meta)
	pred = predict(meta_model, newdata = meta)
	return(pred)
}

fusion_tests = function(config, res_index, cache_dir, big = FALSE, ovr_class = NULL, combn_set = NULL, subset = NULL, balance = FALSE, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE, validate = FALSE) 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print(Sys.time())
	configureMlr(show.learner.output = TRUE, on.learner.error = 'warn', on.par.without.desc = 'warn')	
	set.seed(24601, "L'Ecuyer")
	plan(multicore, workers = 10)

	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	#-----------------------------------------------------------------------------------------------------------------------------
	# DATA STRUCTURES TO COLLECT RESULTS
	#-----------------------------------------------------------------------------------------------------------------------------	
	tasks = create_tasks(data_dir, config, TASK_CLASSIF, ovr_class, big, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
	learners = create_learners(config, base_learners, base_filters, "prob", balance, cache_dir, subset)
	dat = getTaskData(tasks[[1]])
	classes = unique(dat[, config$targetVar])
	print(classes)

#	inner = makeResampleDesc("RepCV", reps = config$itersInner, folds = config$foldsInner, stratify = TRUE)
	inner = makeResampleDesc("CV", iters = config$foldsInner, stratify = TRUE)
	if (config$itersOuter < 2) {
		outer = makeResampleDesc("CV", iters = config$itersOuter, stratify = TRUE)
		ri_outer = makeResampleInstance(outer, tasks[[1]])
	} else {
		outer = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
		ri_outer = makeResampleInstance(outer, tasks[[1]])
	}
	
	# Train and evaluate the meta learner on different combinations of base tasks
	if (is.null(combn_set)) {
		combn_set = list(c(1:length(tasks)))
#		combn_set = list(c(1))
	}
#	print("combn_set:")
#	print(combn_set)

	roc_meta = list()
	perf_meta = list()
	stab_meta = list()
	feat_meta = list()
	predns = list()
	for (j in 1:length(combn_set)) {
		roc_meta[[j]] = ROCMultiClass$new(data_dir)
		perf_meta[[j]] = Performance$new(	classfn.measures)
		stab_meta[[j]] = Stability$new(classes)
		predns[[j]] = list()
	}

	 
	#-----------------------------------------------------------------------------------------------------------------------------
	# MODELING 
	#-----------------------------------------------------------------------------------------------------------------------------		
	for (rep in 1:outer$reps) {
#		print(paste("Rep = ", rep))

		for (fold in 1:outer$folds) {
#			print(paste0("fold = ", fold))
			subset_idx = (rep - 1) * outer$folds + fold
			print(paste("Subset Index = ", subset_idx))
			
			mods = train_base_models(tasks, learners, ri_outer$train.inds[[subset_idx]])
#			for (i in 1:length(mods)) {
#				print(paste0("Model ", i))
#				raw_mod = getLearnerModel(mods[[i]], more.unwrap = TRUE)
#				feat_meta[[i]]$save_multiclass(names(mods)[[i]], getFeatImpScores(raw_mod, classes), i, subset_idx)
#			}

#			evaluate_base_models(tasks, learners, mods, ri_outer$test.inds[[subset_idx]], classes, roc_base, perf_base, stab_base)

			# Generate meta data using all base tasks
			meta_data = get_out_of_fold_predictions(tasks, learners, ri_outer$train.inds[[subset_idx]], inner, config$targetVar)
			print("Meta data:")
			print(head(meta_data))
			
			for (j in 1:length(combn_set)) {
				task_set = names(tasks)[unlist(combn_set[[j]])]
				meta_data_sub = as.data.frame(meta_data[, grepl(paste(task_set, collapse="|"), colnames(meta_data))])
				meta_data_sub[, config$targetVar] = meta_data[, config$targetVar]
				rownames(meta_data_sub) = rownames(meta_data)
		
				meta_res = train_meta_learner(config$metaLearner, meta_data_sub, config$targetVar)
				raw_mod = getLearnerModel(meta_res$mod, more.unwrap = TRUE)
				if (subset_idx == 1) {
						feat_meta[[j]] =  MM_Features$new(list("meta" = meta_res$task))
				}		
				feat_meta[[j]]$save_multiclass("meta", getFeatImpScores(raw_mod, classes), 1, subset_idx, 'MeanDecreaseAccuracy')

				if (!is.null(meta_res$mod)) {
					pred = evaluate_meta_learner(tasks, learners, mods, meta_res$mod, ri_outer$test.inds[[subset_idx]], config$targetVar)			
					predns[[j]][[subset_idx]] = pred$data
#					roc_meta[[j]]$calculate(paste("meta", meta_res$mod$task.desc$id, sep="-"), pred, classes)
					roc_meta[[j]]$calc(pred$data$truth, pred$data$response, classes)
					perf_meta[[j]]$calculate(pred)
				}
			}
		}
	}

	# Aggregate, save and plot ROC results
	for (j in 1:length(combn_set)) {
#		print(paste("j = ", j))
		result_file = paste("meta", res_index, j, sep = "_")
		agg = roc_meta[[j]]$aggregate()
		
		all_responses = bind_rows(predns[[j]])
		print("All responses:")
		print(head(all_responses))
		write.csv(all_responses, paste(result_file, "_predns.csv", sep=""), row.names=FALSE, na="")
		roc_meta[[j]]$calc_mc_roc(as.factor(all_responses$truth), as.factor(all_responses$response))
		roc_meta[[j]]$write(result_file, "meta")
		roc_meta[[j]]$plot("meta", "meta", out_file = result_file)
		
		perf_meta[[j]]$write(result_file, "meta", "meta")
		feat_meta[[j]]$write(result_file)
		stab_meta[[j]]$save_df("meta", feat_meta[[j]]$featsel[["meta"]])
		stab_meta[[j]]$write(result_file)
	}
		
	print(warnings())
  print(Sys.time())
}