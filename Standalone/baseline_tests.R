library(mice)
library(mlr)
library(mlrCPO)
library(dplyr)


source("const.R")
source("config.R")
source("mice_cpo.R")
source("normalisation.R")
source("roc_measures.R")
source("perf_measures.R")
source("filter_boruta.R")
source("filter_sis.R")
source("performance_classfn.R")
source("stability.R")
source("features.R")
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
#				print(paste("Subset Index = ", subset_idx))
				predn_results = list()
				truth = NULL
			
				for (j in 1:length(tasks)) {
#					print(paste0("j = ", j))
					task_idx = ifelse(length(tasks) == length(learners), j, 1L)	
					task_name = tasks[[task_idx]]$task.desc$id				
					sub_task = subsetTask(tasks[[task_idx]], subset = training_set)

					mod = train(learner = learners[[j]], task = sub_task, subset = ri_inner$train.inds[[subset_idx]])
					if (!is.null(mod)) {
						pred = predict(mod, task = sub_task, subset = ri_inner$test.inds[[subset_idx]])
						pred_df = as.data.frame(pred$data[, grepl("prob.", colnames(pred$data))])
						colnames(pred_df) = paste0(task_name, ".", colnames(pred_df))
						predn_results[[j]] = pred_df
						if (is.null(truth))
						{
							truth = factor(pred$data$truth)
						}
					}
				}

				df = data.frame(predn_results)
				df[target_var] = truth
				fold_responses[[length(fold_responses) + 1]] = df
			}
			
			meta_data[[rep]] = bind_rows(fold_responses)
			meta_data[[rep]] = meta_data[[rep]] %>% arrange(target_var)
		}
	
		meta = bind_rows(meta_data)
#		print("End of all iterations - Meta data:")
#		print(meta)
		return(meta)
}

#
# Train the base models on the outer training data set (i.e. 80% of whole dataset)
# Used later for evaluation of the base models to compare to super learner
#
train_base_models = function(tasks, learners, training_set) 
{
	print("In train_base_models")
	base_models = list()
	base_model_names = list()
	for (i in 1:length(learners)) {
		task_idx = ifelse(length(tasks) == length(learners), i, 1L)
#		print(tasks[[task_idx]]$task.desc$id)
		mod = train(learner = learners[[i]], task = tasks[[task_idx]], subset = training_set)
		if (!is.null(mod)) {
			base_models[[length(base_models) + 1]] = mod		
			parts = strsplit(learners[[i]]$id, ".", fixed=TRUE)
			base_model_names = append(base_model_names, paste0(parts[[1]][1], "_", tasks[[task_idx]]$task.desc$id, "_", i))
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
	return(mod)
}

#
# Evaluate base models on outer validation set
#
evaluate_base_models = function(tasks, learners, models, test_set, roc_base, perf_base, stab_base, feat_base)
{
	print("In evaluate base_models")
	for (i in 1:length(learners)) {
#		print(paste0("Model name: ", names(models)[[i]]))
#		print(paste0("Learner name: ", learners[[i]]$id))
		task_idx = ifelse(length(tasks) == length(learners), i, 1L)
		pred = predict(models[[i]], tasks[[task_idx]], subset = test_set)
		roc_base[[i]]$calculate(names(models)[[i]], pred)
		perf_base[[i]]$calculate(pred)
		stab_base[[i]]$save_features(getFilteredFeatures(models[[i]]))
#		feat_base[[i]]$save(tasks[[task_idx]]$task.desc$id, getFilteredFeatures(models[[i]]), NULL, length(models[[i]]))
	}
}

#
# Evaluate meta model
# Predict all base models on validation data and save predictions
# Predict meta learner on those predictions
evaluate_meta_learner = function(tasks, learners, base_models, measures, meta_model, test_set, target_var, roc_meta, perf_meta, stab_meta, class_names)
{
	# Get predictions from base learners on each fold of validation data
	print("In evaluate_meta_learner")
	meta_data = list()
	meta_data_names = list()

	for (i in 1:length(learners)) {
#		print(paste0("i = ", i))
		task_idx = ifelse(length(tasks) == length(learners), i, 1L)
		task_name = tasks[[task_idx]]$task.desc$id				
		pred = predict(base_models[[i]], task = tasks[[task_idx]], subset = test_set)

		pred_df = as.data.frame(pred$data[, grepl("prob.", colnames(pred$data))])
		colnames(pred_df) = paste0(task_name, ".", colnames(pred_df))
		meta_data[[length(meta_data) + 1]] = pred_df
	}
	
	meta = data.frame(meta_data)
	meta[target_var] = factor(pred$data$truth)

	pred = predict(meta_model, newdata = meta)
	roc_meta$calculate(paste("meta", meta_model$task.desc$id, sep="-"), pred, class_names)
	perf_meta$calculate(pred, measures)
	stab_meta$save_features(getFilteredFeatures(meta_model))
}

fusion_tests = function(config, res_index, cache_dir) 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print(Sys.time())
	configureMlr(show.learner.output = TRUE, on.learner.error = 'warn', , on.par.without.desc = 'warn')	
	set.seed(24601, "L'Ecuyer")	

	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	#-----------------------------------------------------------------------------------------------------------------------------
	# DATA STRUCTURES TO COLLECT RESULTS
	#-----------------------------------------------------------------------------------------------------------------------------	
	tasks = create_tasks(data_dir, config, config$targetVar)
	learners = create_learners(config, base_learners, base_filters, "prob", cache_dir)

#	inner = makeResampleDesc("RepCV", reps = config$itersInner, folds = config$foldsInner, stratify = TRUE)
	inner = makeResampleDesc("CV", iters = config$foldsInner, stratify = TRUE)
	if (config$itersOuter < 2) {
		outer = makeResampleDesc("CV", iters = config$itersOuter, stratify = TRUE)
		ri_outer = makeResampleInstance(outer, tasks[[1]])
	} else {
		outer = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
		ri_outer = makeResampleInstance(outer, tasks[[1]])
	}

	roc_meta = ROCMultiClass$new()
	perf_meta = Performance$new(prob.measures)
	stab_meta = Stability$new(tasks)
	roc_base = list()
	perf_base = list()
	stab_base = list()
	feat_base = list()
	for (i in 1:length(learners)) {
			roc_base[[i]] = ROCMultiClass$new()
			perf_base[[i]] = Performance$new(prob.measures)
			stab_base[[i]] = Stability$new(tasks)			
			feat_base[[i]] = Features$new(tasks)
	}
	 
	#-----------------------------------------------------------------------------------------------------------------------------
	# MODELING 
	#-----------------------------------------------------------------------------------------------------------------------------	
	
	for (rep in 1:outer$reps) {
#		print(paste("Rep = ", rep))

		for (fold in 1:outer$folds) {
#			print(paste0("fold = ", fold))
			subset_idx = (rep - 1) * outer$folds + fold
#			print(paste("Subset Index = ", subset_idx))
			
			mods = train_base_models(tasks, learners, ri_outer$train.inds[[subset_idx]])
#			evaluate_base_models(tasks, learners, mods, ri_outer$test.inds[[subset_idx]], roc_base, perf_base, stab_base, feat_base)
			meta_data = get_out_of_fold_predictions(tasks, learners, ri_outer$train.inds[[subset_idx]], inner, config$targetVar)
			meta_model = train_meta_learner(config$metaLearner, meta_data, config$targetVar)
			if (!is.null(meta_model)) {
				evaluate_meta_learner(tasks, learners, mods, prob.measures, meta_model, ri_outer$test.inds[[subset_idx]], config$targetVar, roc_meta, perf_meta, stab_meta, c("CIR", "CON", "LN", "LX"))
			}
		}
	}

	# Aggregate, save and plot ROC results
	result_file = paste(config$resultFile, res_index, sep = "_")
	agg = roc_meta$aggregate()
	roc_meta$write(result_file, "meta")
	roc_meta$plot("meta", "meta", out_file = result_file)
	perf_meta$write(result_file, "meta", "meta")
#	perf_meta$plot(result_file, "meta")
	stab_meta$save("meta", stab_meta$feature_sets, length(tasks))
	stab_meta$write(result_file, "meta")

	for (i in 1:length(tasks)) {
		agg = roc_base[[i]]$aggregate()
		model_id = strsplit(learners[[i]]$id, ".", fixed = TRUE)[[1]][1]
#		print(paste0("Learner ID: ", learners[[i]]$id))
#		print(paste0("Model ID: ", model_id))
		roc_base[[i]]$write(result_file, model_id)
		roc_base[[i]]$plot(model_id, tasks[[i]]$task.desc$id, result_file)
		perf_base[[i]]$write(result_file, model_id, model_id)
		stab_base[[i]]$save(model_id, stab_base[[i]]$feature_sets, getTaskNFeats(tasks[[i]]))
		stab_base[[i]]$write(result_file, model_id)
#		feat_base[[i]]$write(result_file, model_id)
	}
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}