library(mice)
library(mlr)
library(mlrCPO)
library(dplyr)
library(tryCatchLog)


source("const.R")
source("config.R")
source("imputation.R")
source("normalisation.R")
source("balance.R")
source("roc_measures.R")
source("perf_measures.R")
source("filter_boruta.R")
source("filter_sis.R")
source("performance_classfn.R")
source("stability.R")
source("mm_features.R")
source("learners.R")
source("feature_selectors.R")
source("data_helpers.R")


# Calculate the final response, according to the decision_type type and
# add a response column to the results
#
get_final_decision = function(results, classes, decision_type) {
	print("In get_final_decision")
	print(classes)
	results$truth = as.factor(results$truth)
	if (decision_type == 'vote') {
		# Calculate final prediction with a majority vote across modalities
		raw_responses = as.data.frame(results[,!colnames(results) %in% c('id', 'ID', 'truth')])
		results$response = as.factor(apply(raw_responses, 1, function(x) names(which.max(table(x)))))	
	} else if (decision_type == 'prob') {
		# Calculate sum of probabilities for each class and take max of that as prediction
		for (i in 1:length(classes)) {
#			print(classes[i])
#			print(colnames(results)[grepl(paste0("\\<", classes[i], "\\>"), colnames(results))])
			results[, paste0('prob.', classes[i])] = rowSums(as.data.frame(results[, grepl(paste0("\\<", classes[i], "\\>"), colnames(results))]), na.rm = TRUE)
		}
		results$response = apply(results[, grepl("prob.", colnames(results))], 1, function(x) names(which.max(x)))
		if (!is.null(results$response)) {
			results$response = strsplit(as.character(results$response), ".", fixed = TRUE)
			results$response = as.factor(sapply(results$response, "[[", 2))
		} else {
			print("Response is NULL!")
			print(results)
		}
	}
	
	levels(results$response) = levels(results$truth)
	#	print(head(results))
	return(results)
}

#
# Train the  models on each fold of the training data 
# Then predict on the test data for that fold
# Gives a set of predictions: 1 per fold per modality
#
train_base_mods = function(tasks, learners, ri, fold_num, decision, classes, feats, perf) 
{
	print(paste0("In train_base_mods, fold ", fold_num))
	responses = NULL
	training_set = ri$train.inds[[fold_num]]
	test_set = ri$test.inds[[fold_num]]
	print("Training set: ")
	print(training_set)
	print("Test set:")
	print(test_set)
			
	for (i in 1:length(tasks)) {
		lrn_idx = ifelse(length(tasks) == length(learners), i, 1L)
		print(lrn_idx)
		task_id = tasks[[i]]$task.desc$id
		mod = train(learner = learners[[lrn_idx]], task = tasks[[i]], subset = training_set)
	
		if (isFailureModel(mod)) {
			print(paste0("Model ", task_id, " failed on iteration ", fold_num))
			print(getFailureModelMsg(mod))
			feats$save("combn", NULL, task_id, fold_num)
		} else {
		
			# Save feature importance scores
			scores = getFeatImpScores(getLearnerModel(mod, more.unwrap = TRUE), levels(classes))
			print(head(scores))
			selected = getFilteredFeatures(getLearnerModel(mod, more.unwrap = FALSE))
			not_selected = setdiff(getTaskFeatureNames(tasks[[i]]), selected)
			feat_scores = scores[, "all"]
			names(feat_scores) = rownames(scores)
			if (length(not_selected) > 0) {
				feat_scores[not_selected] = 0
				names(feat_scores[not_selected]) = not_selected
			}
			feats$save("combn", feat_scores, task_id, fold_num)

			# Prediction
			print("Predicting...")
			pred = predict(mod, tasks[[i]], subset = test_set)
			if (is.null(responses)) {
				responses = pred$data[, c('id', 'truth')]
				responses$ID = rownames(pred$data)
			}
			if ((decision == 'vote') || (decision == 'hard')) {
				res = pred$data[, 'response', drop = FALSE]
				res$ID = rownames(pred$data)
				responses[, task_id] = res[match(responses$ID, res$ID), 'response']
			} else if ((decision == 'prob') || (decision == 'soft')) {
#				probs = pred$data[, grepl("prob.", colnames(pred$data))]
#				probs$ID = rownames(pred$data)
#				prob_cols = paste0("prob.", levels(classes))
#				responses[, paste0(task_id, ".", levels(classes))] = probs[match(responses$ID, probs$ID), prob_cols, drop = FALSE]
				
				probs = pred$data[, grepl("prob.", colnames(pred$data))]
				prob_cols = colnames(probs)
				new_cols = gsub("prob", task_id, colnames(probs))
				probs$ID = rownames(pred$data)
				responses[, new_cols] = probs[match(responses$ID, probs$ID), prob_cols, drop = FALSE]
			}
		}
	}
	return(as.data.frame(responses))
}


combn_tests = function(config, res_index, cache_dir, decision = "vote", big = FALSE, ovr_class = NULL, subset = NULL, balance = FALSE, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE, validate = FALSE)
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print(Sys.time())
	print(balance)
	configureMlr(show.learner.output = TRUE, on.learner.error = 'warn', on.par.without.desc = 'warn')	
	set.seed(24601, "L'Ecuyer")	

	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	tasks = create_tasks(data_dir, config, TASK_CLASSIF, ovr_class, big, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
	dat = getTaskData(tasks[[1]])
	classes = as.factor(unique(dat[, config$targetVar]))
	print("Classes:")
	print(classes)
	learners = create_learners(config, base_learners, base_filters, "prob", balance, cache_dir, subset)

	resamp = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
	ri = makeResampleInstance(resamp, tasks[[1]])
	print(tasks[[1]])
	print(ri)
	row_names = ifelse(is.null(config$rowNames) || is.na(config$rowNames), "ID", config$rowNames)

	#-----------------------------------------------------------------------------------------------------------------------------
	# DATA STRUCTURES TO COLLECT RESULTS
	#-----------------------------------------------------------------------------------------------------------------------------	
	roc = ROCMultiClass$new()
	perf = Performance$new(classfn.measures)
	stab = Stability$new(classes)			
	feats = MM_Features$new(tasks)
	 
	#-----------------------------------------------------------------------------------------------------------------------------
	# MODELING 
	#-----------------------------------------------------------------------------------------------------------------------------		
	final_response = list()
	
	for (rep in 1:ri$desc$reps) {
		print(paste("Rep = ", rep))

		fold_responses = list()
		for (fold in 1:ri$desc$folds) {
			print(paste("Fold = ", fold))
			subset_idx = (rep - 1) * ri$desc$folds + fold
			print(paste("Subset Index = ", subset_idx))
			responses = train_base_mods(tasks, learners, ri, subset_idx, decision, classes, feats, perf)
			responses = get_final_decision(responses, classes, decision)
			pred_resp = make_mlr_prediction(responses, tasks[[1]])
			perf$calculate(pred_resp)
			fold_responses[[fold]] = responses
#			print(paste0("Fold response for fold ", fold))
#			print(fold_responses[[fold]])
			roc$calc(responses$truth, responses$response, as.list(classes))
		}
		
		# Combine the responses for each fold
		final_response[[rep]] = bind_rows(fold_responses)
#		print(final_response[[rep]])
#		final_response[[rep]] = final_response[[rep]] %>% arrange(id)
		final_response[[rep]][, "rpt"] = rep
#		print(paste0("Final responses for repeat ", rep))
#		print(final_response[[rep]])
	}

	# Aggregate, save and plot results
	all_responses = bind_rows(final_response)
	result_file = paste("combn", res_index, sep = "_")
	write.csv(all_responses, paste(result_file, "_predns.csv", sep=""), row.names=FALSE, na="")
	roc$calc_mc_roc(as.factor(all_responses$truth), as.factor(all_responses$response))
	roc$write(result_file)
	roc$plot("combn", "Combination", paste(result_file, "_plot.csv", sep=""))
	perf$write(result_file)
	feats$write(result_file)
	
	stab$save_all("combn", feats$featsel)
	stab$write(result_file)
	
	if (validate) {
		print("Validation")
		vtasks = create_validation_tasks(tasks, data_dir, config, TASK_CLASSIF, ovr_class, big, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
		vdat = getTaskData(vtasks[[1]])
		print(paste0("Number of rows in validation tasks = ", nrow(vdat)))
		ri_v = makeFixedHoldoutInstance(c(1:nrow(dat)), c((nrow(dat) + 1):nrow(vdat)), nrow(vdat))
		
		vroc = ROCMultiClass$new()
		vperf = Performance$new(classfn.measures)
		vfeats = MM_Features$new(vtasks)

		vresponses = train_base_mods(vtasks, learners, ri_v, 1, decision, classes, vfeats, vperf)
		vresponses = get_final_decision(vresponses, classes, decision)
		vroc$calc(vresponses$truth, vresponses$response, as.list(classes))
		vroc$calc_mc_roc(as.factor(vresponses$truth), as.factor(vresponses$response))

		vpred_resp = make_mlr_prediction(vresponses, vtasks[[1]])
		vperf$calculate(vpred_resp)
		
		vresult_file = paste0(result_file, "_validate")
		vroc$write(vresult_file)
		vroc$plot("combn", "Validation", paste(result_file, "_validate_plot.csv", sep=""))
		vperf$write(vresult_file)
		vfeats$write(vresult_file)
	}
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}