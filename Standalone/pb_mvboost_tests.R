library(mice)
library(mlr)
library(mlrCPO)
library(dplyr)
library(BBmisc)
library(pryr)

source("const.R")
source("config.R")
source("imputation.R")
source("normalisation.R")
source("roc_measures.R")
source("perf_measures.R")
source("filter_boruta.R")
source("filter_sis.R")
source("filter_vsurf.R")
source("filter_rfsrc.R")
source("performance_classfn.R")
source("predictions_classfn.R")
source("stability.R")
source("mm_features.R")
source("learners.R")
source("feature_selectors.R")
source("data_helpers.R")
source("models.R")
source("PB_MVBoost.R")

#
# Format the data for one fold as PB_MVBoost expects it:
#   4 named lists with one element per view
# 
format_data = function(tasks, target_var, train_subset, test_subset) {
	X_train = list()
	y_train = list()
	X_test = list()
	y_test = list()
	
	for (i in 1:length(tasks)) {
		task_id = tasks[[i]]$task.desc$id
		dat = getTaskData(tasks[[i]])
		
		# Convert y to numeric
		y = dat[, target_var]
		if (task_id == 'Clinical') {
			X = dat[, !colnames(dat) %in% c(target_var, 'ID', 'DOB', 'Age', 'Gender', 'Ethnicity')]
		} else {
			X = dat[, !colnames(dat) %in% c(target_var, 'ID')]
		}
			
		X_train[[task_id]] = X[train_subset, ]
		X_test[[task_id]]  = X[test_subset, ]
		y_train[[task_id]] = y[train_subset]
		y_test[[task_id]]  = y[test_subset]
	}	
	return(list("X_train" = X_train,
							"y_train" = y_train,
							"X_test" = X_test,
							"y_test" = y_test))
}

#
# Run PB_MVBoost: Pac-Bayes Multi-view Boosting
#
pb_mvboost_tests = function(config, res_index, cache_dir, big = FALSE, ovr_class = NULL, subset = NULL, balance = FALSE, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE, validate = FALSE) 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print(Sys.time())
	configureMlr(show.learner.output = TRUE, 
							 on.error.dump = TRUE, 
							 on.learner.error = 'warn', 
							 on.par.without.desc = 'warn')	
	set.seed(24601, "L'Ecuyer")	
	plan(multicore, workers = 10)

	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	tasks = create_tasks(data_dir, config, TASK_CLASSIF, ovr_class, big, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
#	prob_method = ifelse(decision == "prob", "prob", "response")
	prob_method = "response"
	learners = create_learners(config, base_learners, base_filters, prob_method, balance, cache_dir, subset)

	resamp = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
	ri = makeResampleInstance(resamp, tasks[[1]])
	dat = getTaskData(tasks[[1]])
	print(tasks[[1]])
	classes = unique(dat[, config$targetVar])
	
	#-----------------------------------------------------------------------------------------------------------------------------
	# DATA STRUCTURES TO COLLECT RESULTS
	#-----------------------------------------------------------------------------------------------------------------------------	

	roc = ROCMultiClass$new()
	perf = Performance$new(classfn.measures)
	stab = Stability$new(classes)			
	feats = MM_Features$new(tasks)
	predn = Prediction$new(tasks[[1]])
	 
	#-----------------------------------------------------------------------------------------------------------------------------
	# MODELING 
	#-----------------------------------------------------------------------------------------------------------------------------	
	final_response = list()
	tree_depth = 2
	print("Memory used:")
	print(mem_used())

	for (rep in 1:ri$desc$reps) {
		print(paste("Rep = ", rep))

		fold_responses = list()
		for (fold in 1:ri$desc$folds) {
			print(paste("Fold = ", fold))
			subset_idx = (rep - 1) * ri$desc$folds + fold
			print(paste("Subset Index = ", subset_idx))
			train_subset = ri$train.inds[[subset_idx]]
			test_subset = ri$test.inds[[subset_idx]]
			print("Training set:")
			print(train_subset)
			print("Test set:")
			print(test_subset)
			
			# Train and predict using PB_MVBoost classifier
			data = format_data(tasks, config$targetVar, train_subset, test_subset)
			pbmvboost = PB_MVBoost$new(data$X_train, data$y_train, data$X_test, data$y_test, 
																	views = names(tasks),
																	tasks, 
																	learners,
																	config$baseModels,
																	prob_method,
																	cache_dir,
																	num_iterations = config$boostIters,
																	decision_tree_depth = tree_depth)
			results = pbmvboost$learn(train_subset, test_subset, base_learners, base_filters, classes)
			roc$calc(results$truth, as.factor(results$response), as.list(classes))

			scores = pbmvboost$get_feature_importances(classes)
			for (tsk in names(scores)) {
				print("Saving features")
				feats$save("pbmvboost", scores[[tsk]], tsk, subset_idx)
			}

			fold_result = data.frame('id' = test_subset, 
															 'ID' = row.names(dat[test_subset, ]), 
															 'truth' = results$truth,
															 'response' = results$response)
			fold_responses[[fold]] = fold_result
		}
		
		# Combine the responses for each fold
		final_response[[rep]] = bind_rows(fold_responses)
		final_response[[rep]] = final_response[[rep]] %>% arrange(id)
		final_response[[rep]][, "rpt"] = rep
#		print(paste0("Final responses for repeat ", rep))
#		print(final_response[[rep]])
	}
		
	# Combine the responses for each repeat into a single data frame
	all_responses = bind_rows(final_response)
#	print("All responses:")
#	print(all_responses)
	print("Memory used at end:")
	print(mem_used())

	# Aggregate, save and plot results
	result_file = paste("pbmv", res_index, sep = "_")
	write.csv(all_responses, paste(result_file, "_predns.csv", sep=""), row.names=FALSE, na="")
	roc$calc_mc_roc(as.factor(all_responses$truth), as.factor(all_responses$response))
	roc$write(result_file)
	roc$plot("pbmvboost", "PBMVBoost", paste(result_file, "_plot.csv", sep=""))
	feats$write(result_file)
	stab$save_all("pbmvboost", feats$featsel)
	stab$write(result_file)
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}