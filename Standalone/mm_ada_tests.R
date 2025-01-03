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
source("exploration.R")
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
source("mm_adaboost.R")
source("method.R")


#
# Run Multi-modal, multi-class Adaboost
# Decision, which determines how the predictions from the individual modalities are combined, can be one of
#		"vote" - simple majority vote
#		"prob" - sum the probabilities across each class and take the maximum
#		"meta" - train a learner on the results of the base models
# Error Type, which determines which samples are incorrect and are therefore upweighted, can be one of
#		"truth"  - Incorrect samples are those where the combined prediction from all modalities,
#							 (combination method specified by value of decision)does not match the ground-truth,
#		"absmaj" - Incorrect samples are those that are incorrect in more than half the modalities
#		"lowesterr"	 - Incorrect samples are those that are incorrect in the modality with the lowest error
#
ada_tests = function(config, res_index, cache_dir, decision = "vote", big = FALSE, ovr_class = NULL, subset = NULL, balance = FALSE, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE, validate = FALSE) 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print(Sys.time())
	configureMlr(show.learner.output = TRUE, on.learner.error = 'warn', on.par.without.desc = 'warn')	
	set.seed(24601, "L'Ecuyer")	
	print(paste0("Number of cores available: ", parallelly::availableCores()))
	plan(multicore)
	print(paste0("Number of parallel workers: ", nbrOfWorkers()))

	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	tasks = create_tasks(data_dir, config, TASK_CLASSIF, ovr_class, big, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
	print(names(tasks))
	prob_method = ifelse(decision == "prob", "prob", "response")
	learners = create_learners(config, base_learners, base_filters, prob_method, balance, cache_dir, subset)
	resamp = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
	ri = makeResampleInstance(resamp, tasks[[1]])
	
	dat = getTaskData(tasks[[1]])
	classes = factor(sort(unique(dat[, config$targetVar])))
	print(classes)
	
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
	print("Memory used:")
	print(mem_used())

	for (rep in 1:ri$desc$reps) {
#		print(paste("Rep = ", rep))

		fold_responses = list()
		for (fold in 1:ri$desc$folds) {
#			print(paste("Fold = ", fold))
			subset_idx = (rep - 1) * ri$desc$folds + fold
			print(paste("Subset Index = ", subset_idx))
			train_subset = ri$train.inds[[subset_idx]]
		  test_subset = ri$test.inds[[subset_idx]]
			label = config$targetVar
			sample_ids = data.frame('id' = train_subset, 
															'ID' = row.names(dat[train_subset, ]), 
															label = dat[train_subset, config$targetVar])

			# Train and predict using multi-modal Adaboost classifier
			mm_ada = MM_Adaboost$new(tasks, learners, config$boostIters, config$targetVar, config$metaLearner)			
			mm_ada$train(train_subset, classes, sample_ids, decision, config$targetVar)
			pred = mm_ada$predict(test_subset, classes, decision)
			
			fold_responses[[fold]] = pred$data
#			print(paste0("Fold response for fold ", fold))
#			print(fold_responses[[fold]])
			roc$calc(pred$data$truth, as.factor(pred$data$response), as.list(classes))
#			perf$save(performance(pred, classfn.measures))
			perf$calculate(pred)
			
			scores = mm_ada$get_feature_importances(classes)
			print(scores)
			for (tsk in names(scores)) {
				print(tsk)
				if (!is.null(scores[[tsk]])) {
					feats$save("adaboost", scores[[tsk]], tsk, subset_idx)
				}
			}
			rm(scores)
			rm(pred)
			rm(mm_ada)
		}
		
		# Combine the responses for each fold
		final_response[[rep]] = bind_rows(fold_responses)
		final_response[[rep]] = final_response[[rep]] %>% arrange(id)
		final_response[[rep]][, "rpt"] = rep
#		print(paste0("Final responses for repeat ", rep))
#		print(final_response[[rep]])
		rm(fold_responses)
	}
		
	# Combine the responses for each repeat into a single data frame
	all_responses = bind_rows(final_response)
#	print("All responses:")
#	print(all_responses)
	print("Memory used at end:")
	print(mem_used())

	# Aggregate, save and plot results
	result_file = paste("adaboost", res_index, sep = "_")
	print(result_file)
	write.csv(all_responses, paste(result_file, "_predns.csv", sep=""), row.names=FALSE, na="")
	roc$calc_mc_roc(as.factor(all_responses$truth), as.factor(all_responses$response))
	roc$write(result_file)
	roc$plot("adaboost", "Adaboost", paste(result_file, "_plot.csv", sep=""))
	perf$write(result_file)
	feats$write(result_file)
	stab$save_all("adaboost", feats$featsel)
	stab$write(result_file)
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}