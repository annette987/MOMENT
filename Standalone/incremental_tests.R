library(mice)
library(mlr)
library(mlrCPO)
library(dplyr)


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
source("stability.R")
source("features.R")
source("learners.R")
source("feature_selectors.R")
source("data_helpers.R")
source("incremental.R")


incremental_tests = function(config, res_index, cache_dir, metric = "f1", exp = "COMBN", decision = 'vote', big = FALSE, ovr_class = NULL, subset = NULL, balance = FALSE, direction = "REV") 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print("In incremental tests")
	print(Sys.time())
	configureMlr(show.learner.output = TRUE, on.learner.error = 'warn', on.par.without.desc = 'warn')	
	set.seed(24601, "L'Ecuyer")	

	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	tasks = create_tasks(data_dir, config, TASK_CLASSIF, ovr_class, big, subset)
	prob_method = ifelse(decision == "prob", "prob", "response")
	learners = create_learners(config, base_learners, base_filters, prob_method, balance, cache_dir, subset)
	resamp = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
	ri = makeResampleInstance(resamp, tasks[[1]])
	dat = getTaskData(tasks[[1]])
	classes = unique(dat[, config$targetVar])
	 
	#-----------------------------------------------------------------------------------------------------------------------------
	# MODELING 
	#-----------------------------------------------------------------------------------------------------------------------------		
	result_file = paste("inc", res_index, sep = "_")
	roc = ROCMultiClass$new()
	
	inc = Incremental$new(tasks, learners, config$targetVar, classes, metric)
	inc$train_base_models(ri, decision)
	print("Performance of base models:")
	print(inc$perf)
	if (direction == "FWD") {
		best_set = inc$train_incremental_forward(config, tasks, metric, exp, decision, result_file)
		print("Best set forward:")
		print(best_set)
	} else if (direction == "REV") {
		inc_rev = inc$train_incremental_reverse(config, tasks, metric, exp, decision, result_file)
		print("Best set reverse:")
		print(inc_rev$set)
		print("Order of removal:")
		print(inc_rev$results)
		write.csv(inc_rev$results, paste(result_file, "_results.csv"))
	}
	
	final_resp = inc$get_response(inc$predns, unlist(inc_rev$set), exp, decision, config)
	for (rpt in 1:ri$desc$reps) {
		rpt_resp = final_resp[final_resp$rpt == rpt, ]
		roc$calc(rpt_resp$truth, rpt_resp$response, as.list(classes))
	}
		
	roc$calc_mc_roc(as.factor(final_resp$truth), as.factor(final_resp$response))
	roc$write(result_file)
	roc$plot("combn", "Combination", paste(result_file, "_plot.csv", sep=""))
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}