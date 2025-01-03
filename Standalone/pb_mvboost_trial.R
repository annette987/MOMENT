library(mice)
library(mlr)
library(mlrCPO)
library(dplyr)
library(BBmisc)
library(pryr)

source("const.R")
source("config.R")
source("mice_cpo.R")
source("normalisation.R")
source("roc_measures.R")
source("perf_measures.R")
source("filter_boruta.R")
source("filter_sis.R")
source("performance_classfn.R")
source("predictions_classfn.R")
source("stability.R")
source("features2.R")
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
		print(task_id)
		dat = getTaskData(tasks[[i]])
		
		# Convert y to numeric
		y = as.numeric(as.character(dat[, target_var]))
		if (task_id == 'Clinical') {
			X = dat[, !colnames(dat) %in% c('Label', 'ID', 'DOB', 'Age', 'Gender', 'Ethnicity')]
		} else {
			X = dat[, !colnames(dat) %in% c('Label', 'ID')]
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
pb_mvboost_trial = function(config, res_index, cache_dir) 
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

	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	print(data_dir)
	tasks = create_tasks(data_dir, config, config$targetVar)
#	prob_method = ifelse(decision == "prob", "prob", "response")
	prob_method = "response"
	learners = create_learners(config, base_learners, base_filters, prob_method, cache_dir)
	
	#-----------------------------------------------------------------------------------------------------------------------------
	# DATA STRUCTURES TO COLLECT RESULTS
	#-----------------------------------------------------------------------------------------------------------------------------	

	roc = ROCMultiClass$new()
	perf = Performance$new(classfn.measures)
	stab = Stability$new(tasks)			
	feat = Features$new(tasks)
	predn = Prediction$new(tasks[[1]])
	 
	#-----------------------------------------------------------------------------------------------------------------------------
	# MODELING 
	#-----------------------------------------------------------------------------------------------------------------------------	
	final_response = list()
	num_iterations_PBMVboost = 10
	tree_depth = 2
	print("Memory used:")
	print(mem_used())

	train_subset = 1:150
	test_subset = 151:10150
	
	# Train and predict using PB_MVBoost classifier
	data = format_data(tasks, config$targetVar, train_subset, test_subset)
	print(names(tasks))
	print("Training data for view 1:")
	print(data$y_train[[1]])
	print(class(data$y_train[[1]]))
	print("Task data:")
	d = getTaskData(tasks[[1]])
	print(d$Label[train_subset])
	print(class(d$Label[train_subset]))
	pbmvboost = PB_MVBoost$new(data$X_train, data$y_train, data$X_test, data$y_test, 
															views = names(tasks),
															tasks, 
															learners,
															config$baseModels,
															prob_method,
															cache_dir,
															num_iterations = num_iterations_PBMVboost,
															decision_tree_depth = tree_depth)
	pbmvboost$learn(train_subset, test_subset, base_learners, base_filters)

		
	print("Memory used at end:")
	print(mem_used())
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}