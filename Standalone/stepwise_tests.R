library(mice)
library(mlr)
library(mlrCPO)
library(dplyr)
library(BBmisc)

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
source("features.R")
source("learners.R")
source("feature_selectors.R")
source("data_helpers.R")

#
# Return all but the last n characters in a string
#
substrLeft = function(x, n){
  substr(x, 1, nchar(x)-n)
}

#
# Create new tasks for each dataset for the current level and store in a list
# Delete the class  for the current level from the training set.
# Delete exclusion from the test set.
# Then combine the two to create a new task.
#
create_level_tasks = function(binary_mode, tasks, target_classes, level, target_var, train_subset, test_subset)
{
	print(paste0("In create_level_tasks: ", level, ", ", target_classes[level+1]))
	new_tasks = list()
	for (task_idx in 1:length(tasks)) {
		dat = getTaskData(tasks[[task_idx]])
		if (binary_mode) {
			print("In binary mode")
			orig_labels = substrLeft(row.names(dat), 2)
			dat$Label = as.factor(orig_labels)
#			print(target_classes[level+1])
			levels(dat$Label)[levels(dat$Label) != target_classes[level+1]] = "REST"  # NB level +1 - preparing for next level
#			print(dat$Label)
		}
		new_dat = dat[sort(c(train_subset, test_subset)), ]
		new_dat$Label = as.factor(as.character(new_dat$Label))
#		print("New dat")
#		print(levels(new_dat$Label))
#		print(new_dat$Label)
#		print(row.names(new_dat))
		new_tasks[[task_idx]] = makeClassifTask(id = tasks[[task_idx]]$task.desc$id, data = new_dat, target = target_var)
	}
  return(new_tasks)
}

#
# Return row numbers of the high confidence predictions
#
get_high_conf_predns = function(probs, threshold) 
{
	high_conf = which(apply(probs[, grepl("prob.", colnames(probs))], 1, function(x) any(x >= threshold)))
	return(high_conf)
}

#
# Return row numbers of the predictions that matched the target class
#
get_matches = function(probs, pos_class) 
{
	print(paste0("In get_matches: ", pos_class))
#	print(probs)
	print(apply(probs, 1, function(x) names(which.max(x))))
	matches = which(apply(probs, 1, function(x) names(which.max(x))) == pos_class) # Also want the probability to be >0.5
	print(matches)
	return(matches)
}

#
# Make a final prediction from the raw responses, taking into account the threshold.
# Divide all rows by the threshold then get max the element in each row.
# This is what mlr does, but only on a Prediction object, so we have to imitate it here.
#
make_prediction = function(responses, threshold, classes)
{
	print("In make_prediction")
	p = sweep(as.matrix(responses), MARGIN = 2, FUN = "/", threshold)
	# 0 / 0 can produce NaNs. For a 0 threshold we always want Inf weight for that class
	p[is.nan(p)] = Inf
	predn = as.factor(getMaxIndexOfRows(p))
	levels(predn) = classes
	return(predn)
}

#
# Create a new prediction threhold, with the target class lower than the other classes
#
make_threshold = function(classes, target_classes, level) 
{
#	if (level > 1)
#		classes = setdiff(classes, target_classes[1:(level-1)])
	print("In make_threshold")
#	dflt_thresh = 1 / length(classes)
#	new_thresh = round(dflt_thresh + 0.1, 1)
	
	dflt_thresh = ifelse(length(classes) == 2, 0.4, 0.2)
	new_thresh = (1 - dflt_thresh) / (length(classes) - 1)
	thresh = rep(new_thresh, times = length(classes))
	names(thresh) = classes
#	thresh[target_classes[[level]]] = 1 - ((new_thresh) * (length(classes) - 1))
	thresh[target_classes[[level]]] = dflt_thresh
	print("Threshold:")
	print(thresh)
	return(thresh)
}

#
# Modified version of mlr function getPredictionProbabilities 
#
get_prediction_probs = function(pred, cl)
{
	cl2 = paste("prob", cl, sep = ".")
	print(colnames(pred$data))
	print(cl)
	y = pred$data[, cl2]
	if (length(cl) > 1L) {
		colnames(y) = cl
	}
}

#
# Modified version of mlr function setThreshold 
#
set_threshold = function(pred, threshold) {
	print("In set_threshold")
  levs = as.character(unique(pred$data$truth))
  p = getPredictionProbabilities(pred, cl = levs)
#	print("Old probabilities:")
#	print(p)
  threshold = threshold[levs]
	
	# divide all rows by threshold then get max element
	p = sweep(as.matrix(p), MARGIN = 2, FUN = "/", threshold)
	# 0 / 0 can produce NaNs. For a 0 threshold we always want Inf weight for that class
	p[is.nan(p)] = Inf
	colnames(p) = paste("prob.", colnames(p))
#	print("New probabilities")
#	print(p)
	ind = getMaxIndexOfRows(p)
	class(ind) = "factor"
	attr(ind, "levels") = levs
#	print(ind)
	pred$data$old_response = pred$data$response
	pred$data$response = ind
#	pred$data = bind_cols(pred$data, p)
	pred$data[, grepl("prob.", colnames(pred$data))] = p
  pred$threshold = threshold
#	print(pred$data)
  return(pred)
}


#
# Train a binary model, with pos_class against the rest - on one level and fold of the multi-level strategy. (not neccessarily)
# Exclude all samples in the target_classes list from the model, as these have already been predicted.
# Gives a set of class predictions per modality
# Combine these and select the final prediction with a majority vote
#
get_predictions = function(tasks, learners, target_classes, level, train_subset, test_subset, prob_method, measures, perf, stab, sample_ids) 
{
	print("In get_predictions")
	stopifnot(length(tasks) >= 1)	
	models = list()	
	predns = list()
	responses = list()

	# Train a model on each task (modality) and record the responses in a data.frame, one per column
	for (i in 1:length(tasks)) {
		lrn_idx = ifelse(length(tasks) == length(learners), i, 1L)
		task_id = tasks[[i]]$task.desc$id
		print(task_id)
		dat = getTaskData(tasks[[i]])
		
		# Train the learner and record the class predictions in a list
		models[[i]] = train(learner = learners[[lrn_idx]], task = tasks[[i]], subset = train_subset)
		stab$save_features(getFilteredFeatures(models[[i]]))
		
		if (!is.null(models[[i]])) {
			pred = predict(models[[i]], task = tasks[[i]], subset = test_subset)
			if (all(is.na(pred$data$response))) {
				print("*** Prediction failed ***")
			}
			classes = as.factor(as.character(unique(pred$data$truth)))
			thresh = make_threshold(classes, target_classes, level)
			pred = set_threshold(pred, threshold = thresh)
#			perf$calculate(pred, measures)

			if (length(predns) == 0) {
				predns[['id']] = test_subset
				predns[['ID']] = sample_ids[test_subset]
				predns[['truth']] = as.factor(pred$data$truth)
			}
			if (prob_method == "vote") {
				predns[[task_id]] = pred$data$response
			} else {
				prob_cols = colnames(pred$data)[grepl("prob.", colnames(pred$data))]
				for (col in prob_cols) {
					new_col = paste0(task_id, ".", col)
					predns[[new_col]] = pred$data[, col]
				}
			}
		}			
	}
	
	# Combine the responses from each task into a single data.frame
	# Add the ID, final decision and probabilities for each class to this data.frame
	predictions = as.data.frame(predns)
	classes = unique(predictions$truth)
	
	if (prob_method == "vote") {
		raw_responses = predictions[,!colnames(predictions) %in% c('id', 'ID', 'truth')]
		predictions$Final = as.factor(apply(raw_responses, 1, function(x) names(which.max(table(x)))))
		raw_responses = predictions[,!colnames(predictions) %in% c('id', 'ID', 'truth', 'Final')]
			
		for (i in 1:nrow(predictions)) {
			t = table(as.matrix(raw_responses[i,]))
			for (cls in classes) {
				predictions[i, paste0('prob.', cls)] = ifelse(is.na(t[cls]), 0, t[cls] / sum(t))
			}
		}
	} else if (prob_method == "avg") {
			# Calculate the average probability for each class
			for (cls in classes) {
				class_cols = grepl(cls, colnames(predictions))
				if (sum(class_cols) == 1) {
					predictions[, paste0('Mean.', cls)] = predictions[, class_cols]
				} else {
					predictions[, paste0('Mean.', cls)] = apply(predictions[, class_cols], 1, sum, na.rm = TRUE) / sum(class_cols, na.rm = TRUE)
				}
			}

			#Make a final prediction from those average probabilities
			predictions$Final = apply(predictions[,grepl("Mean.", colnames(predictions))], 1, function(x) names(which.max(x)))

			# If all predictions were NA due to failed models, then predictions$Final will be NULL - non-existent
			if (is.null(predictions$Final)) {
				predictions$Final = as.factor(rep(0, nrow(predictions)))
			} else {
				predictions$Final = apply(predictions['Final'], 1, function(x) if (!is.na(x)) {strsplit( x, ".", fixed = TRUE)[[1]][2]})
				predictions$Final = as.factor(predictions$Final)
			}
	}
	print(predictions)
	return(predictions)
}

#
# Run the step-wise procedure
#
stepwise_tests = function(config, res_index, prob_method, cache_dir, binary_mode = FALSE) 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print(Sys.time())
	configureMlr(show.learner.output = TRUE, on.learner.error = 'warn', on.par.without.desc = 'warn')	
	set.seed(24601, "L'Ecuyer")	

	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	
	tasks = create_tasks(data_dir, config, config$targetVar)
	classes = unique(getTaskData(tasks[[1]])$Label)
	learners = create_learners(config, base_learners, base_filters, "prob", cache_dir)
	resamp = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
	ri = makeResampleInstance(resamp, tasks[[1]])
	target_classes = c("CON", "LX", "CIR", "LN")
	
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
	for (rep in 1:ri$desc$reps) {
		print(paste("Rep = ", rep))

		fold_responses = list()
		for (fold in 1:ri$desc$folds) {
			print(paste("Fold = ", fold))
			subset_idx = (rep - 1) * ri$desc$folds + fold
			print(paste("Subset Index = ", subset_idx))
			dat = getTaskData(tasks[[1]])
			lvl_tasks = tasks
			train_subset = ri$train.inds[[subset_idx]]
		  test_subset = ri$test.inds[[subset_idx]]
			exclude_train = NULL
			exclude_test = NULL
			level_responses = list()
			
			for (lvl in 1:(length(target_classes)-1)) {
				print(paste("Level = ", lvl))	
				agree_level = 2
				sample_ids = row.names(dat)
				
				while (agree_level < 4) {
					predictions = get_predictions(lvl_tasks, learners, target_classes, lvl, train_subset, test_subset, prob_method, classfn.measures, perf, stab, sample_ids)
					matches = predictions$Final == target_classes[lvl]
					agreements = rowSums(predictions[, !names(predictions) %in% c('id', 'ID', 'truth', 'Final')] == target_classes[lvl])
					print("Agreements")
					print(agreements)
					
					if (lvl == length(target_classes)) {
						level_responses[[lvl]] = predictions[, c('id', 'ID', 'truth', 'Final')]
					} else {
	#					level_responses[[lvl]] = predictions[matches, c('id', 'ID', 'truth', 'Final')]
						level_responses[[lvl]] = predictions[agreements >= 2, ]
					}
					print(paste0("Level responses for level ", lvl, " and agreement level ", agree_level))
					print(level_responses[[lvl]])
				
					test_subset = setdiff(test_subset, which(agreements >=2))
					agree_level = agree_level + 1
				}
				
				# Update the training subset to remove the class for this and all higher levels.
				# And remove the samples already predicted from the test subset.
				# Create new tasks using just the data in these new subsets.
				# Update the training and test subsets to match the news tasks by matching labels.
				exclude_train = which(dat$Label %in% c(target_classes[1:lvl]))
				new_train = setdiff(train_subset, exclude_train)
				exclude_test = which(dat$Label %in% c(target_classes[1:lvl]))
				new_test = setdiff(test_subset, exclude_test)
				lvl_tasks = create_level_tasks(binary_mode, lvl_tasks, target_classes, lvl, config$targetVar, new_train, new_test)
				new_dat = getTaskData(lvl_tasks[[1]])
				train_subset = which(row.names(new_dat) %in% row.names(dat[new_train, ]))				
				test_subset = which(row.names(new_dat) %in% row.names(dat[new_test, ]))
				dat = new_dat
			}
			
			# Combine the responses for each level, first removing the dataframes with 0 rows
			print("Combining responses for each level")
			fold_responses[[fold]] = bind_rows(level_responses[which(lapply(level_responses, nrow) != 0)])
			fold_responses[[fold]] = fold_responses[[fold]] %>% arrange(id)
#			print(paste0("Fold responses for fold ", fold))
#			print(fold_responses[[fold]])
			roc$calc(fold_responses[[fold]]$truth, fold_responses[[fold]]$Final, as.list(classes))
		}
		
		# Combine the responses for each fold
		print("Combining responses for each fold")
		final_response[[rep]] = bind_rows(fold_responses[which(lapply(fold_responses, nrow) != 0)])
		final_response[[rep]] = final_response[[rep]] %>% arrange(id)
		final_response[[rep]][, "rpt"] = rep
		print(paste0("Final responses for repeat ", rep))
		print(final_response[[rep]])
	}
		
	# Combine the responses for each repeat into a single data frame
	print("Combining all responses")
	all_responses = bind_rows(final_response[which(lapply(final_response, nrow) != 0)])
	print("All responses:")
	print(all_responses)

	# Aggregate, save and plot results
	result_file = paste(config$resultFile, res_index, sep = "_")
	write.csv(all_responses, paste(result_file, "_predns.csv", sep=""), row.names=FALSE, na="")
	roc$write(result_file)
	roc$plot("combn", "Combination", paste(result_file, "_plot.csv", sep=""))
	
#	print("Saving data for base models")
#	for (i in 1:length(tasks)) {
#		agg = roc_base[[i]]$aggregate()
#		model_id = strsplit(learners[[i]]$id, ".", fixed = TRUE)[[1]][1]
#		roc_base[[i]]$write(result_file, model_id)
#		roc_base[[i]]$plot(model_id, tasks[[i]]$task.desc$id, result_file)
#		perf_base[[i]]$write(result_file, model_id, model_id)
#		stab_base[[i]]$save(model_id, stab_base[[i]]$feature_sets, getTaskNFeats(tasks[[i]]))
#		stab_base[[i]]$write(result_file, model_id)
#		feat_base[[i]]$write(result_file, model_id)
#	}
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}