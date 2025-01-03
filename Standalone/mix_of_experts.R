library(mice)
library(mlr)
library(mlrCPO)
library(dplyr)
library(BBmisc)
library(pryr)
library(data.table)
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
source("predictions_classfn.R")
source("stability.R")
source("mm_features.R")
source("learners.R")
source("feature_selectors.R")
source("data_helpers.R")
source("models.R")
source("mm_adaboost.R")
source("combn.R")
source("RLearner_classif_ggbm.R")
source("incremental.R")


#
#
expert_tests = function(config, res_index, cache_dir, method = "COMBO", decision = "vote", big = FALSE, subset = NULL, balance = FALSE, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE, validate = FALSE) 
{  
	#--------------------------------------------------------------------------
	# INITIALISATION
	#--------------------------------------------------------------------------
  print(Sys.time())
	configureMlr(show.learner.output = TRUE, on.learner.error = 'stop', on.par.without.desc = 'warn', on.error.dump = TRUE)	
	set.seed(24601, "L'Ecuyer")	
	plan(multicore)

	acc.na = setAggregation(acc, test.mean_narm)
	prob.measures = list(acc.na, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu)
	classfn.measures = list(acc.na)
	#	prob_method = ifelse(decision == "prob", "prob", "response")
	prob_method = "prob"
	decision = "prob"	
	
	tasks = create_tasks(data_dir, config, TASK_CLASSIF, NULL, big, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
	learners = create_learners(config, base_learners, base_filters, prob_method, balance, cache_dir, subset)
	resamp = makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
	ri = makeResampleInstance(resamp, tasks[[1]])
	dat = getTaskData(tasks[[1]])
	classes = sort(unique(dat[, config$targetVar]))
	print("Classes:")
	print(classes)
	
	# Check this - is it possible to use vote for moe? Don't allow hard voting option for now	
	ovr = create_ovr_tasks(tasks, classes, config$targetVar)	
	ovr_tasks = ovr$tasks
	ovr_classes = ovr$classes
	print("OVR classes:")
	print(ovr_classes)
	
	#-----------------------------------------------------------------------------------------------------------------------------
	# DATA STRUCTURES TO COLLECT RESULTS - Need One per Expert?
	#-----------------------------------------------------------------------------------------------------------------------------	
	roc = ROCMultiClass$new()
	perf = Performance$new(classfn.measures)
	feats = list()
	stab = list()
	for (cls_idx in 1:length(classes)) {
		feats[[cls_idx]] = MM_Features$new(tasks)
		stab[[cls_idx]]  = Stability$new(classes)
	}
	 
	#-----------------------------------------------------------------------------------------------------------------------------
	# MODELING 
	#-----------------------------------------------------------------------------------------------------------------------------	
	print("Memory used:")
	print(mem_used())

	# Classes form the outer loop and a resampling instance is made for each class.
	# These have to be unique to each class as each class can be balanced separately and so may have different indices.
	# Therefore, you have to run all folds of each class and then put them together at the end.
	
	expert_responses = list()
	expert_futures = list()
	ri = makeResampleInstance(resamp, ovr_tasks[[1]][[1]])
	
	for (cls_idx in 1:length(classes)) {		
		print(paste0("Expert ", cls_idx, ": ", ovr_classes[[cls_idx]][[1]], " vs REST"))
		final_response = list()
		
		expert_futures[[cls_idx]] = future({
		
			for (rep in 1:ri$desc$reps) {
		#		print(paste("Rep = ", rep))

				fold_responses = list()
				for (fold in 1:ri$desc$folds) {
		#			print(paste("Fold = ", fold))
					subset_idx = (rep - 1) * ri$desc$folds + fold
					print(paste("Subset Index = ", subset_idx))
					train_subset = ri$train.inds[[subset_idx]]
					test_subset = ri$test.inds[[subset_idx]]
#					print("Training subset:")
#					print(train_subset)
#					print("Test subset:")
#					print(test_subset)
										
					# Train and predict the individual experts (can be done in parallel) (maybe using a multi-modal Adaboost classifier - for now combination of probs)
					# NB Add futures to make it parallel	
#					print("Training")
					dat = getTaskData(ovr_tasks[[cls_idx]][[1]])
#					print("Data:")
#					print(head(dat))
#					print("Test data:")
#					print(dat[test_subset, ])
					sample_ids = data.frame('id' = test_subset, 
																	'ID' = row.names(dat[test_subset, ]), 
																	label = dat[test_subset, config$targetVar])
					predns = sample_ids
					rownames(predns) <- predns$ID
#					print(predns)

					if (method == "COMBO") {
						combo = Combination$new(ovr_tasks[[cls_idx]], learners, config$targetVar)
						pred = combo$train_base_mods(ovr_tasks[[cls_idx]], learners, train_subset, test_subset, subset_idx, sample_ids, decision, ovr_classes[[cls_idx]], feats[[cls_idx]])
						predns = combo$get_final_decision(pred, ovr_classes[[cls_idx]], decision, names(ovr_tasks[[cls_idx]]))
#						print(predns)
						rm(combo)
					} else if (method == "ADA") {
						print("MOE ADA")
						print(ovr_classes[[cls_idx]])
						mm_ada = MM_Adaboost$new(ovr_tasks[[cls_idx]], learners, config$boostIters, config$targetVar, config$metaLearner)			
						mm_ada$train(train_subset, ovr_classes[[cls_idx]], sample_ids, decision, config$targetVar)
						pred = mm_ada$predict(test_subset, ovr_classes[[cls_idx]], decision)
						predns = pred$data
						
						scores = mm_ada$get_feature_importances(ovr_classes[[cls_idx]])
						for (tsk in names(scores)) {
							print("Saving features")
							feats[[cls_idx]]$save("adaboost", scores[[tsk]], tsk, subset_idx)
						}	
						rm(mm_ada)
					} else if (method == "META") {
						combo = Combination$new(ovr_tasks[[cls_idx]], learners, config$targetVar)
						pred = combo$train_base_mods(ovr_tasks[[cls_idx]], learners, train_subset, test_subset, subset_idx, sample_ids, decision, ovr_classes[[cls_idx]], feats[[cls_idx]])
						predns = pred$data
#						print(head(pred$data))
						rm(combo)
					}
					predns$fold = subset_idx
					fold_responses[[fold]] = predns
					rm(pred)
					rm(predns)
				}			
			
				# Combine the responses for each fold for this repeat
				print(paste0("Combining responses for repeat ", rep))
				final_response[[rep]] = bind_rows(fold_responses)
				final_response[[rep]] = final_response[[rep]] %>% arrange(id)  # Sort so that all match up
				final_response[[rep]][, "rpt"] = rep
#				print(final_response[[rep]])
				rm(fold_responses)
			}
			
			# Combine the responses for each repeat into a single data frame
			list("expert_responses" = bind_rows(final_response), "features" = feats[[cls_idx]])
#			expert_responses[[cls_idx]] = bind_rows(final_response)
		})	
	}

	resolve(expert_futures)	
	print("Expert futures resolved")
	for (idx in 1:length(expert_futures)) {
#		print(paste0("idx = ", idx))
		val = value(expert_futures[[idx]])
		expert_responses[[idx]] = val$expert_responses
		feats[[idx]] = val$features
		print(paste0("Expert ", idx, " predicted:"))
		print(expert_responses[[idx]] )
	}	

	# Create a summary of the responses from each expert
		print("Summarising expert responses")

	if (length(expert_responses) >= 1) {
		summary_responses = expert_responses[[1]][, c('id', 'ID', 'rpt', 'fold')]  # Relies on each expert being in same order
		prob_res = summary_responses
		for (cls_idx in 1:length(expert_responses)) {
#			print(paste0("Class index = ", cls_idx))
#			print(classes[cls_idx])
#			print(ovr_classes[cls_idx])
			
			# Don't use match here to match to ID as there are multiple of each ID
			truth = as.character(expert_responses[[cls_idx]][, 'truth'])
#			print(truth)
			summary_responses[truth != "REST", 'truth'] = truth[truth != "REST"]
			summary_responses[, paste("Expert", cls_idx)] = as.character(expert_responses[[cls_idx]][, 'response'])
			
#			prob_col = paste0("prob.", classes[cls_idx])  # Use classes, not ovr_classes as we don't want REST, just the main column
#			print(prob_col)
#			prob_res[truth != "REST", 'truth'] = truth[truth != "REST"]
#			prob_res[, prob_col] = expert_responses[[cls_idx]][, prob_col, drop = FALSE]
		}
#		print("Summary Responses:")
#		print(summary_responses)
		response_posns = as.list(apply(summary_responses[, grepl("Expert", colnames(summary_responses))], 1, function(x) {which(x != "REST")}))
#		print("Response positions:")
#		print(head(response_posns))
	}
			
	# Aggregate the responses from all the experts
	# If more than one expert gives a valid prediction (i.e. predicts the class it is an expert on, not REST), 
	# then accept the one that predicts with the highest confidence
#	print("Aggregating responses - Gating")
	prob_res = do.call(cbind, expert_responses)
#	print("Probabilities:")
#	print(prob_res)
	
	for (i in 1:length(response_posns)) {  # For each row
#		print(paste0("i = ", i))
#		print(response_posns[[i]])
		prob_row = prob_res[i, grepl('prob.', colnames(prob_res)) & !grepl('REST', colnames(prob_res)), drop = FALSE]
#		print(prob_row)
		if (length(response_posns[[i]]) > 0) {
			prob_row = prob_row[, unlist(response_posns[[i]]), drop = FALSE]
			print(prob_row)
		} else {
			print("All predicted REST - choose highest confidence prediction")
		}

		if (ncol(prob_row) > 0) {
			max_posn = as.vector(apply(prob_row, 1, which.max))
			max_predn = strsplit(colnames(prob_row)[max_posn[1]], ".", fixed = TRUE)  # Take the first one if there are more than one
			max_predn = sapply(max_predn, "[[", 2)
#			print(paste0("max_predn = ", max_predn))
			summary_responses[i, 'response'] = max_predn
		} else {
			summary_responses[i, 'response'] = "Unknown"
		}
	}

	# Save and plot results
	result_file = paste("moe", res_index, sep = "_")
	print(paste0("Result_file = ", result_file))
	write.csv(summary_responses, paste(result_file, "_predns.csv", sep=""), row.names=FALSE, na="")
	
	print("Calculating ROC scores")
	for (idx in 1:(ri$desc$reps * ri$desc$folds)) {
#		print(paste0("Idx = ", idx))
		roc_rpt = summary_responses[summary_responses$fold == idx, ]
		roc_rpt[roc_rpt == "Unknown"] = "REST"
		roc$calc(as.factor(roc_rpt$truth), as.factor(roc_rpt$response), classes)
		perf$calculate(make_mlr_prediction(roc_rpt, tasks[[1]]))
	}
	perf$write(result_file)
	roc$plot("moe", "MoE", paste(result_file, "_plot.csv", sep=""))
	roc$calc_mc_roc(as.factor(summary_responses$truth), as.factor(summary_responses$response))
	roc$write(result_file)
	
	print("Calculating feat and stab scores")
	for (cls_idx in 1:length(classes)) {
		print(cls_idx)
		print(result_file)
		feats[[cls_idx]]$write(paste(result_file, classes[cls_idx], sep = "_"))
		stab[[cls_idx]]$save_all("moe", feats[[cls_idx]]$featsel)
		stab[[cls_idx]]$write(paste(result_file, classes[cls_idx], sep = "_"))
	}
		
	writeLines("\n")
	print(warnings())
  print(Sys.time())
}