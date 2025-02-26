#' @title MM_Model: R6 class representing a multi-modal model
#'
#' @description
#' Provides functions to create and fit a generic model, which is run using mlr.
#' This is a super-class of all of the multi-model models.
#'
#' @name MM_Model
NULL

MM_Model = R6::R6Class("MM_Model", 
	public = list(
		config     = NULL,
		model_type = NULL,
		task_type  = TASK_CLASSIF,
		targetVar  = "",
		decision   = "prob",
		validation = FALSE,
		tasks      = list(),
		vtasks     = NULL,
		learners   = NULL,
		classes    = c(),
		measures	 = NULL,
		ovr_classes = NULL,
		results    = NULL,
		ri 				 = NULL,
		ri_v			 = NULL,

    #' @description 
		#' Create a new MM_Model object, based on parameters specified in the config object.
		#' Reads in the data and creates an mlr task for each modality.
		#' Creates the required learners, and other structures needed by mlr.
		#' @param config (MM_Config)\cr
		#' Configuration object, specifying how the model should be constructed.
    #' @param model_type (character)\cr
		#' Type of model - "CLASSIF" for classification or "SURV" for survival analysis. 
		#' @param decision (character)\cr
		#' Type of prediction - 'response' or 'prob'.
		#' @param subset (integer)\cr
		#' @param concat (logical(1))\cr
		#' Should the tasks be concatenated to form a single, large dataset?
		#' @param balance (logical(1))\cr
		#' Should the tasks be balanced during training?
		#' @param validate (logical(1))\cr
		#' Should the model be validated with validation data provided in the config file.
		#' @param filter_zeroes (double(1))\cr
		#' Features with this percentage of zero values or greater will not be included in the model.
		#' @param filter_missings (double(1))\cr
		#' Features with this percentage of missing values or greater will not be included in the model.
		#' @param filter_corr (double(1))\cr
		#' Should correlated features be included in the model? If FALSE, one feature from each correlated pair is eliminated.
		#' @param filter_var (double(1))\cr
		#' Should low variance features be included in the model?
### NB explain how these are selected
		#' @return A new `MM_Model`object
		#' @export
		#' 
		initialize = function(config, model_type = "CLASSIF", decision = "hard", subset = NULL, concat = FALSE, balance = FALSE, validate = FALSE, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE)
		{
			mlr::configureMlr(show.learner.output = TRUE, on.learner.error = 'warn', on.par.without.desc = 'warn')
			Filters$new()
			if (!missing(config)) {
#				assertClass(config, "MM_Config")
				self$config     = config
				self$targetVar  = config$targetVar
				
				if (!missing(model_type)) {
					checkmate::assertChoice(model_type, choices = c("CLASSIF", "SURV"))
				}
				self$model_type = model_type
				self$task_type = ifelse(model_type == "SURV", TASK_SURV, TASK_CLASSIF)
				
				if (!missing(subset) && !is.null(subset)) {
					checkmate::assertNumeric(subset, unique = TRUE)
				}
				if (!missing(decision)) {
					checkmate::assertChoice(decision, choices = c("hard", "vote", "soft", "prob", "meta"))
				}
				self$decision   = decision
				predict_type   = ifelse(decision == "hard" || decision == "vote", "response", "prob")
					
				if (self$model_type == "SURV") {
					self$measures = PerformanceMeasures$new(self$model_type)$measures
				} else {
					checkmate::assertLogical(balance)
					self$measures = PerformanceMeasures$new(self$model_type, decision)$measures
					learners = Learners$new(self$task_type)
					self$learners = learners$create_learners(config, env = environment(), predict_type, balance, subset)
				}

				checkmate::assertNumeric(filter_zeroes, lower = 0.0, upper = 100)
				checkmate::assertNumeric(filter_zeroes, lower = 0.0, upper = 100)
				checkmate::assertLogical(filter_corr)
				checkmate::assertLogical(filter_var)
				self$tasks = self$create_tasks(config$dataDir, config, self$task_type, subset, filter_zeroes, filter_zeroes, filter_corr, filter_var)

				checkmate::assertLogical(concat)
				if (concat) {
					ctasks = self$concat_tasks(self$tasks)
					if (length(self$tasks) > 1) {
						config$baseModels[2:length(config$baseModels)] = NULL
					}
					self$tasks = ctasks
				}
			
				self$results = MM_Results$new(self$classes, self$tasks, self$measures, model_type, decision)
				resamp = mlr::makeResampleDesc("RepCV", reps = config$itersOuter, folds = config$foldsOuter, stratify = TRUE)
				self$ri = mlr::makeResampleInstance(resamp, self$tasks[[1]])

				if (!missing(validate)) {
					checkmate::assertLogical(validate)
				}
				self$validation = validate
				if (validate) {
					self$vtasks = self$create_validation_tasks(self$tasks, config$dataDir, config, self$task_type, ovr_class, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
					vdat = getTaskData(self$vtasks[[1]])
					self$ri_v = mlr::makeFixedHoldoutInstance(c(1:nrow(dat)), c((nrow(dat) + 1):nrow(vdat)), nrow(vdat))
					self$vresults = MM_Results$new(self$classes, self$vtasks)
				}
			}
		},
		
    #' @description 
		#' Clone an MM_Model object, from another MM_MOdel object.
		#' Pass in new tasks, classes and optiobnally learners.
		#' @param model (MM_Model)\cr
		#' Model to be cloned from
    #' @param tasks (list)\cr
		#' New list of tasks for this model 
		#' @param classes (factor)\cr
		#' New classes for this model
		#' @param learners (list)\cr
		#' New list of learners for this model (optional) 
		#' If not supplied, the learners will be the same as for the old model
    #' @return A new `MM_Model`object
		#' @export
		#' 
		clone_model = function(model, tasks, classes, learners = NULL) {
			self$tasks 			= tasks
			self$classes    = classes
			self$results    = MM_Results$new(self$classes, self$tasks, self$measures)
			if (is.null(learners)) {
				self$learners 	= model$learners
			} else {
				self$learners   = learners
			}
			self$config   	= model$config
			self$model_type = model$model_type
			self$task_type  = model$task_type
			self$targetVar  = model$targetVar
			self$decision   = model$decision
			self$validation = model$validation
			self$vtasks     = model$vtasks
			self$ri 				= model$ri
			self$ri_v			 	= model$ri_v
		},
		
    #' @description 
		#' Calculate the final response, according to the decision_type type and
		#' add a response column to the results
		#' @noRd
		get_final_decision = function(results) {
			final_results = NULL
			if (self$decision %in% c('vote', 'hard')) {
				# Calculate final prediction with a majority vote across modalities
				raw_responses = as.data.frame(results[,!colnames(results) %in% c('id', 'ID', 'truth')])
				final_results = results[, c('id', 'ID', 'truth')]
				final_results$response = as.factor(apply(raw_responses, 1, function(x) names(which.max(table(x)))))
				
			} else if (self$decision %in% c('prob', 'soft')) {
				# Calculate sum of probabilities for each class and take max of that as prediction
					testit::assert(length(self$classes) == 2)
					ovr_class = ifelse(self$classes[1] == 'REST', self$classes[2], self$classes[1])
					final_results = results[, c('id', 'ID', 'truth')]

					final_results[, paste0('prob.', ovr_class)] = rowSums(as.data.frame(results[, grepl(paste0("\\<", ovr_class, "\\>"), colnames(results))]), na.rm = TRUE)
					final_results[, paste0('prob.', "REST")] = rowSums(as.data.frame(results[, grepl(paste0("\\<", "REST", "\\>"), colnames(results))]), na.rm = TRUE)
					final_results$response = apply(final_results[, grepl("prob.", colnames(final_results))], 1, function(x) names(which.max(x)))					
					
					if (!is.null(final_results$response)) {
						final_results$response = strsplit(as.character(final_results$response), ".", fixed = TRUE)
						final_results$response = as.factor(sapply(final_results$response, "[[", 2))
					} else {
						stop("NULL response")
					}
			} else {
				stop("Invalid decision type")	# WHat about meta???
			}
			
			return(final_results)
		},
		
		
    #' @description 
		#' Perform pre-processing on a single dataset.
		#' @inheritParams create_tasks
		#' @param idx Index into the list of tasks
		#' @param raw Raw data to be prepared
		#' @param id_col Index of column containing sample identifier
		#' @param prepend Should the data modality be prepended to the name of each feature?
    #' @return The filtered data
		#' @noRd
		prepare_data = function(config, idx, raw, id_col, task_type = "CLASSIF", prepend = FALSE) {
			dataset = as.data.frame(raw)
			
			# Remove the patient ID, as it becomes the row name. 
			# Replace '-' in target column as it is converted to '.' and upsets plotting later
			dataset = dataset[ , !colnames(dataset) == id_col]
			dataset = janitor::clean_names(dataset, case = "none")

			# When reading data from the config file, the value returned 
			# will be NULL if the column does not exist in the config file
			# and NA if the column exists but the cell is empty.
			# Therefore we must check for both.

			# Convert categorical features to factors and others to numerics
			if (inherits(config, "config_single")) {
				cat_str = config$categoricals
			} else {
				cat_str = config$dataSets[[idx]]$categoricals
			}
			categoricals = c()
			if (!is.null(cat_str) && !is.na(cat_str)) {
				categoricals = unname(unlist(strsplit(cat_str, split = ",", fixed = TRUE)))
				dataset[, categoricals] <- lapply(dataset[, categoricals], as.factor)
			}
			keep = !colnames(dataset) %in% c(config$targetVar, config$timeVar, config$statusVar, categoricals)
			dataset[, keep] = sapply(dataset[, keep, drop = FALSE], as.numeric)

		# Prepend with name of modality (Is this needed?)
			if (prepend) {
				keep = !colnames(dataset) %in% c(row_names, config$targetVar, config$timeVar, config$statusVar)
				colnames(dataset)[keep] = paste0(mod, "_", make.names(colnames(rdatasetaw)[keep], unique = TRUE))
			}

			return(dataset)
		},

    #' @description 
		#' Apply filtering to the data to identify those variables likely to be relevant.
    #' @inheritParams create_tasks
		#' @return A list of vectors of relevant parameters, one vector per modality
		#' @noRd
		identify_relevant = function(config, dir, task_type, row_names, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
			relevant = list()
			
			for (i in 1:length(config$dataSets)) {
				mod = self$get_modality(config, i)
				message(paste0("Modality: ", mod))
				raw = self$read_raw(dir, config, i, row_names, NULL, FALSE, task_type)
				dataset = as.data.frame(raw)
				dataset = dataset[ , !colnames(dataset) %in% c(config$targetVar, config$timeVar, config$statusVar)]
				
				if (is.null(relevant[[mod]])) {
					relevant[[mod]] = list()
				}		
			
				# Identify features that have more than 50% missing values
				drops_na = c()
				if (any(is.na(dataset))) {
					count_na = as.data.frame(colSums(is.na(dataset)))
					drops_na = rownames(count_na)[which(count_na > nrow(dataset)/ 2)]
				}
				message(paste0("Features that have more than 50% missing values:", length(drops_na)))
				
				# Identify features that have more than 90% zero values
				count_nz = as.data.frame(colSums(dataset == 0, na.rm = TRUE) / nrow(dataset)*100)
				drops_nz = rownames(count_nz)[which(count_nz > 90)]
				drops = c(drops_na, drops_nz)
				if (length(drops) > 0) {
					dataset = as.data.frame(dataset[ , !(names(dataset) %in% drops)])
				}
				message(paste0("Features that have more than 90% zero values:", length(drops_nz)))

				# Identify features that are highly correlated
				drops_cor = c()
				if (filter_corr) {
					numeric_cols = unlist(lapply(dataset, function(x) {is.numeric(x) && !all(x %in% c(0,1))}))
					numeric_data = dataset[, numeric_cols]
					cor_mat = abs(cor(numeric_data))
					cor_mat[!lower.tri(cor_mat)] = 0
					drops_cor = names(which(!apply(cor_mat,2,function(x) all(x<=0.80))))
					message(paste0("Features that are highly correlated:", length(drops_cor)))
					if (length(drops_cor) > 0) {
						dataset = as.data.frame(dataset[ , !(names(dataset) %in% drops_cor)])
					}
				}
					
				drops_var = c()
				fs_ratio = ncol(dataset) / nrow(dataset)
				if (filter_var && fs_ratio > 10) {
					dfvar = sapply(dataset, var, na.rm = TRUE)
					dfvar = dfvar[order(dfvar, decreasing = TRUE, na.last = TRUE)]
					if (length(dfvar) > 500) {
						drops_var = names(dfvar[501:length(dfvar)])
					}
					message(paste0("Features with low variance:", length(drops_var)))

				}

				drops = c(drops_na, drops_nz, drops_cor, drops_var)
				relevant[[mod]] = setdiff(colnames(dataset), drops)
				message(paste0("Keeping ", length(relevant[[mod]]), " relevant features"))
				rm(raw)
				rm(dataset)
			}
			return(relevant)
		},

    #' @description 
		#' Get the modality of the file with the given index.
    #' @param config Configuration object
		#' @param idx Index into the list of tasks
    #' @return modality `character vector`
		#' @noRd
		get_modality = function(config, idx) {
			if (inherits(config, "config_single")) {
				mod = config$modality
			} else {
				if (is.na(config$dataSets[[idx]]$modality) || is.null(config$dataSets[[idx]]$modality)) {
					mod = strsplit(config$dataSets[[idx]]$dataset, "_", fixed = TRUE)[1:4]
				} else {
					mod = config$dataSets[[idx]]$modality
				}
			}
			return(mod)
		},

    #' @description 
		#' Read in a single raw file with the given index
		#' @inheritParams prepare_data
    #' @param config Configuration object
		#' @param dir Directory in which file can be found
		#' @param idx Index into the list of tasks
		#' @param row_names Name of column containing sample identifier
		#' @param selected If this is the second call to this function, the names of the columns selected the first time
		#' @param prepend Should the data modality be prepended to the name of each feature?
		#' @param task_type Type of task - either CLASSIF or SURV
		#' @param validation Is this validation data?
    #' @return A data.frame cobtaining the selected columns of the raw data or all column sif none selected
		#' @noRd
		read_raw = function(dir, config, idx, row_names = 'ID', selected = NULL, prepend = FALSE, task_type = TASK_CLASSIF, validation = FALSE) 
		{
			if (inherits(config, "config_single")) {
				transposing = FALSE
				exclusions = ""
				filename = config$dataset
				if (!is.null(dir)) {
					filename = paste(dir, filename, sep = "/")
				}
			} else {
				if (validation) {
					datasets = config$validationSets
				} else {
					datasets = config$dataSets
				}
				
				trans_str = datasets[[idx]]$transpose
				transposing = !is.null(trans_str) && !is.na(trans_str) && (trans_str == 'T')

				# Get excluded features
				excl_str = datasets[[idx]]$exclusions
				if (!is.null(excl_str) && !is.na(excl_str)) {
					exclusions = unlist(strsplit(excl_str, split = ",", fixed=TRUE))
					exclusions = make.names(exclusions)
				} else {
					exclusions = ""
				}

				filename = datasets[[idx]]$dataset
				if (!is.null(dir)) {
					filename = paste(dir, filename, sep = "/")
				}
			}
			mod = self$get_modality(config, idx)
			
			if (is.null(selected) || (length(selected) == 0)) {
				suppressWarnings(raw <- sqldf::read.csv.sql(filename, header = TRUE, row.names = row_names, eol = "\r\n"))
				raw[raw == ''] = NA
			} else {
				selected = selected[!selected %in% c(exclusions, "")]
				if (task_type == TASK_SURV) {
					selected = c(row_names, config$timeVar, config$statusVar, selected)
				} else {
					selected = c(row_names, config$targetVar, selected)
				}

				if (transposing) {
					suppressWarnings(raw <- sqldf::read.csv.sql(filename, header=TRUE, row.names = row_names, eol = "\r\n", sql = paste0("select * from file where ID in ('", paste(selected, collapse = "','"), "')" )))
					raw[raw == ''] = NA		
				} else {
					suppressWarnings(raw <- sqldf::read.csv.sql(filename, header=TRUE, row.names = row_names, eol = "\r\n", sql = paste0("select `", paste(selected, collapse = "`,`"), "` from file")))
					raw[raw == ''] = NA
				}
			}
				
			# Transpose the data if requested
			if (transposing) {
				rownames(raw) = raw[, row_names]
				tmp = as.data.frame(data.table::transpose(raw, keep.names = row_names, make.names = row_names))
				raw = tmp
				rownames(raw) = raw[, row_names]
			}
			
			# Sort rows by ID column so that all tasks are in the same order and add row names
			raw = raw[order(raw[, row_names]), ]
			rownames(raw) = raw[, row_names]
			
			# Remove exclusions
			if (inherits(config, "config_single")) {
				raw = raw[,!(colnames(raw) %in% c(exclusions, "")), drop = FALSE]
				target = raw[, colnames(raw) %in% c(config$targetVar, config$timeVar, config$statusVar), drop = FALSE]
				rnames = rownames(raw)
				raw[, colnames(target)] = target
				rownames(raw) = rnames
			}

			# Remove ID var
			raw = raw[, !colnames(raw) %in% row_names]
			return(as.data.frame(raw))
		},

    #' @description 
		#' Read in the raw data files and return in a list, one data.frame per modality.
		#' When there are multiple files per modality,  bind these into one data.frame.
		#" inheritsParams read_raw
    #' @return A list of data.frames, one per modality, containing the raw multi-modal data
		#' @noRd
		read_raw_data = function(dir, config, row_names, selected_names, task_type, validation) 
		{
			raw_data = list()		
			for (i in 1:length(config$dataSets)) {
				mod = self$get_modality(config, i)
				raw = self$read_raw(dir, config, i, row_names, selected_names[[mod]], TRUE, task_type, validation)
				raw_data[[mod]] = rbind(raw_data[[mod]], raw)
				rm(raw)
			}
			gc()
			return(raw_data)
		},

    #' @description 
		#' Create the mlr tasks for each dataset and store in a list.
		#' The data for each task will be filtered according to the filter parameters.
    #' @param data_dir Name of directory in which the data files are located
    #' @param config Configuration object
		#' @param task_type Type of task - either CLASSIF or SURV
		#' @param ovr_class One vs Rest class - if non-null data is converted to binary with this class vs REST
		#" @param subset Vector of integers indicating subset of modalities to use
		#' @param filter_zeroes Remove features with a higher percentage of zero values than specified here
		#' @param filter_missings Remove features with a higher percentage of missing values than specified here
		#' @param filter_corr Remove features that are highly correlated
		#' @param filter_var Remove features with low variance
    #' @return A list of mlr tasks, one per modality
		#' @export
		create_tasks = function(data_dir, config, task_type = TASK_CLASSIF, subset = NULL, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE)
		{
			tasks = list()
			row_names = ifelse(is.null(config$idVar) || is.na(config$idVar), "ID", config$idVar)	
			rel_features = self$identify_relevant(config, data_dir, task_type, row_names, filter_zeroes, filter_missings, filter_corr, filter_var)
			raw_data = self$read_raw_data(data_dir, config, row_names, rel_features, task_type, FALSE)

			for (i in 1:length(raw_data)) {
				task_id = names(raw_data)[[i]]
				dat = self$prepare_data(config, i, raw_data[[i]], row_names, task_type)

				if (task_type == TASK_CLASSIF) {		
					self$classes = as.factor(unique(dat[, config$targetVar]))
					tsk = mlr::makeClassifTask(id = task_id, data = dat, target = config$targetVar)
				} else if (task_type == TASK_SURV) {
					tsk = mlr::makeSurvTask(id = task_id, data = dat, target = c(config$timeVar, config$statusVar), fixup.data = "no", check.data = FALSE)
				}
				tasks[[task_id]] = tsk
			}
			
			if (!is.null(subset)) {
				tasks = tasks[subset]
				tasks = tasks[!is.na(tasks)]
			}

			return(tasks)
		},

    #' @description 
		#' Create the mlr tasks for each validation dataset and store in a list.
		#' The data for each task will be filtered according to the filter parameters.
		#' @inheritParams create_tasks
    #' @param base_tasks The base tasks to be validated
    #' @return A list of mlr tasks, one per modality
		#' @export
		create_validation_tasks = function(base_tasks, data_dir, config, task_type = TASK_CLASSIF, subset = NULL, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE)
		{
			tasks = list()
			row_names = ifelse(is.null(config$idVar) || is.na(config$idVar), "ID", config$idVar)
			
			rel_features = list()
			for (j in 1:length(base_tasks)) {
				dat = mlr::getTaskData(base_tasks[[j]])
				rel_features[[j]] = colnames(dat)
			}
			raw_data = self$read_raw_data(data_dir, config, row_names, rel_features, task_type, TRUE)

			for (i in 1:length(raw_data)) {
				task_id = names(raw_data)[[i]]
				dat = self$prepare_data(config, i, raw_data[[i]], row_names, task_type)
				old_dat = getTaskData(base_tasks[[i]], target.extra = FALSE)
				new_dat = rbind(old_dat, dat)

				if (self$model_type == "SURV") {
					tsk = mlr::makeSurvTask(id = task_id, data = new_dat, target = c(config$timeVar, config$statusVar), fixup.data = "no", check.data = FALSE)
				} else {
					tsk = mlr::makeClassifTask(id = task_id, data = new_dat, target = config$targetVar)
				}
				tasks[[task_id]] = tsk
			}
			
			if (!is.null(subset)) {
				tasks = tasks[subset]
				tasks = tasks[!is.na(tasks)]
			}

			return(tasks)
		},

    #' @description 
		#' Convert an existing set of mlr tasks to binary tasks, using the One Vs Rest approach
    #' @param tasks The existing tasks to be converted
		#' @param classes The classes found in the data
		#' @param target_var The target variable 
    #' @return A list of mlr tasks, converted to binary
		#' @export
		create_ovr_tasks = function(data_dir, config, task_type = TASK_CLASSIF, subset = NULL, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE)
		{			
			tasks = list()
			ovr_classes = list()
			row_names = ifelse(is.null(config$idVar) || is.na(config$idVar), "ID", config$idVar)	
			rel_features = self$identify_relevant(config, data_dir, task_type, row_names, filter_zeroes, filter_missings, filter_corr, filter_var)
			raw_data = self$read_raw_data(data_dir, config, row_names, rel_features, task_type, FALSE)
			self$classes = as.factor(unique(raw_data[[1]][, config$targetVar]))
			
			for (cls in self$classes) {
				tasks[[cls]] = list()
				for (i in 1:length(raw_data)) {
					task_id = names(raw_data)[[i]]
					dat = self$prepare_data(config, i, raw_data[[i]], row_names, task_type)
					dat[, config$targetVar] = as.character(dat[, config$targetVar])
					dat[, config$targetVar][dat[, config$targetVar] != cls] = "REST"
					ovr_classes[[cls]] = as.factor(unique(dat[, config$targetVar]))

					if (task_type == TASK_CLASSIF) {		
						tsk = mlr::makeClassifTask(id = task_id, data = dat, target = config$targetVar)
					} else if (task_type == TASK_SURV) {
						tsk = mlr::makeSurvTask(id = task_id, data = dat, target = c(config$timeVar, config$statusVar), fixup.data = "no", check.data = FALSE)
					}
					tasks[[cls]][[task_id]] = tsk
					rm(dat)
				}
			
				if (!is.null(subset)) {
					tasks[[cls]] = tasks[[cls]][subset]
					tasks[[cls]] = tasks[[cls]][!is.na(tasks)]
				}
			}
			
			gc()
			return(list("tasks" = tasks, "classes" = ovr_classes))
		},

    #' @description 
    #' Change the number of classes in a task to override this check in mlr.
    #' @param model The mlr model to be updated
		#' @param classes The classes found in the data
		#' @param target The target variable 
    #' @return A list of mlr tasks 
		#' @noRd
		update_task_classes = function(model, classes, target) {
				model$task.desc$class.levels = as.character(classes)
				if (!is.null(target)) {
					model$task.desc$class.distribution = table(target)
				}
				return(model)		
		},

    #' @description Concatenate a list of tasks into a single task
    #' @param tasks The list fo tasks to be concatenated
		#' @param task_type The type of task "CLASSIF" or "SURV"
    #' @return A list containing a single task
		#' @export
		concat_tasks = function(tasks)
		{
			df_list = list()
			for (i in 1:length(tasks)) {
				td = mlr::getTaskData(tasks[[i]], target.extra = TRUE)
				df_list[[i]] = td$data
				target = td$target
			}
			
			target_name = mlr::getTaskTargetNames(self$tasks[[1]])
			target_df = data.frame(td$target)
			colnames(target_df) = c(target_name)
			df_list[[length(df_list) + 1]] = target_df			
			combined_df = do.call(cbind, df_list)

			if (self$model_type == "SURV") {		
				tsk = mlr::makeSurvTask(id = "concat", data = combined_df, target = target_name, fixup.data = "no", check.data = FALSE)
			} else {
				tsk = mlr::makeClassifTask(id = "concat", data = combined_df, target = target_name, fixup.data = "no", check.data = FALSE)
			}	
			return(list("concat" = tsk))
		},

    #' @description 
		#' Convert a multiclass dataset to multiple binary datasets using the one-versus-one approach
		#' All combinations of one-versus one are provided.
    #' @param dataset The dataset to be converted
		#' @param target_var The name of the target variable 
    #' @return A list of binary datasets
		#' @export
		binarise_OVO = function(dataset, target_var)
		{
			stopifnot(target_var %in% colnames(dataset))
			classes = unique(dataset[, target_var])
			binary_sets = list()
			class_combns = combn(classes, 2)
			for (i in ncol(class_combns)) {
				binary_sets[[i]] = c(which(dataset[, target_var] == class_combns[1, i]), 
														 which(dataset[, target_var] == class_combns[2, i]))
			}
			return (binary_sets)
		},

    #' @description 
		#' Convert a multiclass dataset to multiple binary datasets using the one-versus-all approach
    #' @param dataset The dataset to be converted
		#' @param target_var The name of the target variable 
    #' @return A list of binary datasets
		#' @export		
		binarise_OVA = function(dataset, target_var)
		{
			stopifnot(target_var %in% colnames(dataset))
			classes = unique(dataset[, target_var])
			binary_sets = list()

			for (cls in classes) {
				binary_sets[[i]] = dataset
				levels(binary_sets[[i]][, target_var])[levels(binary_sets[[i]][, target_var]) != cls] = "REST"
			}
			return(binary_sets)
		},		

    #' @description 
		#' Convert a multiclass dataset to multiple binary datasets according to the type specified
    #' @param classfn_type The type of conversion to be done
    #' @param dataset The dataset to be converted
		#' @param target_var The name of the target variable 
    #' @return A list of binary datasets
		#' @export		
		binarise = function(classfn_type, dataset, target_var)
		{
				if (classfn_type == "OVO") {
					data_list = binarise_ovo(dataset, target_var)
				} else if (classfn_type == "OVA") {
					data_list = binarise_ova(dataset, target_var)
				} else {
					datalist = list(dataset)
				}
				return(data_list)
		}
	)
)
