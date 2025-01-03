library(mlr)
library(mlrCPO)
library(arrow)
library(tidyverse)
library(Rtsne)
library(ggplot2)
library(ggfortify)
library(M3C)
library(data.table)
library(sqldf)


#
# Get the directory in which the data is stored (machine dependent)
#
get_data_dir = function()
{
	if (Sys.info()['nodename'] == 'nemesis.cse.unsw.edu.au') {		# Nemsis in CSE
		data_dir = "./"
	} else if (substring(Sys.info()['nodename'], 1, 1) == 'k') {	# Katana
		data_dir = "/srv/scratch/z8039617/"
	} else {																											# Local
		data_dir = "C:/Users/Annette/Documents/Liver Cancer Project/Experiments/Data/"
	}
	return(data_dir)
}

#
# Get the directory in which the code is stored (machine dependent)
#
get_code_dir = function()
{
	if (Sys.info()['nodename'] == 'nemesis.cse.unsw.edu.au')
	{
		code_dir = "./"
	} else if (substring(Sys.info()['nodename'], 1, 1) == 'k') {
		code_dir = "./"
	} else {
		code_dir = "C:/Users/Annette/Documents/Liver Cancer Project/Experiments/R Code/"
	}
	return(code_dir)
}

#
# Calculate the weights for an imbalanced dataset.
# To be passed to train()
#
calc_class_weights = function(target_dat) {
#	print("In calc_class_weights")
	classes = unique(target_dat)
	n_classes = length(classes)
	class_weights = list()
	
	for (cls in classes) {
		n_cls = length(target_dat[target_dat == cls])
		class_weights[cls] = length(target_dat) / (n_classes * n_cls)
	}

	weights = unlist(class_weights[target_dat])
	return(weights)
}

# 
# Run SMOTE to balance the data.
# For multi-class data, run on each pair of classes until balanced
#
balance_task = function(task, target_name, method = "SMOTE") {
	print(paste0("Balancing data: ", method))
	dat = getTaskData(task, target.extra = FALSE)
	task_id = task$task.desc$id
	
	# Convert any integer columns to numeric
	col_order = colnames(dat)[1:ncol(dat)]
	numeric_cols = unlist(lapply(dat, is.numeric))
	if (sum(numeric_cols) > 0) {
		num_dat = as.data.frame(lapply(dat[, numeric_cols, drop = FALSE], as.numeric))
		if (sum(!numeric_cols) > 0)
		{
			dat = cbind(dat[, !numeric_cols, drop = FALSE], num_dat)
		} else {
			dat = num_dat
		}
	}
	dat = dat[, col_order]
	
	class_sizes = table(dat[, target_name])
	print(class_sizes)
	max_class_size = max(class_sizes)
	max_class_name = names(which.max(class_sizes))
	max_ind = which(dat[, target_name] == max_class_name)
	balanced_dat = dat[max_ind, ]
	
	for (cls in names(class_sizes)) {
		print(paste0("Balancing class: ", cls))
		if (class_sizes[cls] < max_class_size / 1.5) {
			cls_ind = which(dat[, target_name] == cls)
			newdat = rbind(dat[max_ind, ], dat[cls_ind, ])
			newdat[, target_name] = as.factor(as.character(newdat[, target_name]))
			binary_task = makeClassifTask(id = cls, 
																		data = rbind(dat[max_ind, ], dat[cls_ind, ]), 
																		target = target_name,
																		fixup.data = "no")
			rate = round(class_sizes[max_class_name] / class_sizes[cls], 1)
			
			if (method == "SMOTE") {
					bal_task = smote(binary_task, rate)
			} else if (method == "OVERSAMPLE") {
					bal_task = oversample(binary_task, rate)
			}
			bal_dat = getTaskData(bal_task, target.extra = FALSE)
			bal_ind = which(bal_dat[, target_name] == cls)
			balanced_dat = rbind(balanced_dat, bal_dat[bal_ind, ])
		} else if (cls != max_class_name) {
			cls_ind = which(dat[, target_name] == cls)
			balanced_dat = rbind(balanced_dat, dat[cls_ind, ])
		}
	}
	
	old_rows = rownames(balanced_dat)
	char_rows = is.na(as.numeric(old_rows))
	rownames(balanced_dat) = NULL
	rownames(balanced_dat)[char_rows] = old_rows[char_rows]
	class_sizes = table(balanced_dat[, target_name])
	print(class_sizes)

	balanced_task = makeClassifTask(id = getTaskId(task), data = balanced_dat, target = target_name, fixup.data = "no", check.data = FALSE)
	return (balanced_task)
}

#
# Perform pre-processing on a single dataset
#
prepare_data = function(config, idx, raw, id_col, ovr_class, task_type, prepend = FALSE) {
	print("In prepare_data")
  dataset = as.data.frame(raw)
	
  # Remove the patient ID, as it becomes the row name. 
	# Replace '-' in target column as it is converted to '.' and upsets plotting later
  dataset = dataset[ , !colnames(dataset) == id_col]
	colnames(dataset) = make.names(colnames(dataset), unique = TRUE)
	if (task_type == TASK_CLASSIF) {
		dataset[, config$targetVar] = gsub('-', '_', dataset[, config$targetVar])
	
		# If doing One-vs-Rest modelling, convert the target var to binary
		if (!is.null(ovr_class)) {
			dataset[, config$targetVar][dataset[, config$targetVar] != ovr_class] = "REST"
		}
	}

	# When reading data from the config file, the value returned 
	# will be NULL if the column does not exist in the config file
	# and NA if the column exists but the cell is empty.
	# Therefore we must check for both.

	# Convert categorical features to factors and others to numerics
	if (class(config) == "config_single") {
		cat_str = config$categoricals
	} else {
		cat_str = config$dataSets[[idx]]$categoricals
	}
#	print(cat_str)
	categoricals = c()
	if (!is.null(cat_str) && !is.na(cat_str)) {
		categoricals = unname(unlist(strsplit(cat_str, split = ",", fixed = TRUE)))
		categoricals = intersect(categoricals, colnames(dataset))
#		print(categoricals)
#		print(colnames(dataset))
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
}


identify_relevant = function(config, dir, task_type, row_names, filter_zeroes = 90.0, filter_missings = 50.0, filter_corr = FALSE, filter_var = FALSE) {
	print("In identify_relevant")
	relevant = list()
	
	for (i in 1:length(config$dataSets)) {
		mod = get_modality(config, i)
		print(mod)
		raw = read_raw(dir, config, i, row_names, NULL, FALSE, task_type)
		dataset = as.data.frame(raw)
		dataset = dataset[ , !colnames(dataset) %in% c(config$targetVar, config$timeVar, config$statusVar)]
		print(paste0("Number of features in raw dataset: ", ncol(dataset)))
		
		if (is.null(relevant[[mod]])) {
			relevant[[mod]] = list()
		}		
	
		# Identify features that have more than 50% missing values
		drops_na = c()
		if (any(is.na(dataset))) {
			count_na = as.data.frame(colSums(is.na(dataset)))
			drops_na = rownames(count_na)[which(count_na > nrow(dataset)/ 2)]
		}
		print(paste0("Features that have more than 50% missing values:", length(drops_na)))
		
		# Identify features that have more than 90% zero values
		count_nz = as.data.frame(colSums(dataset == 0, na.rm = TRUE) / nrow(dataset)*100)
		drops_nz = rownames(count_nz)[which(count_nz > 90)]
		print(paste0("Features that have more than 90% zero values:", length(drops_nz)))
		drops = c(drops_na, drops_nz)
		if (length(drops) > 0) {
			dataset = as.data.frame(dataset[ , !(names(dataset) %in% drops)])
		}

		# Identify features that are highly correlated
		drops_cor = c()
#		print(paste0("filter_corr = ", filter_corr))
		if (filter_corr) {
			numeric_cols = unlist(lapply(dataset, function(x) {is.numeric(x) && !all(x %in% c(0,1))}))
			numeric_data = dataset[, numeric_cols]
			cor_mat = abs(cor(numeric_data))
			cor_mat[!lower.tri(cor_mat)] = 0
			drops_cor = names(which(!apply(cor_mat,2,function(x) all(x<=0.80))))
			print(paste0("Features that are highly correlated:", length(drops_cor)))
			if (length(drops_cor) > 0) {
				dataset = as.data.frame(dataset[ , !(names(dataset) %in% drops_cor)])
			}
		}
			
		drops_var = c()
#		print(paste0("filter_var = ", filter_var))
		fs_ratio = ncol(dataset) / nrow(dataset)
		print(paste0("fs_ratio = ", fs_ratio))
		if (filter_var && fs_ratio > 10) {
			dfvar = sapply(dataset, var, na.rm = TRUE)
			dfvar = dfvar[order(dfvar, decreasing = TRUE, na.last = TRUE)]
			if (length(dfvar) > 500) {
				drops_var = names(dfvar[501:length(dfvar)])
			}
			print(paste0("Features with low variance:", length(drops_var)))
		}

		drops = c(drops_na, drops_nz, drops_cor, drops_var)
		print(paste0("Identified ", length(drops), " irrelevant features in total"))
		relevant[[mod]] = setdiff(colnames(dataset), drops)
		print(paste0("Keeping ", length(relevant[[mod]]), " relevant features"))
		rm(raw)
		rm(dataset)
	}
	return(relevant)
}


#
# Get the modality of the file with the given index
#
get_modality = function(config, idx) {
	if (class(config) == "config_single") {
		mod = config$modality
	} else {
		if (is.na(config$dataSets[[idx]]$modality) || is.null(config$dataSets[[idx]]$modality)) {
			mod = strsplit(config$dataSets[[idx]]$dataset, "_", fixed = TRUE)[1:4]
		} else {
			mod = config$dataSets[[idx]]$modality
		}
	}
	return(mod)
}

#
# Read in a single raw file with the given index
#
read_raw = function(dir, config, idx, row_names = 'ID', selected = NULL, prepend = FALSE, task_type = TASK_CLASSIF, validation = FALSE) {
	print(paste0("In read_raw: ", task_type))
	if (class(config) == "config_single") {
		transposing = FALSE
		exclusions = c()
		inclusions = c()
		filename = paste(dir, config$dataset, sep = "/")
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
			exclusions = c()
		}	
		
		# Get included features
		incl_str = datasets[[idx]]$inclusions
		if (!is.null(incl_str) && !is.na(incl_str)) {
			inclusions = unlist(strsplit(incl_str, split = ",", fixed=TRUE))
			inclusions = make.names(inclusions)
		} else {
			inclusions = c()
		}

		filename = paste(dir, datasets[[idx]]$dataset, sep = "/")
		print(filename)
	}
	mod = get_modality(config, idx)
	
	if (is.null(selected) || (length(selected) == 0)) {
#		raw = as.data.frame(read_csv_arrow(filename, read_options=list(block_size=2147483647), col_names = TRUE))
		raw = read.csv.sql(filename, header = TRUE, row.names = row_names, eol = "\r\n")
		raw[raw == ''] = NA
		print(paste0("Read raw data: number of columns = ", ncol(raw)))
	} else {
		selected = selected[!selected %in% c(exclusions, "")]
		if (length(inclusions) > 0) {
			selected = selected[selected %in% c(inclusions, "")]
		}
		if (task_type == TASK_SURV) {
			selected = c(row_names, config$timeVar, config$statusVar, selected)
		} else {
			selected = c(row_names, config$targetVar, selected)
		}

		if (transposing) {
			print(paste0("select * from file where ID in ('", paste(selected, collapse = "','"), "')" ))
			raw = read.csv.sql(filename, header=TRUE, row.names = row_names, eol = "\r\n", sql = paste0("select * from file where ID in ('", paste(selected, collapse = "','"), "')" ))
			raw[raw == ''] = NA		
		} else {
			print(paste0("select `", paste(selected, collapse = "`,`"), "` from file"))
			raw = read.csv.sql(filename, header=TRUE, row.names = row_names, eol = "\r\n", sql = paste0("select `", paste(selected, collapse = "`,`"), "` from file"))
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
#	unknown = unlist(lapply(raw, function(x) {class(x) == "vctrs_unspecified"}))
#	raw[,unknown] = sapply(raw[,unknown], function(x) as.numeric(x))
	
	# Remove exclusions, keep inclusions
	print("Remove exclusions, keep inclusions")
	if (class(config) != "config_single") {
		raw = raw[,!(colnames(raw) %in% c(exclusions, "")), drop = FALSE]
		target = raw[, colnames(raw) %in% c(config$targetVar, config$timeVar, config$statusVar), drop = FALSE]
		rnames = rownames(raw)
		if (length(inclusions) > 0) {
			raw = raw[,(colnames(raw) %in% c(inclusions, "")), drop = FALSE]
		}
		raw[, colnames(target)] = target
		rownames(raw) = rnames
	}

	# Remove ID var
	raw = raw[, !colnames(raw) %in% row_names]
	return(as.data.frame(raw))
}

valid_hv_metric = function(value) { 
		return (!is.null(value) && !is.na(value) && value %in% c("VAR", "IQR", "DI")) 
}

valid_hv_threshold = function(value) { 
		return (!is.null(value) && !is.na(value) && value %in% c("FIXED", "PERCENT", "QUANTILE")) 
}

#Function nzv from Caret package - full acknowledgement to Max Kuhn
nzv <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = FALSE)
{
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  freqRatio <- apply(x, 2, function(data)
  {
    t <- table(data[!is.na(data)])
    if (length(t) <= 1) {
      return(0);
    }
    w <- which.max(t);
    return(max(t, na.rm=TRUE)/max(t[-w], na.rm=TRUE))
  })
  lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
  percentUnique <- 100 * lunique / apply(x, 2, length)
  zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
  if (saveMetrics)
  {
    out <- data.frame(freqRatio = freqRatio,
                      percentUnique = percentUnique,
                      zeroVar = zeroVar,
                      nzv = (freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
  }
  else {
    out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
    names(out) <- NULL
    if(names){
      out <- colnames(x)[out]
    }
  }
  out
}


#
# *** NO LONGER USED ***
# Determine the features in use in the analysis
# For small files, all features will be used
# For very large files, only the highly variable features that are common across all files in a modality will be used
#
get_hv_features = function(dir, config, row_names) {
	print("In get_hv_features")
	high_var = list()
	
	for (i in 1:length(config$dataSets)) {
		mod = get_modality(config, i)
		print(mod)
		raw = read_raw(dir, config, i, row_names, NULL, FALSE)
		
		if (is.null(high_var[[mod]])) {
			high_var[[mod]] = list()
		}
		
		if (ncol(raw) > 400) {
			# For large files, only keep the high variance features			
			dat = NULL
			hv_metric = config$hv_metric
			if (!valid_hv_metric(hv_metric)) {
				hv_metric = "VAR"
			}
			if (hv_metric == "VAR") { # Variance
				dat = na.omit(sapply(raw, function(x) {var(x, na.rm = TRUE)}))
			} else if (hv_metric == "IQR") {
				dat = na.omit(sapply(raw, function(x) {IQR(x, na.rm = TRUE)}))
			} else if (hv_metric == "DI") {
				dat = na.omit(sapply(raw, function(x) {var(x, na.rm = TRUE)/mean(x, na.rm = TRUE)}))
			}
			
			hv_threshold = config$hvThreshold
			hv_value = config$hvValue
			print(paste0("HV metric: ", hv_metric, ", HV Threshold: ", hv_threshold, ", HV Value: ", hv_value))
			if (!valid_hv_threshold(hv_threshold)) {
				hv_threshold = "FIXED"
				hv_value = 500
			}
			print(paste0("HV metric: ", hv_metric, ", HV Threshold: ", hv_threshold, ", HV Value: ", hv_value))
			if (hv_threshold == "FIXED") {
				hv_threshold = min(c(hv_value, length(dat)))
				feats = names(sort(dat, decreasing = TRUE))[1:hv_threshold]
			} else if (hv_threshold == "PERCENT") {
				hv_threshold = quantile(dat, prob = 1 - hv_value/100, na.rm = TRUE)
				feats = names(dat[dat > hv_threshold])
			} else if (hv_threshold == "QUANTILE") {
				hv_threshold = quantile(dat, probs=c(hv_value/100), na.rm = TRUE)
				feats = names(dat[dat > hv_threshold])
			}		
		} else {
			# For smaller files keep all features
			feats = names(raw)
		}
		feats = feats[!(feats %in% c("", config$targetVar, config$timeVar, config$statusVar, row_names))]
		print(paste0("Number of features kept: ", length(feats)))
		high_var[[mod]][[config$targetVar]] = feats
		rm(raw)
  }
	
	# Determine which high variance features are common across all files when there are multiple files per modality
	common_names = list()
	for (i in 1:length(high_var)) {
		mod = names(high_var)[[i]]
		common_names[[mod]] = Reduce(intersect, high_var[[mod]])
		print(paste0("Number of common features for modality ", mod, " = ", length(common_names[[mod]])))
	}
	return(common_names)
}

#
# Read in the raw data files and return in a list, one df per modality
# When there are multiple files per modality,  bind these into one df.
# Only used after the HV features have been identified
#
read_raw_data = function(dir, config, row_names, selected_names, task_type, validation) {
	print("In read_raw_data")
	raw_data = list()
		
	for (i in 1:length(config$dataSets)) {
		mod = get_modality(config, i)
		print(mod)
		raw = read_raw(dir, config, i, row_names, selected_names[[mod]], TRUE, task_type, validation)
		raw_data[[mod]] = rbind(raw_data[[mod]], raw)
		rm(raw)
  }
	return(raw_data)
}


#
# Create the tasks for each dataset and store in a list
#
create_tasks = function(data_dir, config, task_type, ovr_class, big = FALSE, subset = NULL, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE)
{
	print("In create_tasks")
  tasks = list()
	row_names = ifelse(is.null(config$idVar) || is.na(config$idVar), "ID", config$idVar)
	
	rel_features = identify_relevant(config, data_dir, task_type, row_names, filter_zeroes, filter_missings, filter_corr, filter_var)
	raw_data = read_raw_data(data_dir, config, row_names, rel_features, task_type, FALSE)

	for (i in 1:length(raw_data)) {
		task_id = names(raw_data)[[i]]
		print(task_id)
		dat = prepare_data(config, i, raw_data[[i]], row_names, ovr_class, task_type)

		if (task_type == TASK_CLASSIF) {		
#			tsk = makeClassifTask(id = task_id, data = dat, target = config$targetVar, fixup.data = "no", check.data = FALSE)
			tsk = makeClassifTask(id = task_id, data = dat, target = config$targetVar)
		} else if (task_type == TASK_SURV) {
			tsk = makeSurvTask(id = task_id, data = dat, target = c(config$timeVar, config$statusVar), fixup.data = "no", check.data = FALSE)
		}
		tasks[[task_id]] = tsk
	}
	
	if (!is.null(subset)) {
		tasks = tasks[subset]
		tasks = tasks[!is.na(tasks)]
	}

  return(tasks)
}


create_validation_tasks = function(base_tasks, data_dir, config, task_type, ovr_class, big = FALSE, subset = NULL, filter_zeroes = 90, filter_missings = 50, filter_corr = FALSE, filter_var = FALSE)
{
	print("In create_validation_tasks")
  tasks = list()
	row_names = ifelse(is.null(config$idVar) || is.na(config$idVar), "ID", config$idVar)
	
	rel_features = list()
	for (j in 1:length(base_tasks)) {
		dat = getTaskData(base_tasks[[j]])
		print(paste0("Number of samples in base tasks = ", nrow(dat)))
		rel_features[[j]] = colnames(dat)
	}
	raw_data = read_raw_data(data_dir, config, row_names, rel_features, task_type, TRUE)

	for (i in 1:length(raw_data)) {
		task_id = names(raw_data)[[i]]
		print(task_id)
		dat = prepare_data(config, i, raw_data[[i]], row_names, ovr_class, task_type)
		print(paste0("Number of samples in validation tasks = ", nrow(dat)))
		old_dat = getTaskData(base_tasks[[i]], target.extra = FALSE)
		new_dat = rbind(old_dat, dat)
		print(paste0("Number of samples in full tasks = ", nrow(new_dat)))

		if (task_type == TASK_CLASSIF) {		
			tsk = makeClassifTask(id = task_id, data = new_dat, target = config$targetVar)
		} else if (task_type == TASK_SURV) {
			tsk = makeSurvTask(id = task_id, data = new_dat, target = c(config$timeVar, config$statusVar), fixup.data = "no", check.data = FALSE)
		}
		tasks[[task_id]] = tsk
	}
	
	if (!is.null(subset)) {
		tasks = tasks[subset]
		tasks = tasks[!is.na(tasks)]
	}

  return(tasks)
}


create_ovr_tasks = function(tasks, classes, target_var) {
	print("Creating ovr_tasks")
	print(target_var)
	ovr_tasks = list()
	ovr_classes = list()
	
	for (cls in classes) {
		print(cls)
		ovr_tasks[[cls]] = list()
		for (tsk in tasks) {
			print(tsk)
			dat = getTaskData(tsk)
			print(head(dat))
			dat[, target_var] = as.character(dat[, target_var])
			dat[, target_var][dat[, target_var] != cls] = "REST"
#			dat[, target_var] = as.factor(dat[, target_var])
			print(dat[, target_var])
			print(tsk$task.desc)			
			ovr_tasks[[cls]][[tsk$task.desc$id]] = makeClassifTask(id = tsk$task.desc$id, data = dat, target = target_var)
		}
		ovr_classes[[cls]] = factor(unique(dat[, target_var]))
#		ovr_classes[[cls]] = unique(dat[, target_var])

	}
	return(list("tasks" = ovr_tasks, "classes" = ovr_classes))
}


#
# Trick mlr - change number of classes
# Overrides check for different number of classes in training and test
#
update_task_classes = function(model, classes, target) {
		print("Updating model classes to ")
		print(classes)
		model$task.desc$class.levels = as.character(classes)
		if (!is.null(target)) {
			model$task.desc$class.distribution = table(target)
		}
		return(model)		
}


#
# Concatenate the data in all tasks to form a single task
#
concat_tasks = function(tasks, task_type = TASK_CLASSIF)
{
	print("In concat_tasks")
	df_list = list()
	for (i in 1:length(tasks)) {
		td = getTaskData(tasks[[i]], target.extra = TRUE)
		df_list[[i]] = td$data
	}
	
	target_name = getTaskTargetNames(tasks[[1]])
	target_df = data.frame(td$target)
	colnames(target_df) = c(target_name)
	df_list[[length(df_list) + 1]] = target_df
	combined_df = do.call(cbind, df_list)
	
	if (task_type == TASK_CLASSIF) {		
		tsk = makeClassifTask(id = "concat", data = combined_df, target = target_name, fixup.data = "no", check.data = FALSE)
	} else if (task_type == TASK_SURV) {
		tsk = makeSurvTask(id = "concat", data = combined_df, target = target_name, fixup.data = "no", check.data = FALSE)
	}	
	return(list("concat" = tsk))
}


#
# Create the learners for each dataset and store in a list
# Steps are added to the ML pipeline in reverse order
#
getArgs = function(...) return(list(...))

create_learners = function(config, base_learners, base_filters, pred_type, balance = FALSE, cache_dir, subset = NULL, model_name = NULL)
#create_learners = function(config, base_learners, base_filters, pred_type, weights = NULL, cache_dir, subset = NULL, model_name = NULL)
{
	print(paste0("In create_learners: ", pred_type))
  learners = list()
	
	for (i in 1:length(config$baseModels)) {
		if (is.na(config$baseModels[[i]]$params) || (length(config$baseModels[[i]]$params) == 0)) {
			pars = list()
		} else if (is.character(config$baseModels[[i]]$params)) {
			pars = eval(parse(text=sprintf("getArgs(%s)", config$baseModels[[i]]$params)))
		} else {
			pars = config$baseModels[[i]]$params
		}

		if (is.na(config$baseModels[[i]]$fsparams) || (length(config$baseModels[[i]]$fsparams) == 0)) {
			fspars = list()
		} else if (is.character(config$baseModels[[i]]$fsparams)) {
			fspars = eval(parse(text=sprintf("getArgs(%s)", config$baseModels[[i]]$fsparams)))
		} else {
			fspars = config$baseModels[[i]]$fsparams
		}
		
		#	Begin pipeline with basic learner
    baselrn = base_learners[[config$baseModels[[i]]$learner]]
		if (is.null(model_name)) {
			model_name = baselrn$name
		}
    lrn = do.call(makeLearner, args = append(list("cl" = baselrn$class, "id" = model_name, "predict.type" = pred_type, predict.hv_threshold = NULL, fix.factors.prediction = TRUE), pars))
#		if (!is.null(weights)) {
#			lrn = makeWeightedClassesWrapper(lrn, wcw.weight = weights)
#		})
		
		# Add feature selection to pipeline
		basefilt = base_filters[[config$baseModels[[i]]$featsel]]
		if (!is.null(basefilt)) {
			filter_args = list("learner" = lrn, "fw.method" = basefilt$method, "cache" = cache_dir)
#			filter_args = list("learner" = lrn, "fw.method" = basefilt$method, "cache" = FALSE)
			lrn = do.call(makeFilterWrapper, args = c(filter_args, fspars))
		}
		
		#Add multi-class balancing to the pipeline if requested
		if (balance) {
			lrn = makePreprocWrapperBalanceMC(lrn, config$targetVar, "SMOTE")
		}
						
		#	Add normalisation to pipeline
		lrn = cpoNormalise(config$baseModels[[i]]$norm) %>>% lrn
#		lrn = makePreprocWrapperNormalise(lrn, config$baseModels[[i]]$norm)
		
		#Add imputation to the pipeline - use cop as it adds the missings property
		lrn = cpoImputeData(config$baseModels[[i]]$imputation, NULL) %>>% lrn
#		lrn = makePreprocWrapperImpute(lrn, config$baseModels[[i]]$imputation)

    learners[[i]] = lrn
  }
	
	if (!is.null(subset)) {
		learners = learners[subset]
		learners = learners[!is.na(learners)]
	}
  return(learners)
}

	
# Create an mlr prediction object using the specified results
make_mlr_prediction = function(results, task) {
	p = makeS3Obj(c("PredictionClassif", "Prediction"),
		predict.type = "response",
		data = results,
		hv_threshold = NA_real_,
		task.desc = task$task.desc,
		time = NULL,
		error = NULL,
		dump = NULL
	)
	return(p)
}


#
# Convert the multiclass problem to multiple binary classifiers using the OVO approach
# Returns a list of list of indices, each sublist containing the indices to be included
#
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
}


#
# Convert the multiclass problem to multiple binary classifiers using the OVA approach
# Returns a list of new binary datasets
#
binarise_OVA = function(dataset, target_var)
{
	stopifnot(target_var %in% colnames(dataset))
	classes = unique(dataset[, target_var])
	binary_sets = list()

	for (cls in classes) {
		binary_sets[[i]] = dataset
		levels(binary_sets[[i]][, target_var])[levels(binary_sets[[i]][, target_var]) != cls] = "REST"
	}
	return (binary_sets)
}

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