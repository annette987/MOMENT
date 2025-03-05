#' R6 base class for model configuration
#'
#' @description
#' Provides functions to read and store configuration parameters
#'
#' @name Config
NULL

#' @importFrom utils read.csv

normalisation = factor(levels = c("NONE", "STAND", "LOGT", "CPM", "CPM_LOGT"))

new_config = function(config_data, row) {
	structure(
		list(
			expType     = config_data[row, 'ExpType'],
			metaLearner = config_data[row, 'MetaLearner'],
			metaParams  = config_data[row, 'MetaParams'],
			resultFile  = config_data[row, 'ResultFile'],
			baseConfig  = config_data[row, 'BaseConfig'],
			itersOuter  = config_data[row, 'ItersOuter'],
			foldsOuter  = config_data[row, 'FoldsOuter'],
			itersInner  = config_data[row, 'ItersInner'],
			foldsInner  = config_data[row, 'FoldsInner'],
			boostIters  = config_data[row, 'BoostIters'],
			hvMetric    = config_data[row, 'HVMetric'],			
			hvThreshold = config_data[row, 'HVThreshold'],			
			hvValue     = config_data[row, 'HVValue'],			
			targetVar   = config_data[row, 'TargetVar'],
			statusVar		= config_data[row, 'StatusVar'],
			timeVar			= config_data[row, 'TimeVar'],
			idVar   		= config_data[row, 'IDVar'],
			rowNames    = config_data[row, 'RowNames'],
			dataDir     = config_data[row, 'DataDir'],
			cacheDir    = config_data[row, 'CacheDir'],
			threshold   = config_data[row, 'Threshold'],
			activeLrn   = eval(parse(text = config_data[row, 'ActiveLearners'])),
			primaryClass = config_data[row, 'PrimaryClass'],
			combns   		= config_data[row, 'Combns'],
			baseModels  = list(),
			dataSets    = list(),
			experts			= NULL
		),
		class = "config"
	)
}

new_config_base = function(base_data) {
	structure(
		list(
			modality     = base_data['Modality'],
			norm         = base_data['Normalisation'],
			imputation   = base_data['Imputation'],
			featsel      = base_data['Feature Selector'],
			fsparams 		 = base_data['FSParams'],
			learner      = base_data['Learner'],
			params 			 = base_data['Params']
		),
		class = "config_base"
	)
}

new_config_base_empty = function() {
	structure(
		list(
			modality     = "",
			norm         = "",
			imputation   = "",
			featsel      = "",
			fsparams 		 = "",
			learner      = "",
			params 			 = ""
		
		),
		class = "config_base"
	)
}

new_config_learner = function(base_data) {
	structure(
		list(
			modality     = base_data['Modality'],
			norm         = base_data['Normalisation'],
			imputation   = base_data['Imputation'],
			featsel      = NA,
			fsparams 		 = NA,
			learner      = base_data['Learner'],
			params 			 = base_data['Params']
		),
		class = "config_base"
	)
}

add_learner = function(cf, lrn, learners) {
	if (length(cf$baseModels) > 0) {
		cf$baseModels[[1]]$learner = lrn
		cf$baseModels[[1]]$norm    = "STAND"		
		cf$baseModels[[1]]$imputation = "MICE"		
		cf$baseModels[[1]]$params  = learners[[lrn]]$args
	}
	return(cf)
}

add_featsel = function(cf, lrn, featsel, learners, filters) {
	for (i in 1:length(cf$baseModels)) {
		cf$baseModels[[i]]$learner = lrn
		cf$baseModels[[i]]$params = learners[[lrn]]$args
		cf$baseModels[[i]]$featsel = featsel
		cf$baseModels[[i]]$fsparams = filters[[featsel]]$args
	}
	return(cf)
}

new_config_data = function(base_data) {
	structure(
		list(
			modality     = base_data['Modality'],
			dataset      = base_data['Dataset'],
			exclusions   = base_data['Exclusions'],
			categoricals = base_data['Categoricals'],
			transpose		 = base_data['Transpose']
		),
		class = "config_data"
	)
}

new_config_single = function(config_data, row) {
	structure(
		list(
			expType      = config_data[row, 'ExpType'],
			resultFile   = config_data[row, 'ResultFile'],
			modality     = config_data[row, 'Modality'],
			dataset      = config_data[row, 'DataFile'],
			targetVar    = config_data[row, 'TargetVar'],
			statusVar		 = config_data[row, 'StatusVar'],
			timeVar			 = config_data[row, 'TimeVar'],
			idVar    		 = config_data[row, 'IDVar'],
			imputation   = config_data[row, 'Impute'],
			norm         = config_data[row, 'Normalisation'],
			categoricals = config_data[row, 'Categoricals'],
			exclusions   = config_data[row, 'Exclusions'],
			activeLrn    = eval(parse(text = config_data[row, 'ActiveLearners']))
		),
		class = "config_single"
	)
}

#' @title Create a configuration object
#' @description 
#' Reads in the supplied configuration file, which contains information about how the model should be constructed,
#' and sets up a configuration object for use by the models.
#' @param filename (character)\cr
#' The name of teh configuration file.
#' @return A configuration object.
#' @export
make_config = function(filename) {
	stopifnot(is.character(filename))
	stopifnot(file.exists(filename))
	cf = NULL
	
	if (tools::file_ext(filename) == "csv") {
		config_data = read.csv(filename, header=TRUE, row.names=NULL, check.names=FALSE, fileEncoding="UTF-8-BOM")
		cf = new_config_single(config_data, 1)
		cf$baseModels = list(new_config_base_empty())
	} else if (tools::file_ext(filename) == "xlsx") {
		sheet_names = readxl::excel_sheets(filename)
		config_data = as.data.frame(readxl::read_excel(filename, sheet = "Main"))
		cf = new_config(config_data, 1)
		
		if (is.na(cf$dataDir) || is.null(cf$dataDir)) {
			cf$dataDir = "."
		}
		
		base_data = as.data.frame(readxl::read_excel(filename, sheet = "BaseModels"))
		cf$baseModels = apply(base_data, 1, new_config_base)
		
		if ("Datasets" %in% sheet_names) {
			data_sets = as.data.frame(readxl::read_excel(filename, sheet = "Datasets"))
			cf$dataSets = apply(data_sets, 1, new_config_data)
		} else {
			cf$dataSets = apply(base_data, 1, new_config_data)
		}
		
		if ("ValidationSets" %in% sheet_names) {
			validation_sets = as.data.frame(readxl::read_excel(filename, sheet = "ValidationSets"))
			cf$validationSets = apply(validation_sets, 1, new_config_data)
		}
		
		if ("Experts" %in% sheet_names) {
			experts = as.data.frame(readxl::read_excel(filename, sheet = "Experts"))
			cf$experts = lapply(experts, as.list)
		}
	}
	return(cf)
}

getArgs = function(...) return(list(...))


