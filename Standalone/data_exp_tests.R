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
source("filter_sis.R")
source("performance_classfn.R")
source("predictions_classfn.R")
source("data_helpers.R")


#
# Run Data Exploration and plot graphs
#
data_exp_tests = function(config, res_index, big, ovr_class, concat = FALSE) 
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
	dir = get_data_dir()
	tasks = create_tasks(dir, config, TASK_CLASSIF, ovr_class, big)
#	norm_techniques = list("NONE", "LOGT", "MINMAX", "QUANTILE", "ZSCORE", "MEDIAN", "CPM_LOGT", "CPM")

	if (concat) {
		tasks = concat_tasks(tasks)
		if (length(config$baseModels) > 1) {
			config$baseModels[2:length(config$baseModels)] = NULL
		}
	}

	
	#-----------------------------------------------------------------------------------------------------------------------------
	# MODELING 
	#-----------------------------------------------------------------------------------------------------------------------------	
	print("Memory used:")
	print(mem_used())

	for (i in 1:length(tasks)) {
		mod = get_modality(config, i)
		print(paste0("Modality: ", mod))
		dat = getTaskData(tasks[[i]], target.extra = TRUE)
		if (mod == "METH") {
			print(dat$data[1:20, 1:5])
		}
		imp_data = imputeData(dat$data, config$baseModels[[i]]$imputation, NULL)
		if (any(is.na(imp_data))) {
			print("Missing values present after imputation!")
		} else {			
#			for (norm in norm_techniques) {
#				print(paste0("Normalising with ", norm))
#				data = normaliseData(imp_data$data, norm)
#				file_prefix = paste0(mod, "_", norm, "_", res_index)

				data = imp_data$data
				file_prefix = paste0(mod, "_", res_index)
				
				exploreData("PCA", data, dat$target, dir, file_prefix)
				exploreData("TSNE", data, dat$target, dir, file_prefix)
				exploreData("UMAP", data, dat$target, dir, file_prefix)
#			}
		}
	}
		
	print(warnings())
  print(Sys.time())
}