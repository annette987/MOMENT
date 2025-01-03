library(mice)
library(mlr)
library(mlrCPO)
library(dplyr)
library(BBmisc)
library(pryr)
library(IntNMF)

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
int_nmf_tests = function(config, res_index, big = FALSE) 
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
	tasks = create_tasks(dir, config, config$targetVar, big)

	
	#-----------------------------------------------------------------------------------------------------------------------------
	# MODELING 
	#-----------------------------------------------------------------------------------------------------------------------------	
	print("Memory used:")
	print(mem_used())

	dat = list()
	for (i in 1:length(tasks)) {
		print(paste0("i = ", i))
		print(paste0("Task = ", names(tasks)[[i]]))
		tdata = getTaskData(tasks[[i]], target.extra = TRUE)
		imp_data = imputeData(tdata$data, config$baseModels[[i]]$imputation, NULL)$data
		if (any(is.na(imp_data))) {
			print("Missing values present after imputation!")
		} else {
			print("Removing negatives...")
			if (any(imp_data < 0)) {
#				imp_data = pmax(imp_data + abs(min(imp_data)), .Machine$double.eps, na.rm = TRUE)
				imp_data = imp_data + abs(min(imp_data))
				if (any(is.na(imp_data))) {
					print("Missing values present after removing negatives!")
				}
			}
		}
		dat[[i]] = imp_data
	}
	print("Setup done")
	
	#opt.k <- nmf.opt.k(dat=dat, n.runs=5, n.fold=5, k.range=2:7, result=TRUE,
	#make.plot=TRUE, progress=TRUE)
	
	# Find clustering assignment for the samples
	print("Calling nmf.mnnals")
	fit = nmf.mnnals(dat=dat, k=8, maxiter=200, st.count=20, n.ini=15,
										ini.nndsvd=TRUE, seed=TRUE)
	tbl = table(fit$clusters)
	print(tbl)
		
	print(warnings())
  print(Sys.time())
}