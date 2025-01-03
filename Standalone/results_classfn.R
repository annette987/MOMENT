results_classfn = function(dir, suffix) {
  f1_results = list()
  acc_results = list()
  
	roc_files = list.files(dir, "*_roc.csv")	
	for (filename in roc_files) {
		dataset_split = strsplit(filename, "_", fixed = TRUE)
		ds_idx = ifelse(dataset_split[[1]][1] == "results", 2, 1)
		dataset = dataset_split[[1]][ds_idx]
		if (dataset %in% c('oral', 'stool')){
			dataset = paste0(dataset, "_", dataset_split[[1]][3])
		}
		
		roc_data = read.csv(paste(dir, filename, sep="/"), header=TRUE, row.names=NULL, check.names=FALSE, fileEncoding="UTF-8-BOM")
		idx = 3
		while (idx < ncol(roc_data)) {
			names = strsplit(colnames(roc_data)[idx], '.', fixed = TRUE)
			method  = names[[1]][1]
			res_name = names[[1]][3]
			if (suffix == "subsets") {
				class_name = dataset_split[[1]][2]
			} else {
				class_name = names[[1]][2]
			}
			
			if (!class_name %in% names(f1_results)) {
				f1_results[[class_name]] = data.frame()
				acc_results[[class_name]] = data.frame()
			}
			f1_results[[class_name]][dataset, method] = mean(roc_data[,idx], na.rm = TRUE)
			acc_results[[class_name]][dataset, method] = mean(roc_data[,idx+1], na.rm = TRUE)
			idx = idx + 4
		}
	}

  for (cls in names(f1_results)) {
    write.csv(f1_results[[cls]], paste0(res_dir, "/f1_results_", cls, "_", suffix, ".csv"), row.names=TRUE)
    write.csv(acc_results[[cls]], paste0(res_dir, "/acc_results_", cls, "_", suffix, ".csv"), row.names=TRUE)
  }
}

results_classfn("* - Classification*", "classfn")
results_classfn("* - Interactions*", "inter")
results_classfn("* - Subsets*", "subsets")
