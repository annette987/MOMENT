results_ensemble = function(dir, suffix) {
  results = list()
  row_idx = 1
	roc_files = list.files(dir, "*_roc.csv")	
	
	for (filename in roc_files) {
		dataset_split = strsplit(filename, "_", fixed = TRUE)
		dataset = dataset_split[[1]][2]
		agg_thresh = dataset_split[[1]][4]
		agg_thresh = strsplit(agg_thresh, '.', fixed = TRUE)[[1]][2]
		if (dataset %in% c('oral', 'stool')){
			dataset = paste0(dataset, "_", dataset_split[[1]][3])
		}
		
		roc_data = read.csv(paste(dir, filename, sep="/"), header=TRUE, row.names=NULL, check.names=FALSE, fileEncoding="UTF-8-BOM")
		if (!dataset %in% names(results)) {
			results[[dataset]] = data.frame()
		}
		
		names = strsplit(colnames(roc_data)[1], '.', fixed = TRUE)
		method  = names[[1]][1]
		curr_row = cbind(data.frame("filter" = method, "threshold" = agg_thresh), 
											as.data.frame(t(colMeans(roc_data))))
		names(curr_row) = c("filter", "threshold", 
												"CIR.sens", "CIR.spec", "CIR.F1", "CIR.acc",
												"CON.sens", "CON.spec", "CON.F1", "CON.acc",
												"LN.sens", "LN.spec", "LN.F1", "LN.acc",
												"LX.sens", "LX.spec", "LX.F1", "LX.acc")
		results[[dataset]] = rbind(results[[dataset]], curr_row)
		row_idx = row_idx + 1
	}

	print("Saving")
  for (ds in names(results)) {
		print(ds)
		df  = results[[ds]]
    write.csv(df[order(df$filter, df$threshold), ], paste0(res_dir, "/results_", ds, ".csv"), row.names=FALSE)
  }
}
