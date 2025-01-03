dir = "C:/Users/Annette/Documents/Liver Cancer Project/Experiments/Results/TCGA/"
#roc_files = list.files(dir, "adaboost_[0-9]*_roc.csv")
roc_files = list.files(dir, "results_meta_exp_[0-9]*_roc.csv")
roc_results = list()

for (filename in roc_files) {
  roc_data = read.csv(paste(dir, filename, sep="/"), header=TRUE, row.names=NULL, check.names=FALSE, fileEncoding="UTF-8-BOM")
  roc_means = colMeans(roc_data)
  f1_cols = grep("F1", colnames(roc_data))
  f1_mean = mean(roc_means[f1_cols], na.rm = TRUE)
  acc_cols = grep("acc", colnames(roc_data))
  acc_mean = mean(roc_means[acc_cols], na.rm = TRUE)

  roc_results[[length(roc_results) + 1]] = list("File" = filename, "F1" = f1_mean, "Acc" = acc_mean)
}

roc_res = as.data.frame(do.call(rbind, lapply(roc_results, unlist)))
write.csv(roc_res, paste0(dir, "/roc_summary.csv"), row.names=TRUE)