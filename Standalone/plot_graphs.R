setwd("C:/Users/Annette/Documents/Liver Cancer Project/Experiments/R Code/")
source("roc_measures.R")

roc_file = "moe_1029_roc.csv"
data_dir = "C:/Users/Annette/Documents/Liver Cancer Project/Experiments/Results/Mix of Experts/New/"
result_file = paste0(data_dir, roc_file)

roc = ROCMultiClass$new()
roc$plot("moe", "MoE", result_file, result_file)
#roc$calc_mc_roc(as.factor(summary_responses$truth), as.factor(summary_responses$response))
roc$write(result_file)
