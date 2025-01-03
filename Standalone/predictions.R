code_dir = "C:/Users/Annette/Documents/Liver Cancer Project/Experiments/R Code/"
predn_dir = "C:/Users/Annette/Documents/Liver Cancer Project/Experiments/Predictions/"
setwd(code_dir)

source("prediction_summary.R")

filenames = list.files(path = predn_dir, pattern = "results_.*4_predns.csv")
print(filenames)

predn = PrednSummary$new(predn_dir, filenames)
predn$combine()
predn$write(paste0(predn_dir, "combined"))

