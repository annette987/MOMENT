NUM_DATASETS = 8

prepare_data = function(dataset) {
  #Remove the patient ID
  dataset = dataset[ , !startsWith(names(dataset), "ID")]
  
  # Drop features that have > 50% missing values or <= 10% non-zero values
  count_na = as.data.frame(colSums(is.na(dataset)))
  drops_na = rownames(count_na)[which(count_na > nrow(dataset)/2)]
  count_nz = as.data.frame(colSums(dataset != 0, na.rm = TRUE) / nrow(dataset)*100)
  drops_nz = rownames(count_nz)[which(count_nz <= 10)]
  drops = c(drops_na, drops_nz)
  dataset = as.data.frame(dataset[ , !(names(dataset) %in% drops)])
  print("Dropping the following columns that have too many missing or zero values:")
  print(drops)
  
  # Separate target variable
  data = list()
  data$Y = dataset['Label']
  data$X = dataset[, !(names(dataset) %in% c("Label"))]
  return(data)
}

setwd("C:/Users/Annette/Documents/Liver Cancer Project/Experiments/Data")
datasets = list()
datasets[["clinical"]] = read.csv("Clinical_cmn.csv", row.names = 1, sep = ",", dec = '.', header = TRUE)
datasets[["cytokine"]] = read.csv("Cytokine_cmn.csv", row.names = 1, sep = ",", header = TRUE)
datasets[["pathology"]] = read.csv("Pathologic_cmn.csv", row.names = 1, sep = ",", header = TRUE)
metab1 = read.csv("Metabolomic1_cmn.csv", row.names = 1, sep = ",", header = TRUE)
metab2 = read.csv("Metabolomic2_cmn.csv", row.names = 1, sep = ",", header = TRUE)
datasets[["metabolomic"]] = cbind(metab1, metab2)
datasets[["oral_genus"]] = read.csv("oral_genus_num_cmn.csv", row.names = 1, sep = ",", header = TRUE)
datasets[["oral_species"]] = read.csv("oral_species_num_cmn.csv", row.names = 1, sep = ",", header = TRUE)
datasets[["stool_genus"]] = read.csv("stool_genus_num_cmn.csv", row.names = 1, sep = ",", header = TRUE)
datasets[["stool_species"]] = read.csv("stool_species_num_cmn.csv", row.names = 1, sep = ",", header = TRUE)

data_concat = list()
for (i in seq(1, NUM_DATASETS)) {
  print(names(datasets[i]))
  if (names(datasets[i]) == "clinical") {
    datasets[[i]] = datasets[[i]][, !(names(datasets[[i]]) %in% c("Aetiology"))]
  }
  data_concat[[i]] = prepare_data(datasets[[i]])
}
names(data_concat) = names(datasets)

concat_genus = cbind(data_concat$clinical$Y, data_concat$clinical$X, data_concat$cytokine$X, data_concat$pathology$X, data_concat$oral_genus$X, data_concat$stool_genus$X)
concat_species = cbind(data_concat$clinical$Y, data_concat$clinical$X, data_concat$cytokine$X, data_concat$pathology$X, data_concat$oral_species$X, data_concat$stool_species$X)
write.csv(concat_genus, "concat_genus.csv", row.names = FALSE)
write.csv(concat_species, "concat_species.csv", row.names = FALSE)

concat_genus_m = cbind(data_concat$clinical$Y, data_concat$clinical$X, data_concat$cytokine$X, data_concat$pathology$X, data_concat$metabolomic$X, data_concat$oral_genus$X, data_concat$stool_genus$X)
concat_species_m = cbind(data_concat$clinical$Y, data_concat$clinical$X, data_concat$cytokine$X, data_concat$pathology$X, data_concat$metabolomic$X, data_concat$oral_species$X, data_concat$stool_species$X)
write.csv(concat_genus_m, "concat_genus_m.csv", row.names = FALSE)
write.csv(concat_species_m, "concat_species_m.csv", row.names = FALSE)
