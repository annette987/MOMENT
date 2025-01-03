library(mice)

#
# Perform pre-processing on a single dataset
#
prepare_data = function(dataset) {
  #Remove the patient ID
  dataset = dataset[ , !startsWith(names(dataset), "ID")]
  
  #Identify features that have > 50% missing values or <= 10% non-zero values
  count_na = as.data.frame(colSums(is.na(dataset)))
  drops_na = rownames(count_na)[which(count_na > nrow(dataset)/2)]
  count_nz = as.data.frame(colSums(dataset != 0, na.rm = TRUE) / nrow(dataset)*100)
  drops_nz = rownames(count_nz)[which(count_nz <= 10)]
	
	#Identify collinear features
	ini = mice(dataset, maxit=0)
	drops_col = as.character(ini$loggedEvents[ini$loggedEvents$meth == 'collinear', "out"])
	print("Collinear:")
	print(collinear)

	#Drop unhelpful features
  drops = c(drops_na, drops_nz, drops_col)	
  dataset = as.data.frame(dataset[ , !(names(dataset) %in% drops)])
  print(paste0("Dropping columns that have too many missing or zero values or are collinear:", length(drops)))
  return(dataset)
}
