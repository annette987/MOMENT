#--------------------------------------------------------
# CLASS PREDICTIONS
# Stores the predictions in each iteration
#--------------------------------------------------------

library(R6)
library(dplyr)
library(tidyr)


# Prediction object.
# Result of an experiment with the following members:
#		pred - the predictions for each sample in each iteration
#          (data.frame - rows are the samples, columns are the predictions)

Prediction = R6Class("Prediction", list(
	reshaped = NULL,
	labels = NULL,
	
	initialize = function(task) {
		dat = getTaskData(task)
		self$labels = dimnames(dat)[[1]]
	},
	
	save = function(pred) {
		df = as.data.frame(pred)
		self$reshaped = df[c('id', 'response', 'truth', 'iter')] %>%
#												mutate(label = self$labels[row_number()]) %>% 
												group_by(id) %>% 
												pivot_wider(names_from = c(iter),
																		values_from = c(response))
		print(self$reshaped)																	
	},
	
	read = function(folder, filename) {

	},
		
	write = function(result_file, suffix = "", method = "") {
		if (suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		write.csv(self$reshaped[order(self$reshaped$id),], paste(result_file, "_predns.csv", sep=""), row.names=FALSE, na="")
	})
)
