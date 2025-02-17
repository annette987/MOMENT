#--------------------------------------------------------
# CLASS PREDICTIONS
# Stores the predictions in each iteration
#--------------------------------------------------------


# Prediction object.
# Result of an experiment with the following members:
#		pred - the predictions for each sample in each iteration
#          (data.frame - rows are the samples, columns are the predictions)

Prediction = R6::R6Class("Prediction", list(
	reshaped = NULL,
	
	initialize = function() {
	},
	
	save = function(pred) {
		df = as.data.frame(pred)
		if (!('id' %in% colnames(pred))) {
			df$id = rownames(pred)
		}
		self$reshaped = df %>%
												group_by(id) %>% 
												tidyr::pivot_wider(names_from = c(iter),
																		values_from = c(response))
	},
	
	get_results = function() {
		return(self$reshaped[order(self$reshaped$id),])
	},
		
	write = function(result_file, suffix = "", method = "") {
		if (!is.null(suffix) && suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		write.csv(self$reshaped[order(self$reshaped$id),], paste(result_file, "_predns.csv", sep=""), row.names=FALSE, na="")
	})
)
