#--------------------------------------------------------
# CLASS TUNING
# Stores results of tuning
#--------------------------------------------------------


# Tuning object.
# Results of the tuning runs from an experiment with the following members:

Tuning = R6::R6Class("Tuning", list(
	tune_res = NULL,
			
	save = function(res) {
		self$tune_res = rbind(self$tune_res, as.data.frame(res))
	},
		
	write = function(result_file, suffix = "") {
		if (suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		write.csv(self$tune_res, paste(result_file, "_tune.csv", sep=""), row.names=TRUE)
	})
)
