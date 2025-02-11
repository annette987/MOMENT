#--------------------------------------------------------
# CLASS PREDICTIONS
# Stores the predictions in each iteration
#--------------------------------------------------------


# Prediction Summary object.
# Reads in the prediction files from each modality and 

PrednSummary = R6::R6Class("PrednSummary", list(
	df_list = list(),
	final_mat = NULL,
	summary_df = NULL,
	combined = NULL,
	
	initialize = function(folder, filenames) {
		for (name in filenames) {
			self$df_list[[name]] = read.csv(paste0(folder, name), header = TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
		}
	},
	
	combine = function() {
		self$combined = do.call(cbind, unname(self$df_list))
		self$combined = self$combined[ , order(names(self$combined))]
	},
	
	summarise = function() {
		# get the complete set of row and column names
		all_rows = unique(unlist(lapply(self$df_list, rownames)))
		all_cols = unique(unlist(lapply(self$df_list, colnames)))

		# initialize a final matrix to NA
		self$final_mat = matrix(NA, nrow = length(all_rows), ncol = length(all_cols))
		rownames(self$final_mat) = all_rows
		colnames(self$final_mat) = all_cols

		# go through each df in the list
		for(i in seq_along(self$df_list)) {
			num_predns = rowSums(!is.na(self$df_list[[i]][-which(names(self$df_list[[i]]) %in% c('id', 'truth'))]))
			num_correct = rowSums(!is.na(self$df_list[[i]][-which(names(self$df_list[[i]]) %in% c('id', 'truth'))]))
			# set any NAs in the selection to 0
			self$final_mat[rownames(self$df_list[[i]]), colnames(self$df_list[[i]])][is.na(self$final_mat[rownames(self$df_list[[i]]), colnames(self$df_list[[i]])])] = 0
			# add the data frame to the selection
			self$final_mat[rownames(self$df_list[[i]]), colnames(self$df_list[[i]])] = self$final_mat[rownames(self$df_list[[i]]), colnames(self$df_list[[i]])] + as.matrix(self$df_list[[i]])
		}
	},
		
	write = function(result_file, suffix = "", method = "") {
		result_file = ifelse(suffix == "", result_file, paste0(result_file, "_", suffix))
		write.csv(self$combined, paste(result_file, "_predns.csv", sep=""), row.names=FALSE, na="")
	})
)
