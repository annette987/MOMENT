#' R6 class to hold performance results
#'
#' @description
#' Stores and gives access to the performance results for each metric for each modality.
#'
#' @name Features
#' @docType package
NULL

Performance = R6::R6Class("Performance", list(
	#' @field perf (list of lists)\cr
	#' The top level list contains one list per performance metric requested.
	#' The second level list contains one value per modality ???
	perf = list(),
	
	#' @field aggr (list of lists)\cr
	#' The top level list contains one list per performance metric requested.
	#' The second level list contains teh aggregated performance for each modality.
	aggr = list(),
	
	#' @field measures (list)\cr
	#' The requested mlr performance metrics.
	measures = NULL,
	
	#' @description 
	#' Create a new `Performance` object.
	#' @param measures (list)\cr
	#' 	The names of the requested mlr performance metrics.
	#' @return A new `Performance` object
	#' @examples
	#' 	perf = Performance$new(list(acc))
	#' @export
	initialize = function(measures) {
		self$measures = measures
		for (m in measures) {
			self$perf[[m$id]] = list()
			self$aggr[[m$id]] = list()
		}
	},
	
	#' @description 
	#' Calculate the performance from an mlr Prediction object and save it.
	#' @param pred (Prediction)\cr
	#' 	mlr Prediction object
	#' @return Nothing
	#' @examples
	#' 	Performance$calculate(pred)
	#' @export
	calculate = function(pred, task = NULL, model = NULL) {
		all_perf = mlr::performance(pred, self$measures, task = task, model = model)
		for (m in self$measures) {
			self$perf[[m$id]] = append(self$perf[[m$id]], all_perf[[m$id]])
			self$aggr[[m$id]] = mean(unlist(self$perf[[m$id]]), na.rm = TRUE)
		}
	},
				
	#' @description 
	#' Save the performance results internally
	#' @param res (ResampleResult or ???)\cr
	#' 	An mlr structure containing the performance results.
	#' @return Nothing
	#' @examples
	#' 	imp = Features$save(res)
	#' @export	
	save = function(res) {
		for (m in self$measures) {
			if (inherits(res, 'ResampleResult')) {
				self$perf[[m$id]] = as.list(res$measures.test[[m$id]])
				self$aggr[[m$id]] = res$aggr[paste0(m, '.test.mean_narm')]
			} else {
				self$perf[[m$id]] = as.list(res[[m$id]])
			}
		}
	},
		
	#' @description 
	#' Write the performance results to disk.
	#' @param result_file (character)\cr
	#' 	The prefix of the name of the files to which the results will be written.
	#' @param suffix (character)\cr
	#' 	The suffix will be appended to the prefix of the output file name.
	#' @param method (character)\cr
	#' 	Method used ???
	#' @return Nothing
	#' @examples
	#' 	imp = Features$write("results", "241105")
	#' @export	
	write = function(result_file, suffix = "") {
		out_perf = do.call(cbind, self$perf)
		names(out_perf) = self$measures
		if (!is.null(suffix) && suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		write.csv(out_perf, paste(result_file, "_perf.csv", sep=""), row.names=TRUE)
	},
		
		
	#' @description 
	#' Plot the performance results as boxplots and return the JPEG plot object.
	#' @param method (character)\cr
	#' 	Method used ???
	#' @param result_file (character)\cr
	#' 	The name of the file containing the results to be plotted (optional).
	#'  If this value is NA (the default), the results saved internally are plotted.
	#' @return The plot object
	#' @examples
	#' 	imp = Features$plot("results", "") ???
	#' @export	
	#Produce boxplots from data in a Performance object
	plot = function(method, result_file = NA) {
		if (!is.na(result_file)) {
			data = read.csv(result_file, sep = ",", dec = '.', header = TRUE, stringsAsFactors=FALSE)
		} else {
			data = as.data.frame(cbind(unlist(self$perf_acc), unlist(self$perf_auc)))
			colnames(data) = self$measures
		}

		par(cex.main = 2.5)
		par(cex.lab = 2.5)
		par(cex.axis = 2.5)
#		jpeg(paste0(out_file, "_plot.jpg"))
		plt = boxplot(data, 
						col = rainbow(ncol(data)), 
						main = paste0(method, " - Performance"),
						xlab = "Measurement", 
						names = colnames(data), 
						ylab = "Value",
						ylim = c(0.0, 1.0) )
#		dev.off()
		return(plt)
	})
)
