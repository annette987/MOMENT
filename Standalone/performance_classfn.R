#--------------------------------------------------------
# CLASS PERFORMANCE
# Stores overall performance metrics
#--------------------------------------------------------

library(BBmisc)
library(R6)
library(testthat)
library(mice)

# Performance object.
# Result of an experiment with the following members:
#		perf - the overall performance values 
#		aggr - aggregated performance values

Performance = R6Class("Performance", list(
	perf = list(),
	aggr = list(),
	measures = NULL,
	
	initialize = function(measures) {
		self$measures = measures
		for (m in measures) {
			self$perf[[m$id]] = list()
			self$aggr[[m$id]] = list()
		}
	},
	
	calculate = function(pred) {
		all_perf = mlr::performance(pred, self$measures)	# mlr function to get the performance values
		print(all_perf)
		for (m in self$measures) {
			self$perf[[m$id]] = append(self$perf[[m$id]], all_perf[[m$id]])
			self$aggr[[m$id]] = mean(unlist(self$perf[[m$id]]), na.rm = TRUE)
		}
	},
		
	save = function(res) {
		print(class(res))
		for (m in self$measures) {
			print(m)
			if (inherits(res, 'ResampleResult')) {
				print("ResampleResult")
				self$perf[[m$id]] = as.list(res$measures.test[[m$id]])
				self$aggr[[m$id]] = res$aggr[paste0(m, '.test.mean_narm')]
			} else if (inherits(res, "BenchmarkResult")) {
				print("BenchmarkResult")
				raw = getBMRPerformances(res, learner.ids = c(method))
				print(raw)
#				print(raw[[task_id]][[method]])
#				perf$perf[[method]] = raw[[task_id]][[method]]	
#				perf$perf = raw[[task_id]][[method]]	
				aggr <- getBMRAggrPerformances(res, learner.ids = c(method))
				print(aggr)
				self$aggr[[m$id]] = aggr[[1]][[1]]
			} else {
				print("Other")
				self$perf[[m$id]] = as.list(res[[m$id]])
			}
		}
	},
		
	write = function(result_file, suffix = "", method = "") {
#		print(self$perf)
#		print(self$aggr)
		out_perf = do.call(cbind, self$perf)
#		cm = colMeans(out_perf)
#		print(cm)
#		out_perf = rbind(out_perf, colMeans = colMeans(out_perf[, lapply(out_perf, is.numeric)]))
#		out_perf = rbind(out_perf, lapply(out_perf, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else ""))
#		out_perf = rbind(out_perf, cm)
		names(out_perf) = self$measures
		if (suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		write.csv(out_perf, paste(result_file, "_perf.csv", sep=""), row.names=TRUE)
	},
		
	#Produce boxplots from data in a Performance object
	plot = function(method, out_file, result_file = NA) {
		if (!is.na(result_file)) {
			data = read.csv(result_file, sep = ",", dec = '.', header = TRUE, stringsAsFactors=FALSE)
		} else {
			data = as.data.frame(cbind(unlist(self$perf_acc), unlist(self$perf_auc)))
			colnames(data) = self$measures
		}

		par(cex.main = 2.5)
		par(cex.lab = 2.5)
		par(cex.axis = 2.5)
		jpeg(paste0(out_file, "_plot.jpg"))
		boxplot(data, 
						col = rainbow(ncol(data)), 
						main = paste0(method, " - Performance"),
						xlab = "Measurement", 
						names = colnames(data), 
						ylab = "Value",
						ylim = c(0.0, 1.0) )
		dev.off()
	})
)
