#' R6 class to hold Receiver Operating Curve (ROC) measures for one class
#'
#' @description
#' Stores ROC measures for a single class
#'
#' @name ROC
#' @docType package
NULL

#' @noRd
symdiff <- function( x, y) { setdiff( union(x, y), intersect(x, y))}

ROC = R6::R6Class("ROC", list(
	#' @field roc_vals (list)\cr
	#' A list of the stored ROC metrics.
		roc_vals = list(),
		
	#' @description 
	#' Create a new `ROC` object.
	#' @param label (character) Class name
	#' @param sens (float) Sensitivity
	#' @param spec (float) Specificity
	#' @param F1 (float) F1 score
	#' @param acc (float) Accuracy
	#' @return A new `ROC` object containing the values passed in.
	#' @examples
	#' 	perf = ROC$new("C1", 0.3, 0.7, 0.65, 0.75)
	#' @noRd
		initialize = function(label, sens, spec, f1, acc) {
			self$roc_vals[[paste0(label,'-','sens')]] = sens
			self$roc_vals[[paste0(label,'-','spec')]] = spec
			self$roc_vals[[paste0(label,'-','F1')]] 	= f1
			self$roc_vals[[paste0(label,'-','acc')]] 	= acc
		}
	)
)


#' R6 class to hold multi-class Receiver Operating Curve (ROC) measures
#'
#' @description
#' Calculates and stores ROC measures for a multiclass classification problem.
#'
#' @name ROCMultiClass
#' @docType package
NULL

ROCMultiClass = R6Class("ROCMultiClass", list(
	roc_mc = NULL,
	auroc  = 0,
	
	#' @description 
	#' Create a new `ROCMultiClass` object.
	#' @return A new `ROCMultiClass` object.
	#' @examples
	#' 	perf = ROC$new("C1", 0.3, 0.7, 0.65, 0.75)
	#' @noRd
	initialize = function() {
	},

	save = function(roc_list, auc) {
		stopifnot(is.double(auc))
		self$roc_mc = rbind(self$roc_mc, as.data.frame(roc_list, "AUC" = auc))
	},
	
	aggregate = function() {
		stopifnot(!is.null(self$roc_mc))
		return(colMeans(self$roc_mc, na.rm = TRUE))
	},
	
	# Write a ROCMultiClass object out to a file
	write = function(result_file, suffix = "") {
		if (!is.null(suffix) && suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		write.table(self$roc_mc, paste0(result_file, "_roc.csv"), row.names = FALSE, col.names = TRUE, append = FALSE, sep = ",", quote = FALSE)
		write(self$auroc, file = paste0(result_file, "_auroc.txt"))
	},
	
	#Produce boxplots from data in a ROCMulticlass object
	plot = function(title, out_file, result_file = NA) {
		plots = list()
		if (!is.na(result_file)) {
			data = read.csv(result_file, sep = ",", dec = '.', header = TRUE, stringsAsFactors=FALSE)
			names = colnames(data)
			for (i in 1:length(names)) {
				names[[i]] = strsplit(names[[i]], ".", fixed = TRUE)[[1]][1]
			}
		} else {
			data = self$roc_mc
		}
		
		if (ncol(data) > 4) {
			for (plt in c("sens", "spec", "F1", "acc")) {
				df = data[, grepl(plt, colnames(data)), drop = FALSE]
				split_names = strsplit(colnames(df), "[.]")
				plot_names = sapply(split_names, tail, n = 2L)
				plot_names = data.matrix(plot_names)[1,]
				
				jpeg(paste0(out_file, "_", plt, "_plot.jpg"))
				par(cex.main = 2.5)
				par(cex.lab = 1.5)
				par(cex.axis = 1.5)
				plots[[plt]] = boxplot(df, 
															 col = rainbow(ncol(df)), 
															 main = paste0(title, " - ", MC_PLOTS[[plt]]), 
															 xlab = "", 
															 las = 2,
															 names = plot_names, 
															 ylab = MC_PLOTS[[plt]],
															 ylim = c(0.0, 1.0) )
				dev.off()
			}
		} else {
				jpeg(paste0(out_file, "_plot.jpg"))
				par(cex.main = 2.5)
				par(cex.lab = 2.5)
				par(cex.axis = 1.5)
				plots[["ROC"]] = boxplot(data, 
															 col = viridis::viridis(ncol(data)),
															 main = title, 
															 xlab = "", 
															 las = 2,
															 names = list("sens", "spec", "F1", "Acc"), 
															 ylim = c(0.0, 1.0) )
				dev.off()
		}
		return(plots)
	},
	
	#Get methods
	get_class_names = function(pred) {
			stopifnot(inherits(pred, "Prediction"))
			return(pred$task.desc$class.levels)
	},
		
	get_num_iters = function(pred) {
			stopifnot(inherits(pred, "Prediction"))
			return(pred$instance$desc$iters)
	},

	#
	# Convert to binary classification (if multiclass) and return the truth and response vectors
	#
	get_truth_response = function(pred, pos_class, iter = NA, binary_classfn = FALSE) {
			if (is.na(iter)) {
				dat = pred$data
			} else {
				dat = pred$data[pred$data['iter'] == iter, ]
			}
			if (!binary_classfn) {
				levels(dat$truth)[levels(dat$truth) != pos_class] = "REST"
				levels(dat$response)[levels(dat$response) != pos_class] = "REST"
			}
			return(list("truth" = dat$truth, "response" = dat$response))
	},
	
	#
	# Calculate the ROC values and return as a ROC object
	#
	getROCVals = function(method, pos_class, neg_class, tr) {
		diff = symdiff(levels(tr$truth), levels(tr$response))
		if (length(diff) > 0) {
			warning("Truth and response have different levels!")
			tp = tn = fp = fn = f1 = specr = sensr = precision = acc = 0
		} else {
			tp = measureTP(tr$truth, tr$response, as.character(pos_class))
			tn = measureTN(tr$truth, tr$response, neg_class)
			fp = measureFP(tr$truth, tr$response, as.character(pos_class))
			fn = measureFN(tr$truth, tr$response, neg_class)
			f1 = measureF1(tr$truth, tr$response, as.character(pos_class))
			specr = ifelse((tn + fp) == 0, 0, tn / (tn + fp))
			sensr = ifelse((tp + fn) == 0, 0, tp / (tp + fn))
			precision = ifelse((tp + fp) == 0, 0, tp / (tp + fp))
			acc = (tn + tp) / (tp + fp + tn + fn)
		}
		roc_data = ROC$new(paste(method, pos_class, sep="-"), sensr, specr, f1, acc)
		return(roc_data$roc_vals)
	},


	# Calculate ROC measures for an mlr Prediction object - one iteration only
	calculate = function(method, predn, class_names = NULL) {
		if (is.null(class_names)) {
			class_names = self$get_class_names(predn)
		}
		binary_classfn = (length(class_names) == 2)
		if (binary_classfn) {
#			neg_class = "REST"		# When do we need to do this? FIX ME
			neg_class = class_names[[2]]
			pos_class = ifelse(class_names[[1]] == neg_class, class_names[[2]], class_names[[1]])
		}
		
		if (inherits(predn, "ResamplePrediction")) {
			num_iters = self$get_num_iters(predn)
			for (i in 1:num_iters) {
				roc_measures = list()
				if (binary_classfn) {
					tr = self$get_truth_response(predn, pos_class, i, binary_classfn)
					roc_measures[[1]] = self$getROCVals(method, pos_class, neg_class, tr)
				} else {
					for (j in 1:length(class_names)) {
						tr = self$get_truth_response(predn, class_names[[j]], i, binary_classfn)
						roc_measures[[j]] = self$getROCVals(method, class_names[[j]], "REST", tr)
					}
				}
				self$save(roc_measures, 0)
			}
		} else if (inherits(predn, "Prediction")) {
			roc_measures = list()
			if (binary_classfn) {
					tr = self$get_truth_response(predn, pos_class, NA, binary_classfn)
					roc_measures[[1]] = self$getROCVals(method, pos_class, neg_class, tr)
			} else {
				for (j in 1:length(class_names)) {
					tr = self$get_truth_response(predn, class_names[[j]], NA, binary_classfn)
					roc_measures[[j]] = self$getROCVals(method, class_names[[j]], "REST", tr)
				}
			}
			self$save(roc_measures, 0)
			roc_measures = list()
			for (j in 1:length(class_names)) {
				dat = predn
				levels(dat$truth)[levels(dat$truth) != class_names[[j]]] = neg_class
				levels(dat$response)[levels(dat$response) != class_names[[j]]] = neg_class
				tr = list("truth" = dat$truth, "response" = dat$response)
				roc_measures[[j]] = self$getROCVals(method, class_names[[j]], neg_class, tr)
			}
			self$save(roc_measures, 0)
		}
	},
	
	calc = function(truth, response, class_names) {
		# Match factor levels
		diff = symdiff(levels(as.factor(truth)), levels(as.factor(response)))
		if (length(diff) > 0) {
			warning("Truth and response have different levels in calc")
			if (!("REST" %in% levels(truth))) {
				truth = factor(truth, levels = c(levels(truth), "REST"))
			} else {
				response = factor(response, levels = c(levels(truth), "REST")) 
			}
		}
		
		roc_measures = list()
		if (length(class_names) == 2) {
			class_names = levels(class_names[[1]])
			pos_class = class_names[1]
			neg_class = class_names[2]
			tr = list("truth" = truth, "response" = response)
			roc_measures[[1]] = self$getROCVals("combn", pos_class, neg_class, tr)
		} else {			
			for (j in 1:length(class_names)) {
				tmp_truth = as.factor(truth)
				tmp_response = as.factor(response)
				levels(tmp_truth)[levels(tmp_truth) != class_names[[j]]] = "REST"
				levels(tmp_response)[levels(tmp_response) != class_names[[j]]] = "REST"
				tr = list("truth" = tmp_truth, "response" = tmp_response)
				roc_measures[[j]] = self$getROCVals("combn", class_names[[j]], "REST", tr)
			}
		}
		self$save(roc_measures, 0)
	},
	
	calc_mc_roc = function(truth, response) {
		levels(truth) = make.names(levels(truth))
		levels(response) = make.names(levels(response))
	
		diff = symdiff(levels(truth), levels(response))
		if (length(diff) > 0) {
			warning("Truth and response have different levels in calculating multi-class ROC")
			if (length(levels(truth)) < length(levels(response))) {
				levels(truth) = levels(response)
			} else {
				levels(response) = levels(truth)
			}
		}
		
		l = list()
		for (i in 1:nlevels(response)) {
			col = as.data.frame(as.numeric(response))
			col[col != i] = 0
			l[levels(response)[i]] = col
		}
		resp = data.frame(l)
		auroc = pROC::multiclass.roc(response = truth, predictor = resp)
		self$auroc = auroc$auc
		return(self$auroc)
	})
)

	
#Get methods
get_class_names = function(pred) {
		stopifnot(inherits(pred, "Prediction"))
		return(pred$task.desc$class.levels)
}
