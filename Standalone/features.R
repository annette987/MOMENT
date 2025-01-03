#--------------------------------------------------------
# CLASS FEATURES
# Stores feature selections
#--------------------------------------------------------

library(R6)
library(testthat)

# Features object.
# Result of an experiment with the following members:
#		featssel - the features selected in each run (list of character vectors)

Features = R6Class("Features", list(
	featsel = list(),
	featsel_aggr = list(),
	featnames = list(),
	class_names = NULL,
	
	initialize = function(tasks) {
			print("Initialising Features")
			self$class_names = c(getTaskClassLevels(tasks[[1]]), "all")
			stopifnot(is.character(self$class_names), length(self$class_names) >= 1)
			all_feats = unique(unlist(lapply(tasks, getTaskFeatureNames)))
			print(all_feats)
			for (cls in self$class_names) {
				self$featsel[[cls]]      = data.frame(row.names = all_feats)
				self$featsel_aggr[[cls]] = data.frame(row.names = all_feats)
				self$featnames[[cls]] 	 = list()
			}
	},
	
	save_class = function(method, class_name, names, scores, num_models) {
#		print(class_name)
	},

	
	save = function(method, names, scores, num_models) {
		for (class_name in self$class_names) {
			for (i in 1:num_models) {
				col_name = paste0(method, "-", i)
				self$featsel[[class_name]][, col_name] = 0
				if (i <= length(names[[class_name]])) {
					x = which(!is.na(names[[class_name]][[i]]))
					labels = names[[class_name]][[i]][x]
					if (is.null(scores) || length(scores[[class_name]]) == 0)
						self$featsel[[class_name]][labels, col_name] = 1
					else
						self$featsel[[class_name]][labels, col_name] = scores[[class_name]][[i]][x]
					self$featnames[[class_name]][[i]] = labels
				}
			}

			self$featsel_aggr[[class_name]][1:nrow(self$featsel_aggr[[class_name]]), class_name] = rowMeans(self$featsel[[class_name]], na.rm = TRUE)[1:nrow(self$featsel_aggr[[class_name]])]
		}
	},
	
	save_single = function(method, names, scores, model_num) {
		cls = 'all'
		col_name = paste0(method, "-", model_num)
		self$featsel[[cls]][, col_name] = 0
		if (model_num <= length(names)) {
			x = which(!is.na(names))
			labels = names[x]
			if (is.null(scores) || length(scores) == 0)
				self$featsel[[cls]][labels, col_name] = 1
			else
				self$featsel[[cls]][labels, col_name] = scores[x]
			self$featnames[[cls]][[model_num]] = labels
		}

#		x = rowMeans(self$featsel[[cls]], na.rm = TRUE)
		self$featsel_aggr[[cls]][1:nrow(self$featsel_aggr[[cls]]), cls] = rowMeans(self$featsel[[cls]], na.rm = TRUE)[1:nrow(self$featsel_aggr[[cls]])]
	},
		
	write = function(result_file, suffix = "") {
		if (suffix != "") {
			result_file = paste0(result_file, "_", suffix)
		}
		out_feats = as.data.frame(do.call(cbind, self$featsel))
		out_aggr  = as.data.frame(do.call(cbind, self$featsel_aggr))
		write.csv(out_feats, paste0(result_file, "_featsel.csv"), row.names=TRUE)
		write.csv(out_aggr, paste0(result_file, "_featsel_aggr.csv"), row.names=TRUE)
	})
)
		
# Featsel object.
# Result of an experiment with the following members:
#		feats - the features selected in each run (list of character vectors)

new_features = function(task) {
	makeS3Obj(
		"Features",
		featsel      = data.frame(row.names = getTaskFeatureNames(task)),	
		featsel_aggr = data.frame(row.names = getTaskFeatureNames(task)),
		featnames    = list()
	)
}

add_features = function(feats, new_feats) {
  feats$featsel = cbind(feats$featsel, new_feats$featsel)
  feats$featsel_aggr = cbind(feats$featsel_aggr, new_feats$featsel_aggr)
  return(feats)
}

save_features = function(feats, method, names, scores, num_models) {
	for (i in 1:num_models) {
		col_name = paste0(method, "-", i)
		feats$featsel[, col_name] = 0
		if (i <= length(names)) {
			x = which(!is.na(names[[i]]))
			labels = names[[i]][x]
			if (length(scores) == 0)
				feats$featsel[labels, col_name] = 1
			else
				feats$featsel[labels, col_name] = scores[[i]][x]
			feats$featnames[[i]] = labels
		}
	}
	x = rowMeans(feats$featsel, na.rm = TRUE)
	feats$featsel_aggr[1:nrow(feats$featsel_aggr), method] = rowMeans(feats$featsel, na.rm = TRUE)[1:nrow(feats$featsel_aggr)]
	return(feats)
}
	
write_features = function(feats, result_file, suffix = "") {
	feats$featsel[["Total"]] = apply(feats$featsel, 1, function(x) sum(x, na.rm=TRUE))
	write.csv(feats$featsel, paste0(result_file, "_featsel", "_", suffix, ".csv"), row.names=TRUE)
	write.csv(feats$featsel_aggr, paste0(result_file, "_featsel_aggr", "_", suffix, ".csv"), row.names=TRUE)
}

# scores is a named vector
write_raw_features = function(filename, suffix, scores) {
	ordered = scores[order(names(scores))]
	full_fname = paste(filename, "_", suffix, ".csv", sep="")
	incl_names = (file.size(full_fname) == 0)
	write.table(t(ordered), full_fname, sep=",", append=TRUE, row.names=FALSE, col.names=incl_names)
}

