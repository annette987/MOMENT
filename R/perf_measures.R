#############################################################################
# CLASS PERFORMANCE_MEASURES
# Customised performance aggregation measures
# Modify existing measures to provide a valid result in the presence of NA
#############################################################################

PerformanceMeasures = R6::R6Class("PerformanceMeasures", 
	public = list(
		measures = NULL,
		
		initialize = function(task_type, predict_type) {
		
			test.mean_narm = mlr::makeAggregation(
				id = "test.mean_narm",
				name = "Test mean with NA removed",
				properties = "req.test",
				fun = function(task, perf.test, perf.train, measure, group, pred) {
					mean(perf.test, na.rm = TRUE)
				})

			test.sd_narm = mlr::makeAggregation(
				id = "test.sd_narm",
				name = "Test sd with NA removed",
				properties = "req.test",
				fun = function(task, perf.test, perf.train, measure, group, pred) {
					sd(perf.test, na.rm = TRUE)
				})

			testgroup.mean_narm = mlr::makeAggregation(
				id = "testgroup.mean",
				name = "Test group mean with NA removed",
				properties = "req.test",
				fun = function(task, perf.test, perf.train, measure, group, pred) {
					mean(BBmisc::vnapply(split(perf.test, group), mean), na.rm = TRUE)
				})

			testgroup.sd_narm = mlr::makeAggregation(
				id = "testgroup.sd",
				name = "Test group standard  with NA removed",
				properties = "req.test",
				fun = function(task, perf.test, perf.train, measure, group, pred) {
					sd(BBmisc::vnapply(split(perf.test, group), mean), na.rm = TRUE)
				})
				
			if (task_type == TASK_SURV) {
				cindex.na = mlr::setAggregation(cindex, test.mean_narm)
				cindex.sdna = mlr::setAggregation(cindex, test.sd_narm)
				cindex_grp.na = mlr::setAggregation(cindex, testgroup.mean_narm)
				cindex_grp.sdna = mlr::setAggregation(cindex, testgroup.sd_narm)
				cindex.uno.na = mlr::setAggregation(cindex.uno, test.mean_narm)
				cindex.uno.sdna = mlr::setAggregation(cindex.uno, test.sd_narm)
				self$measures = list(cindex.na, cindex.sdna, cindex_grp.na, cindex_grp.sdna, cindex.uno.na, cindex.uno.sdna)
#				self$measures = list(cindex.na, cindex.sdna, cindex.uno.na, cindex.uno.sdna)
			} else {
				acc.na = mlr::setAggregation(mlr::acc, test.mean_narm)
				acc.sdna = mlr::setAggregation(mlr::acc, test.sd_narm)
				multiclass.aunu.na = mlr::setAggregation(mlr::multiclass.aunu, test.mean_narm)
				multiclass.aunu.sdna = mlr::setAggregation(mlr::multiclass.aunu, test.sd_narm)
				if (predict_type == "prob") {
					self$measures = list(acc.na)
#					self$measures = list(acc.na, multiclass.aunu.na)   ## multiclass.aunu doesn't work for PBMV - why?
				} else {
					self$measures = list(acc.na)
				}
			}
		}
))		

