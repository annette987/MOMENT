#############################################################################
# PERFORMANCE MEASURES
# Customised performance aggregation measures
# Modify existing measures to provide a valid result in the presence of NA
#############################################################################

library(mlr)

test.mean_narm = makeAggregation(
	id = "test.mean_narm",
	name = "Test mean with NA removed",
	properties = "req.test",
	fun = function(task, perf.test, perf.train, measure, group, pred) {
		mean(perf.test, na.rm = TRUE)
	})

test.sd_narm = makeAggregation(
	id = "test.sd_narm",
	name = "Test sd with NA removed",
	properties = "req.test",
	fun = function(task, perf.test, perf.train, measure, group, pred) {
		sd(perf.test, na.rm = TRUE)
	})

testgroup.mean_narm = makeAggregation(
	id = "testgroup.mean",
	name = "Test group mean with NA removed",
	properties = "req.test",
	fun = function(task, perf.test, perf.train, measure, group, pred) {
		mean(vnapply(split(perf.test, group), mean), na.rm = TRUE)
	})

testgroup.sd_narm = makeAggregation(
	id = "testgroup.sd",
	name = "Test group standard  with NA removed",
	properties = "req.test",
	fun = function(task, perf.test, perf.train, measure, group, pred) {
		sd(BBmisc::vnapply(split(perf.test, group), mean), na.rm = TRUE)
	})

cindex.na = setAggregation(cindex, test.mean_narm)
cindex.sdna = setAggregation(cindex, test.sd_narm)
cindex_grp.na = setAggregation(cindex, testgroup.mean_narm)
cindex_grp.sdna = setAggregation(cindex, testgroup.sd_narm)
cindex.uno.na = setAggregation(cindex.uno, test.mean_narm)
cindex.uno.sdna = setAggregation(cindex.uno, test.sd_narm)

acc.na = setAggregation(acc, test.mean_narm)
acc.sdna = setAggregation(acc, test.sd_narm)
multiclass.aunu.na = setAggregation(multiclass.aunu, test.mean_narm)
multiclass.aunu.sdna = setAggregation(multiclass.aunu, test.sd_narm)

surv.measures = list(cindex.na, cindex.sdna, cindex_grp.na, cindex_grp.sdna, cindex.uno.na, cindex.uno.sdna)
prob.measures = list(acc, multiclass.aunu)
classfn.measures = list(acc.na)
