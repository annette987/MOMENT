library(Boruta)
library(randomForest)



#------------------------------------------------------------------------------------------------------------------------------
# FEATURE SELECTION - FILTER METHODS
#------------------------------------------------------------------------------------------------------------------------------
cox.lrn = makeLearner(cl = "surv.coxph", id = "perf.cox", predict.type = "response")
base_filters = list(
	"UNI" = list("method" = "univariate.model.score",
									"code" = LRN_FS_UNIVARIATE,
									"prefix" = "Univariate",
									"args" = list(perf.learner = cox.lrn, fw.perc = 1.0)),
	"BORUTA" = list("method" = "boruta",
									 "code" = LRN_FS_BORUTA,
									 "prefix" = "Boruta",
#									 "args" = list(get_imp = getImpLegacyRfZ, pValue = 0.01, maxRuns = 200, withTentative = FALSE, verbose = 1, mustKeep = NULL, fw.threshold = 0.001)),
									 "args" = list(get_imp = getImpRfZ, pValue = 0.05, maxRuns = 100, withTentative = FALSE, verbose = 1, mustKeep = NULL, fw.threshold = 0.001)),
	"VSURF"  = list("method" = "vsurf",
										"code" = LRN_FS_VSURF,
										"prefix" = "vsurf",
										"args" = list(fw.threshold = 0.001)),
	"RFMD" 		 = list("method" = "randomForestSRC_var.select",
										"code" = LRN_FS_RF_MINDEPTH,
										"prefix" = "RF_MD",
										"args" = list(ntree = 1000, fw.threshold = 0.001, conservative = "low")),
	"RANGER" = list("method" = "ranger_permutation",
										"code" = LRN_FS_RF_PERMUTE,
										"prefix" = "Ranger",
										"args" = list(num.trees = 1000, splitrule = "gini", fw.perc = 0.2)),
	"RFMD-MED" 		 = list("method" = "randomForestSRC_var.select",
										"code" = LRN_FS_RF_MINDEPTH,
										"prefix" = "RF_MD",
										"args" = list(ntree = 1000, fw.threshold = 0.001, conservative = "medium")),
	"RFMD-HIGH" 		 = list("method" = "randomForestSRC_var.select",
										"code" = LRN_FS_RF_MINDEPTH,
										"prefix" = "RF_MD",
										"args" = list(ntree = 1000, fw.threshold = 0.001, conservative = "high")),
	"RF" 		 = list("method" = "randomForest_importance",
										"code" = LRN_FS_RF_VARIMP,
										"prefix" = "RF_VarImp",
										"args" = list(ntree = 1000, nsplit = 10, nodesize = 3, fw.perc = 0.2)),
	"LEFSE"  = list("method" = "lefse",
									 "code" = LRN_FS_LEFSE,
									 "prefix" = "LeFSe",
									 "args" = list(lda.threshold = 2))
#	"SIS" = list("method" = "sis",
#									 "code" = LRN_FS_SIS,
#									 "prefix" = "SIS",
#									 "args" = list(family="gaussian", penalty = "lasso", tune = "cv", nfolds = 2, varISIS = 'vanilla', standardize = FALSE))
)
