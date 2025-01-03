#-----------------------------------------------------------------------------------------------------------------------------
# BASE LEARNERS - METHODS WITH EMBEDDED FEATURE SELECTION - INHERENTLY SUITED TO HIGH_DIM DATA
#-----------------------------------------------------------------------------------------------------------------------------	

library(rpart)

rfsrc_params1 = makeParamSet(
#		makeIntegerParam("mtry", lower = round(psqrt/2), upper = psqrt*2),
		makeIntegerParam("nodesize", lower = 1, upper = 20)
#			makeIntegerParam("nodedepth", lower = 1, upper = 20)	
)
rfsrc_params2 = makeParamSet(
		makeDiscreteParam("mtry", values = seq(from = 10, to = 120, by = 10)),
		makeDiscreteParam("nodesize", values= seq(from = 2, to = 20, by = 2))
)
ranger_params = makeParamSet(
#	makeIntegerParam("mtry", lower = round(psqrt/2), upper = psqrt*2),
	makeIntegerParam("min.node.size", lower = 5, upper = 50)
)	
xgbtree_params <- makeParamSet(
	# The number of trees in the model (each one built sequentially)
	makeIntegerParam("nrounds", lower = 100, upper = 500),
	# number of splits in each tree
	makeIntegerParam("max_depth", lower = 1, upper = 10),
	# "shrinkage" - prevents overfitting
	makeNumericParam("eta", lower = .1, upper = .5)
#	# L2 regularization - prevents overfitting
#	makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
)
xgblinear_params = makeParamSet(
	makeNumericParam("lambda", lower = 0, upper = 50)
)
svm_params = makeParamSet(
	makeNumericParam("cost", lower = 0.1, upper = 2)
)

# Arguments have been tuned on each dataset and averaged
base_learners = list(
	"RANGER" = list("class" = "classif.ranger",
								"code" = LRN_RANGER,
								"name" = "Ranger",
								"tune_params" = ranger_params,
								"args" = list(splitrule = "gini", importance = "permutation", num.trees = 1000, min.node.size = 15)),
	"GBM" = list("class" = "classif.gbm",
								"code" = LRN_GBM,
								"name" = "GBM",
								"tune_params" = NULL,
#								"args" = list(distribution = "bernoulli", n.trees = 1000, n.minobsinnode = 1)),
								"args" = list(distribution = "bernoulli", n.trees = 1000, interaction.depth = 6, shrinkage = 0.01, n.minobsinnode = 5, keep.data = TRUE)),
#								"args" = list(distribution = "multinomial", n.trees = 1000, interaction.depth = 6, shrinkage = 0.01, n.minobsinnode = 5, keep.data = TRUE)),
	"XGBTREE" = list("class" = "classif.xgboost",
								"code" = LRN_XGB_TREE,
								"name" = "XGBTree",
								"tune_params" = xgbtree_params,
								"args" = list(booster = BOOSTER_TREE, objective = "multi:softprob", eval_metric = "mlogloss", num_class = 4, max_depth = 5, eta = 0.001, gamma = 3, subsample = 0.75)),
#	"SVM" = list("class" = "classif.svm",
#								"code" = LRN_SVM,
#								"name" = "SVM",
#								"tune_params" = svm_params,
#								"args" = list(kernel = "linear", cost = 1)),
	"NB" = list("class" = "classif.naiveBayes",
								"code" = LRN_NB,
								"name" = "NB",
								"tune_params" = NULL,
								"args" = list()),
	"LASSO" = list("class" = "classif.cvglmnet",
								 "code" = LRN_LASSO,
								 "name" = "Lasso",
								 "tune_params" = NULL,
								 "args" = list(alpha = 1, nfolds = 5)),
	"RIDGE" = list("class" = "classif.cvglmnet",
								 "code" = LRN_RIDGE,
								 "name" = "Ridge",
								 "tune_params" = NULL,
								 "args" = list(alpha = 0, nfolds = 5)),
	"ELASTICNET" = list("class" = "classif.cvglmnet",
								 "code" = LRN_ELASTICNET,
								 "name" = "ElasticNet",
								 "tune_params" = NULL,
								 "args" = list(alpha = 0.5, nfolds = 5)),
	"XGBLIN" = list("class" = "classif.xgboost",
								"code" = LRN_XGB_LINEAR,
								"name" = "XGBLinear",
								"tune_params" = xgblinear_params,
								"args" = list(booster = BOOSTER_LINEAR, alpha = 0, lambda = 19)),
	"RF" = list("class" = "classif.imputedRF",
								"code" = LRN_RF,
								"name" = "RF",
								"tune_params" = rfsrc_params1,
								"args" = list(ntree = 1000, importance = TRUE, nodesize = 4, na.fail = na.omit)),
	"GGBM" = list("class" = "classif.ggbm",
							"code" = LRN_GBM,
							"name" = "GBM",
							"tune_params" = NULL,
							"args" = list(distribution = "multinomial", n.trees = 1000, interaction.depth = 6, shrinkage = 0.01, n.minobsinnode = 5, keep.data = TRUE))
)


base_learners_surv = list(
		"COXPH" = list("class" = "surv.coxph",
									 "code" = LRN_COX,
									 "name" = "CoxPH",
									 "tune_params" = NULL,
									 "args" = NULL),
		"LASSO" = list("class" = "surv.cvglmnet",
									 "code" = LRN_LASSO,
									 "name" = "Lasso",
									 "tune_params" = NULL,
									 "args" = list(alpha = 1, nfolds = 5)),
		"RIDGE" = list("class" = "surv.cvglmnet",
									 "code" = LRN_RIDGE,
									 "name" = "Ridge",
									 "tune_params" = NULL,
									 "args" = list(alpha = 0, nfolds = 5)),
		"ELASTICNET" = list("class" = "surv.cvglmnet",
									 "code" = LRN_ELASTICNET,
									 "name" = "ElasticNet",
									 "tune_params" = NULL,
									 "args" = list(alpha = 0.5, nfolds = 5)),
		"COXBOOST" = list("class" = "surv.cv.CoxBoost",
									"code" = LRN_COXBOOST,
									"name" = "CoxBoost",
									"tune_params" = NULL,
									"args" = NULL),
		"GLMBOOST" = list("class" = "surv.glmboost",
									"code" = LRN_GLMBOOST,
									"name" = "GLMBoost",
									"tune_params" = NULL,
									"args" = NULL),
		"XGBTREE" = list("class" = "surv.xgboost",
									"code" = LRN_XGB_TREE,
									"name" = "XGBTree",
									"tune_params" = xgbtree_params,
									"args" = list(booster = BOOSTER_TREE, alpha = 0)),
		"XGBLIN" = list("class" = "surv.xgboost",
									"code" = LRN_XGB_LINEAR,
									"name" = "XGBLinear",
									"tune_params" = xgblinear_params,
									"args" = list(booster = BOOSTER_LINEAR, alpha = 0)),
		"RFSRC" = list("class" = "surv.randomForestSRC",
									"code" = LRN_RFSRC,
									"name" = "RFSRC",
									"tune_params" = rfsrc_params2,
									"args" = list(ntree = 1000, importance = TRUE)),
		"RANGER" = list("class" = "surv.ranger",
									"code" = LRN_RANGER,
									"name" = "Ranger",
									"tune_params" = ranger_params,
									"args" = list(splitrule = "maxstat", importance = "permutation", num.trees = 1000))
#		"SSVM1" = list("class" = "surv.survivalsvm",
#									"code" = LRN_SSVM_VB1,
#									"name" = "SSVM VB1",
#									"tune_params" = ssvm1_params,
#									"args" = NULL),
#		"SSVM2" = list("class" = "surv.survivalsvm",
#									"code" = LRN_SSVM_VB2,
#									"name" = "SSVM VB2",
#									"tune_params" = ssvm2_params,
#									"args" = NULL)
)
