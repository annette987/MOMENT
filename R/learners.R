#--------------------------------------------------------------------------------
# CLASS Learners
# Allows the user to select and create a set of learners for the machine learning
#---------------------------------------------------------------------------------


Learners = R6::R6Class("Learners", 
	public = list(
		base_learners 			= list(),
		base_filters				= list(),
		
		initialize = function(learner_type) {
			rfsrc_params1 = ParamHelpers::makeParamSet(
			#		ParamHelpers::makeIntegerParam("mtry", lower = round(psqrt/2), upper = psqrt*2),
					ParamHelpers::makeIntegerParam("nodesize", lower = 1, upper = 20)
			#			ParamHelpers::makeIntegerParam("nodedepth", lower = 1, upper = 20)	
			)
			rfsrc_params2 = ParamHelpers::makeParamSet(
					ParamHelpers::makeDiscreteParam("mtry", values = seq(from = 10, to = 120, by = 10)),
					ParamHelpers::makeDiscreteParam("nodesize", values= seq(from = 2, to = 20, by = 2))
			)
			ranger_params = ParamHelpers::makeParamSet(
			#	ParamHelpers::makeIntegerParam("mtry", lower = round(psqrt/2), upper = psqrt*2),
				ParamHelpers::makeIntegerParam("min.node.size", lower = 5, upper = 50)
			)	
			xgbtree_params <- ParamHelpers::makeParamSet(
				# The number of trees in the model (each one built sequentially)
				ParamHelpers::makeIntegerParam("nrounds", lower = 100, upper = 500),
				# number of splits in each tree
				ParamHelpers::makeIntegerParam("max_depth", lower = 1, upper = 10),
				# "shrinkage" - prevents overfitting
				ParamHelpers::makeNumericParam("eta", lower = .1, upper = .5)
			#	# L2 regularization - prevents overfitting
			#	ParamHelpers::makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
			)
			xgblinear_params = ParamHelpers::makeParamSet(
				ParamHelpers::makeNumericParam("lambda", lower = 0, upper = 50)
			)
			svm_params = ParamHelpers::makeParamSet(
				ParamHelpers::makeNumericParam("cost", lower = 0.1, upper = 2)
			)

			if (learner_type == TASK_CLASSIF) {	# Classification models
					self$base_learners = list(
						"RANGER" = list("class" = "classif.ranger",
													"code" = LRN_RANGER,
													"name" = "RANGER",
													"tune_params" = ranger_params,
													"args" = list(splitrule = "gini", importance = "permutation", num.trees = 1000, min.node.size = 15)),
						"GBM" = list("class" = "classif.gbm",
													"code" = LRN_GBM,
													"name" = "GBM",
													"tune_params" = NULL,
													"args" = list(n.trees = 1000, interaction.depth = 6, shrinkage = 0.01, n.minobsinnode = 5, keep.data = TRUE)),
						"XGBTREE" = list("class" = "classif.xgboost",
													"code" = LRN_XGB_TREE,
													"name" = "XGBTREE",
													"tune_params" = xgbtree_params,
													"args" = list(booster = BOOSTER_TREE, objective = "multi:softprob", eval_metric = "mlogloss", num_class = 4, max_depth = 5, eta = 0.001, gamma = 3, subsample = 0.75)),
						"NB" = list("class" = "classif.naiveBayes",
													"code" = LRN_NB,
													"name" = "NB",
													"tune_params" = NULL,
													"args" = list()),
						"LASSO" = list("class" = "classif.cvglmnet",
													 "code" = LRN_LASSO,
													 "name" = "LASSO",
													 "tune_params" = NULL,
													 "args" = list(alpha = 1, nfolds = 5)),
						"RIDGE" = list("class" = "classif.cvglmnet",
													 "code" = LRN_RIDGE,
													 "name" = "RIDGE",
													 "tune_params" = NULL,
													 "args" = list(alpha = 0, nfolds = 5)),
						"ELASTICNET" = list("class" = "classif.cvglmnet",
													 "code" = LRN_ELASTICNET,
													 "name" = "ELASTICNET",
													 "tune_params" = NULL,
													 "args" = list(alpha = 0.5, nfolds = 5)),
						"XGBLIN" = list("class" = "classif.xgboost",
													"code" = LRN_XGB_LINEAR,
													"name" = "XGBLIN",
													"tune_params" = xgblinear_params,
													"args" = list(booster = BOOSTER_LINEAR, alpha = 0, lambda = 19)),
						"RF" = list("class" = "classif.randomForest",
													"code" = LRN_RF,
													"name" = "RF",
													"tune_params" = rfsrc_params1,
													"args" = list(ntree = 1000, importance = TRUE, nodesize = 4))
						)
				} else {  # Survival modela
						self$base_learners = list(
							"COXPH" = list("class" = "surv.coxph",
														 "code" = LRN_COX,
														 "name" = "COXPH",
														 "tune_params" = NULL,
														 "args" = NULL),
							"LASSO" = list("class" = "surv.cvglmnet",
														 "code" = LRN_LASSO,
														 "name" = "LASSO",
														 "tune_params" = NULL,
														 "args" = list(alpha = 1, nfolds = 5)),
							"RIDGE" = list("class" = "surv.cvglmnet",
														 "code" = LRN_RIDGE,
														 "name" = "RIDGE",
														 "tune_params" = NULL,
														 "args" = list(alpha = 0, nfolds = 5)),
							"ELASTICNET" = list("class" = "surv.cvglmnet",
														 "code" = LRN_ELASTICNET,
														 "name" = "ELASTICNET",
														 "tune_params" = NULL,
														 "args" = list(alpha = 0.5, nfolds = 5)),
							"COXBOOST" = list("class" = "surv.cv.CoxBoost",
														"code" = LRN_COXBOOST,
														"name" = "COXBOOST",
														"tune_params" = NULL,
														"args" = NULL),
							"GLMBOOST" = list("class" = "surv.glmboost",
														"code" = LRN_GLMBOOST,
														"name" = "GLMBOOST",
														"tune_params" = NULL,
														"args" = NULL),
							"XGBTREE" = list("class" = "surv.xgboost",
														"code" = LRN_XGB_TREE,
														"name" = "XGBTREE",
														"tune_params" = xgbtree_params,
														"args" = list(booster = BOOSTER_TREE, alpha = 0)),
							"XGBLIN" = list("class" = "surv.xgboost",
														"code" = LRN_XGB_LINEAR,
														"name" = "XGBLIN",
														"tune_params" = xgblinear_params,
														"args" = list(booster = BOOSTER_LINEAR, alpha = 0)),
							"RFSRC" = list("class" = "surv.randomForestSRC",
														"code" = LRN_RFSRC,
														"name" = "RFSRC",
														"tune_params" = rfsrc_params2,
														"args" = list(ntree = 1000, importance = TRUE)),
							"RANGER" = list("class" = "surv.ranger",
														"code" = LRN_RANGER,
														"name" = "RANGER",
														"tune_params" = ranger_params,
														"args" = list(splitrule = "maxstat", importance = "permutation", num.trees = 1000))
					#		"SSVM1" = list("class" = "surv.survivalsvm",
					#									"code" = LRN_SSVM_VB1,
					#									"name" = "SSVM1 VB1",
					#									"tune_params" = ssvm1_params,
					#									"args" = NULL),
					#		"SSVM2" = list("class" = "surv.survivalsvm",
					#									"code" = LRN_SSVM_VB2,
					#									"name" = "SSVM2 VB2",
					#									"tune_params" = ssvm2_params,
					#									"args" = NULL)
					)
			}
				
			cox.lrn = mlr::makeLearner(cl = "surv.coxph", id = "perf.cox", predict.type = "response")
			self$base_filters = list(
				"UNI" = list("method" = "univariate.model.score",
												"code" = LRN_FS_UNIVARIATE,
												"name" = "UNI",
												"args" = list(perf.learner = cox.lrn, fw.perc = 1.0)),
				"BORUTA" = list("method" = "boruta",
												 "code" = LRN_FS_BORUTA,
												 "name" = "BORUTA",
			#									 "args" = list(get_imp = Boruta::getImpLegacyRfZ, pValue = 0.01, maxRuns = 200, withTentative = FALSE, verbose = 1, mustKeep = NULL, fw.threshold = 0.001)),
												 "args" = list(get_imp = Boruta::getImpRfZ, pValue = 0.05, maxRuns = 100, withTentative = FALSE, verbose = 1, mustKeep = NULL, fw.threshold = 0.001)),
				"VSURF"  = list("method" = "vsurf",
													"code" = LRN_FS_VSURF,
													"name" = "VSURF",
													"args" = list(fw.threshold = 0.001)),
				"RFMD" 		 = list("method" = "randomForestSRC_var.select",
													"code" = LRN_FS_RF_MINDEPTH,
													"name" = "RFMD",
													"args" = list(ntree = 1000, fw.threshold = 0.001, conservative = "low")),
				"RANGER" = list("method" = "ranger_permutation",
													"code" = LRN_FS_RF_PERMUTE,
													"name" = "RANGER",
													"args" = list(num.trees = 1000, splitrule = "gini", fw.perc = 0.2)),
				"RFMD-MED" 		 = list("method" = "randomForestSRC_var.select",
													"code" = LRN_FS_RF_MINDEPTH,
													"name" = "RFMD-MED",
													"args" = list(ntree = 1000, fw.threshold = 0.001, conservative = "medium")),
				"RFMD-HIGH" 		 = list("method" = "randomForestSRC_var.select",
													"code" = LRN_FS_RF_MINDEPTH,
													"name" = "RFMD-HIGH",
													"args" = list(ntree = 1000, fw.threshold = 0.001, conservative = "high")),
				"RF" 		 = list("method" = "randomForest_importance",
													"code" = LRN_FS_RF_VARIMP,
													"name" = "RF",
													"args" = list(ntree = 1000, nsplit = 10, nodesize = 3, fw.perc = 0.2)),
				"LEFSE"  = list("method" = "lefse",
												 "code" = LRN_FS_LEFSE,
												 "name" = "LEFSE",
												 "args" = list(lda.threshold = 2))
			)
		},			

		#
		# Create the learners for each dataset and store in a list
		# Steps are added to the ML pipeline in reverse order
		#

		create_learners = function(config, env, learner_type, pred_type = "response", balance = FALSE, subset = NULL, model_name = NULL)
		{
			learners = list()
			getArgs <- function(...) return(list(...))
			evalstr <- function(ss) eval.parent(parse(text=sprintf("getArgs(%s)", ss)))
			
			for (i in 1:length(config$baseModels)) {
				if (is.na(config$baseModels[[i]]$params) || (length(config$baseModels[[i]]$params) == 0)) {
					pars = list()
				} else if (is.character(config$baseModels[[i]]$params)) {
#					pars = eval(parse(text=sprintf("getArgs(%s)", config$baseModels[[i]]$params)), envir = env)
					pars = evalstr(config$baseModels[[i]]$params)
				} else {
					pars = config$baseModels[[i]]$params
				}

				if (is.na(config$baseModels[[i]]$fsparams) || (length(config$baseModels[[i]]$fsparams) == 0)) {
					fspars = list()
				} else if (is.character(config$baseModels[[i]]$fsparams)) {
#					fspars = eval(parse(text=sprintf("getArgs(%s)", config$baseModels[[i]]$fsparams)), envir = env)
					fspars = evalstr(config$baseModels[[i]]$fsparams)
				} else {
					fspars = config$baseModels[[i]]$fsparams
				}
				
				#	Begin pipeline with basic learner
				baselrn = self$base_learners[[config$baseModels[[i]]$learner]]
				lrn = do.call(mlr::makeLearner, args = append(list("cl" = baselrn$class, "id" = baselrn$name, "predict.type" = pred_type, fix.factors.prediction = TRUE), pars))
				
				# Add feature selection to pipeline
				basefilt = self$base_filters[[config$baseModels[[i]]$featsel]]
				if (!is.null(basefilt)) {
					filter_args = list("learner" = lrn, "fw.method" = basefilt$method)
					if (!is.null(config$cacheDir)) {
						filter_args = c(filter_args, list("cache" = config$cacheDir))
					}
					lrn = do.call(makeFilterWrapper, args = c(filter_args, fspars))
				}
				
				#Add multi-class balancing to the pipeline if requested
				if (balance) {
					lrn = makePreprocWrapperBalanceMC(lrn, config$targetVar, "SMOTE")
				}
								
				#	Add normalisation to pipeline
				if (!is.null(config$baseModels[[i]]$norm)) {				
					lrn = cpoNormalise(config$baseModels[[i]]$norm) %>>% lrn		
				}
				
				#Add imputation to the pipeline - use cop as it adds the missings property
				if (!is.null(config$baseModels[[i]]$imputation)) {				
					lrn = cpoImputeData(config$baseModels[[i]]$imputation, NULL) %>>% lrn
				}
	
				learners[[i]] = lrn
			}
			
			if (!is.null(subset)) {
				learners = learners[subset]
				learners = learners[!is.na(learners)]
			}
			return(learners)
		},
		
		
		create_learner = function(targetVar, baselrn, featsel = NULL, learner_type = TASK_CLASSIF, pred_type = "response", balance = FALSE, norm = NULL, imp = NULL)
		{
			#NB - check for valid values of lrn_idx and fs_idx
			
			if (length(baselrn$args) == 0) {
				pars = list()
			} else if (is.character(baselrn$args)) {
				pars = eval(parse(text=sprintf("list(%s)", baselrn$args)))
			} else {
				pars = baselrn$args
			}

			if (!is.null(featsel)) {
				if (length(featsel$args) == 0) {
					fspars = list()
				} else if (is.character(featsel$args)) {
					fspars = eval(parse(text=sprintf("list(%s)", featsel$args)))
				} else {
					fspars = featsel$args
				}
			}
			
			lrn = do.call(mlr::makeLearner, args = append(list("cl" = baselrn$class, "id" = baselrn$name, "predict.type" = pred_type, fix.factors.prediction = TRUE), pars))
			
			# Add feature selection to pipeline
			if (!is.null(featsel)) {
#				filter_args = list("learner" = lrn, "fw.method" = featsel$method, "cache" = config$cacheDir)
				filter_args = list("learner" = lrn, "fw.method" = featsel$method)
				lrn = do.call(makeFilterWrapper, args = c(filter_args, fspars))
			}
			
			#Add multi-class balancing to the pipeline if requested
			if (balance) {
#				lrn = makePreprocWrapperBalanceMC(lrn, targetVar, "SMOTE")
			}
							
			#	Add normalisation to pipeline
			if (!is.null(norm)) {
				# Check that norm is a valid value
				lrn = cpoNormalise(norm) %>>% lrn
			}
			
			#Add imputation to the pipeline - use cop as it adds the missings property
			if (!is.null(imp)) {
				# Check that imp is a valid value
				lrn = cpoImputeData(imp, NULL) %>>% lrn
			}
	
			return(lrn)
		}
	)
)

