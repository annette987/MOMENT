MMOmicsConst = new.env(parent = emptyenv())

#
# Experiment types
#
assign("TASK_CLASSIF", "CLASSIF", MMOmicsConst)

EXP_CLASSFN  = "CLASSFN"
EXP_FUSION	 = "FUSION"
EXP_COMBN	 	 = "COMBN"
EXP_SURVIVAL = "SURV"
EXP_ENSEMBLE = "ENS"
EXP_TEMPORAL = "TPM"

#
# Task types
#
TASK_CLASSIF = "CLASSIF"
TASK_SURV		 = "SURV"	

#
# Classes for multiclass classification
#
MC_LEVELS  = c("CIR", "CON", "LN", "LX")
MC_CLASSES = factor(MC_LEVELS)
MC_MEASURES = factor(c("Precision", "Recall", "F1-score", "Accuracy"))
MC_PLOTS = list("sens" = "Sensitivity", "spec" = "Specificity", "F1" = "F1 Score", "acc" = "Accuracy")
MC_PERF = list("acc" = "Accuracy", "auc" = "AUC")

#
#	Constant values for  benchmarking tests
#
NUM_INPUTS	 = 3
NUM_MEASURES = 4
NUM_CLASSES  = 4
NUM_ITERS    = 5
NUM_FOLDS    = 5
NUM_ITERS_TUNE 	 = 5
NUM_FOLDS_TUNE 	 = 5	
NUM_ITERS_FILTER = 10
NUM_ITERS_STAB 	 = 100
NUM_BOOTSTRAP_SAMPLES = 50
FREQUENCY_THRESH = 0.5
NUM_DELGN_RUNS = 2


NUM_AGGREGATORS = 5
NUM_THRESHOLDS  = 7
NUM_ALGORITHMS  = 10
NUM_FEATSEL		  = 9
NUM_WRAPPERS	  = 2

MAX_FEATURES	 = 21
MAX_RESOLUTION	 = 20

NUM_WEIGHT_AGGREGATES = 2
IND_MEAN_WEIGHT = 1
IND_SUM_WEIGHT  = 2

NUM_RANK_AGGREGATES = 4
IND_MEAN_RANK       = 1
IND_SUM_RANK        = 2
IND_MIN_RANK    	= 3
IND_MAX_RANK    	= 4

NUM_STAB_METRICS	= 5
IND_STAB_JACCARD	= 1
IND_STAB_DICE			= 2
IND_STAB_KUNCHEVA	= 3
IND_STAB_LUST			= 4
IND_STAB_CONS			= 5

BOOSTER_TREE		= "gbtree"
BOOSTER_LINEAR		= "gblinear"

NO_OHE = list(c("CoxPH", "RF", "Ranger", "XGBTree"))
FS_RANKERS = list("univariate.model.score", "ranger_permutation", "randomForest_importance", "randomForestSRC_var.select", "mrmr", "XGBoost")
FS_SPARSE = list("cv.glmnet", "glmboost", "cv.CoxBoost")
ALGO_NAMES = list(c("CoxPH", "Lasso", "Ridge", "ElasticNet", "CoxBoost", "GLMBoost", "RF", "Ranger", "Univariate", "XGBTree", "XBGLinear"))
DATASET_NAMES = list(datsets=c("Activities", "Blood", "Cog Objective", "Cog Subjective", "Demographics", "Family History", "Nutrition", "Functional", "Genetic Risk", 
					 "Quality of Life", "Medical Exam", "Medical History", "Medications", "MRI", "Psychological", "Combination")) 

	
#
# Codes to enable/disable different learners
#
# Base learners - duplicates are for survival analysis
LRN_COX 	 			= 0x0001
LRN_RIDGE	 			= 0x0002
LRN_ELASTICNET	= 0x0004
LRN_LASSO	 			= 0x0008
LRN_COXBOOST 		= 0x0010
LRN_GBM					= 0x0020
LRN_GLMBOOST 		= 0x0020
LRN_RF		 			= 0x0040
LRN_RFSRC	 			= 0x0040
LRN_RANGER	 		= 0x0080
LRN_XGB_TREE 		= 0x0100
LRN_XGB_LINEAR	= 0x0200 
LRN_RPART 			= 0x0400
LRN_NB					= 0x0800
LRN_LAST				= 0x0800

# Wrappers
LRN_FS_SFS					= 0x010000
LRN_FS_SFFS					= 0x020000
LRN_LAST_WRAPPER 		= 0x020000

# Filters
LRN_FS_NONE					= 0x0010000
LRN_FS_BORUTA		   	= 0x0020000
LRN_FS_VSURF				= 0x0040000	
LRN_FS_RF_PERMUTE	 	= 0x0080000
LRN_FS_RF_VARIMP	 	= 0x0100000
LRN_FS_LEFSE			  = 0x0200000	
LRN_FS_SIS					= 0x0400000
LRN_FS_MRMR			   	= 0x0800000		
LRN_FS_UNIVARIATE	 	= 0x1000000
LRN_FS_RF_MINDEPTH 	= 0x2000000
LRN_FS_RF_VARHUNT	 	= 0x4000000
LRN_LAST_FS			   	= 0x400000

#Combinations
LRN_ALL_FS			  	= 0x10000000	#All feature selection methods on one algorithm
LRN_ALL_MODELS			= 0x20000000	#One feature selection method on all algorithms
LRN_ALL_ENS_FS			= 0x40000000	#All ensemble feature selection methods on one algorithm

#Combinations-Univariate
LRN_UNICOX    	= 0x004001
LRN_UNIRID	= 0x004002
LRN_UNILAS	= 0x004008
LRN_UNIELA	= 0x004004
LRN_UNICOXBOO	= 0x004010
LRN_UNIGLMBOO	= 0x004020
LRN_UNIRFS	= 0x004040
LRN_UNIRANG	= 0x004080
LRN_UNIXGBT	= 0x004100
LRN_UNIXBGL	= 0x004200

#Combinations-Boruta
LRN_BORCOX	= 0x100001
LRN_BORRID	= 0x100002
LRN_BORLAS	= 0x100008
LRN_BORELA	= 0x100004
LRN_BORCOXBOO	= 0x100010
LRN_BORGLMBOO	= 0x100020
LRN_BORRFS	= 0x100040
LRN_BORRANG	= 0x100080
LRN_BORXGBT	= 0x100100
LRN_BORXGBL	= 0x100200

# Ensembles
LRN_FS_ENS_START		= 0x0100000000
LRN_FS_ENS_MEAN			= 0x0100000000
LRN_FS_ENS_SUM			= 0x0200000000
LRN_FS_ENS_MIN			= 0x0400000000
LRN_FS_ENS_FREQ			= 0x0800000000
LRN_FS_ENS_WMA			= 0x1000000000
LRN_FS_ENS_END			= 0x1FFFFFFFFF

#Combinations
LRN_LEARNERS		= 0x0003FF	#All base algorithms except SSVM, no filters
LRN_FILTERS			= 0x0FF000	#All feature selection methods
LRN_MOST	 		= 0x0FF07F	#All but SSVM & XGBoost until that is working& Ranger - too slow
LRN_ALL		 		= 0x0FF3FF	#All but SSVM
LRN_XGBLIN_FILTERS 	= 0x100200	#All filters on XGB Linear only
#LRN_XGBTREE_FILTERS = 0x100100	#All filters on XGB Tree only. 
LRN_XGBTREE_FILTERS = 0x0DF100	#All filters except RFVH on XGB Tree only. 
LRN_XGBTREE_SFS		= 0x01100 # XGB Tree with SFS
LRN_XGBTREE_SFFS	= 0x02100 # XGB Tree with SFFS
LRN_XGBTREE_RANGER	= 0x040100 # XGB Tree with Ranger
LRN_XGBTREE_RFVH	= 0x020100 # XGB Tree with RF VAR HUNT
LRN_RFUNIV_FILTER		= 0x204000	#Univariate filter on all models
LRN_RFMD_FILTER		= 0x210000	#RF Min-depth filter on all models
LRN_RFVI_FILTER		= 0x208000	#RF Variable importance filter on all models
LRN_RANGER_FILTER   = 0x240000	#Ranger filter on all models
LRN_MRMR_FILTER   = 0x280000	#MRMR filter on all models
LRN_MIND_FILTERS    = 0x230000	#RFSRC filters on all models
LRN_REST_FILTERS	= 0x2F3000	#All filters except Univariate & RF_VARIMP, all models
LRN_FILTERS_ONLY	= 0x300000	#All filter selection methods only
LRN_SFS_COXBOOST	= 0x001010	#SFS on Coxboost only
LRN_ALL_RFVH		= 0x220000	#RF Variable hunting filter on all algorithms
LRN_COX_RFVH		= 0x020001	#RF Variable hunting filter on Cox only
LRN_RIDGE_RFVH		= 0x020002	#RF Variable hunting filter on Ridge only
LRN_ELASTICNET_RFVH		= 0x020004	#RF Variable hunting filter on ElasticNet only
LRN_LASSO_RFVH		= 0x020008	#RF Variable hunting filter on Lasso only
LRN_COXBOOST_RFVH		= 0x020010	#RF Variable hunting filter on CoxBoost only
LRN_GLMBOOST_RFVH		= 0x020020	#RF Variable hunting filter on GLMBoost only
LRN_RFSRC_RFVH		= 0x020040	#RF Variable hunting filter on RFSRC only
LRN_RANGER_RFVH		= 0x020080	#RF Variable hunting filter on Ranger only
LRN_XGBLINEAR_RFVH		= 0x020200	#RF Variable hunting filter on XGB Linear only
LRN_COX_RFMD		= 0x010001	#RF Minimal depth filter on Cox only
LRN_COX_MEAN		= 0x400001	#Ensemble mean filter with Cox
LRN_ALL_WRAPPERS	= 0x202000	#SFFS on all models
LRN_SSVM_FILTERS 	= 0x100C00	#All filters on SSVM only
LRN_ENS_FILTERS		= 0x600000	#All ensemble filters on all models
LRN_ENS_FREQ		= 0x08000001	#Ensemble frequency filter with Cox
LRN_ENS_MEAN		= 0x01000001	#Ensemble mean filter with Cox
LRN_ENS_MEAN_CB	= 0x01000010	#Ensemble mean filter with CoxBoost
LRN_SFS_COX			= 0x001001	#SFS on Cox only
LRN_COX_ALL			= 0x100001	# Cox with all filters
LRN_COX_RANGER	= 0x040001	# Cox with Ranger
LRN_COX_WRAP		= 0x003001	# Cox with wrappers
LRN_XGBTREE_RFVARHUNT = 0x520100 # RF VarHunt and XGBTree on all featsel methods
LRN_RFVH_ALLBUTXGBTREE	= 0x202FF # RF Varhunt of all learners except XGBTree
LRN_RFSRC_XGBTREE	= 0x008100	# RF Var imp on XGBtree
LRN_COXBOOST_SFS		= 0x01010 # CoxBOost with SFS
LRN_COXBOOST_SFFS	= 0x02010 # CoxBoost with SFFS
LRN_XGBLINEAR_SFS		= 0x01200 # XGB Linear with SFS
LRN_XGBLINEAR_SFFS	= 0x02200 # XGB Linear with SFFS
LRN_MRMR_LASSO	= 0x080008	# MRMR on Lasso
LRN_MRMR_ELASTICNET	= 0x080004	# MRMR on ElasticNet
LRN_MRMR_RIDGE	= 0x080004	# MRMR on Ridge
LRN_MRMR_COXPH	= 0x080001	# MRMR on CoxPH
LRN_GBM_BORUTA		   	= 0x0020020

