library(tryCatchLog)

print(Sys.info()['nodename'])
if (substring(Sys.info()['nodename'], 1, 1) == 'k') { # Katana
	data_dir = "/srv/scratch/z8039617"
	code_dir = "./"
} else if (substring(Sys.info()['nodename'], 1, 1) == 'g')  { #Gadi
	data_dir = "/scratch/zq94/as0786"
	code_dir = "./"
} else {
	data_dir = "C:/Users/Annette/Documents/Liver Cancer Project/Experiments/R Code/"
	code_dir = data_dir
}
#cache_dir = paste0(data_dir, "/mlr/.cache")
cache_dir = paste0(data_dir, "/mlr/.cache2")

source(paste(code_dir, "classfn_tests.R", sep=""))
source(paste(code_dir, "combn_tests.R", sep=""))
source(paste(code_dir, "incremental_tests.R", sep=""))
source(paste(code_dir, "fusion_tests.R", sep=""))
source(paste(code_dir, "pb_mvboost_tests.R", sep=""))
source(paste(code_dir, "sl_filters.R", sep=""))
source(paste(code_dir, "mm_ada_tests.R", sep=""))
source(paste(code_dir, "data_exp_tests.R", sep=""))
#source(paste(code_dir, "int_nmf_tests.R", sep=""))
source(paste(code_dir, "super_tests.R", sep=""))
source(paste(code_dir, "method_mcauc.R", sep=""))
source(paste(code_dir, "mix_of_experts.R", sep=""))
source(paste(code_dir, "survival_tests.R", sep=""))

#-------------------------------------------------------------------
# Get Environment Variables
#-------------------------------------------------------------------
res_index = 900
decision = "prob"
direction = "REV"
metric = "F1"
config_file = ''
exp = "COMBN"
ovr_class = NULL
excl = TRUE
imp = TRUE
big = FALSE
inc = FALSE
combns = 0
subset = NULL
concat = FALSE
balance = FALSE
filter_zeroes = 90
filter_missings = 50
filter_corr = FALSE
filter_var = FALSE
validate = FALSE


env_vars = Sys.getenv()
for (name in names(env_vars)) {
	switch(name, 
				 res_index = {res_index = env_vars[[name]]}, 
				 decision = {decision = env_vars[[name]]},
				 direction = {direction = env_vars[[name]]},
				 metric = {metric = env_vars[[name]]},
				 config_file = {config_file = env_vars[[name]]},
				 subset = {subset = env_vars[[name]]},
				 exp = {exp = env_vars[[name]]},
				 ovr_class = {ovr_class = env_vars[[name]]},				 
				 excl = {excl = as.logical(env_vars[[name]])},
				 imp = {imp = as.logical(env_vars[[name]])},
				 big = {big = as.logical(env_vars[[name]])},
				 filter_zeroes = {filter_zeroes = as.numeric(env_vars[[name]])}, 
				 filter_missings = {filter_missings = as.numeric(env_vars[[name]])}, 
				 filter_corr = {filter_corr = as.logical(env_vars[[name]])},
				 filter_var = {filter_var = as.logical(env_vars[[name]])},
				 validate = {validate = as.logical(env_vars[[name]])},
				 inc = {inc = as.logical(env_vars[[name]])},
				 concat = {concat = as.logical(env_vars[[name]])},
				 balance = {balance = as.logical(env_vars[[name]])},
				 combns = {combns = as.numeric(env_vars[[name]])}
				)
}

print(paste0("res_index = ", res_index))
print(paste0("decision = ", decision))
print(paste0("direction = ", direction))
print(paste0("metric = ", metric))
print(paste0("config_file = ", config_file))
print(paste0("exp = ", exp))
print(paste0("ovr_class = ", ovr_class))
print(paste0("excl = ", excl))
print(paste0("imp = ", imp))
print(paste0("big = ", big))
print(paste0("inc = ", inc))
print(paste0("concat = ", concat))
print(paste0("balance = ", balance))
print(paste0("combns = ", combns))
print(paste0("subset = ", subset))
print(paste0("filter_zeroes = ", filter_zeroes))
print(paste0("filter_missings = ", filter_missings))
print(paste0("filter_corr = ", filter_corr))
print(paste0("filter_var = ", filter_var))
print(paste0("validate = ", validate))

#
#-------------------------------------------------------------------
# Get Configuration File
#-------------------------------------------------------------------
#if (length(config_file) == 0) {
if (config_file == '') {
	args = commandArgs(TRUE)
	config_file <- args[1]
	print(paste0("config_file = ", config_file))
}
cf = config(config_file)
cat("Config file: ", config_file, "\n")
setwd(data_dir)

#
#-------------------------------------------------------------------
# Get Combinations
#-------------------------------------------------------------------
comb_tbl = NULL
if (combns > 0) {
	v1 = 1:length(cf$baseModels)
	if (length(cf$baseModels) == 9) {
		comb_tbl = combn(v1, combns)
		# Prune if combns = 7 or less
		if (combns <= 7) {
			csums <- colSums((comb_tbl == 6) + (comb_tbl == 7))
			comb_tbl = comb_tbl[, csums < 2]
			csums <- colSums((comb_tbl == 8) + (comb_tbl == 9))
			comb_tbl = comb_tbl[, csums < 2]
		}
	} else if (length(cf$baseModels) == 4) {
		comb_tbl = combn(v1, combns)
	}
	comb_tbl = as.list(as.data.frame(comb_tbl))
	
#	combn_list = do.call(c, lapply(2:length(v1), function(x) combn(v1, x, FUN = list)))
#	pbs_index = as.integer(Sys.getenv("PBS_ARRAY_INDEX"))
#	if (!is.na(pbs_index ) && pbs_index <= length(cf$baseModels)) {
#		combn_set = combn_list[[pbs_index]]
#	}
	print("Combn set: ")
	print(comb_tbl)
}

#
#-------------------------------------------------------------------
# Get Subset of modilities to work on
#-------------------------------------------------------------------
if (!is.null(subset)) {
	subset = as.numeric(unlist(strsplit(subset, ",")))
}

#-------------------------------------------------------------------
# Run Tests
#-------------------------------------------------------------------

if (inc) {
	incremental_tests(cf, res_index, cache_dir, metric, exp, decision, big, ovr_class, subset, balance, direction)
} else if (exp == "ADA") {
	ada_tests(cf, res_index, cache_dir, decision, big, ovr_class, subset, balance, filter_zeroes, filter_missings, filter_corr, filter_var, validate)
} else if (exp == "MOE") {
	expert_tests(cf, res_index, cache_dir, method = "COMBO", decision, big, subset, balance, filter_zeroes, filter_missings, filter_corr, filter_var, validate)
} else if (exp == "MOE_ADA") {
	expert_tests(cf, res_index, cache_dir, method = "ADA", decision, big, subset, balance, filter_zeroes, filter_missings, filter_corr, filter_var, validate)
} else if (exp == "META") {
	fusion_tests(cf, res_index, cache_dir, big, ovr_class, comb_tbl, subset, balance, filter_zeroes, filter_missings, filter_corr, filter_var, validate)
} else if (exp == "COMBN") {
	combn_tests(cf, res_index, cache_dir, decision, big, ovr_class, subset, balance, filter_zeroes, filter_missings, filter_corr, filter_var, validate)
} else if (exp == "PBMV") {
	pb_mvboost_tests(cf, res_index, cache_dir, big, ovr_class, subset, balance, filter_zeroes, filter_missings, filter_corr, filter_var)
} else if (exp == "EXPLORE") {
	data_exp_tests(cf, res_index, big, ovr_class, concat)
} else if (exp == "CLASSFN") {
	classfn_tests(cf, res_index, cache_dir, big, ovr_class, subset, concat = concat, balance, filter_zeroes, filter_missings, filter_corr, filter_var)
} else if (exp == "SUPER") {
	res = super_tests(cf, res_index, cache_dir, big, ovr_class, subset, balance)
	outfile = paste0("super_", res_index, "_")
	write.csv(res$coeffs, paste0(outfile, "coeffs.csv"), row.names=FALSE)
	write.csv(res$cvrisk, paste0(outfile, "cvrisk_raw.csv"), row.names=FALSE)
	cvrisk_res = aggregate(res$cvrisk[, -1], list(res$cvrisk[,1]), mean)
	write.csv(cvrisk_res, paste0(outfile, "cvrisk.csv"), row.names=FALSE)
	write.csv(res$auc, paste0(outfile, "auc.csv"), row.names=FALSE)
	write.csv(res$slpred, paste0(outfile, "sl_predictions.csv"), row.names=FALSE)
	write.csv(res$libpred, paste0(outfile, "lib_predictions.csv"), row.names=FALSE)
} else if (exp == "SURV") {
	survival_tests(cf, res_index, cache_dir, big, ovr_class, subset, concat = concat, balance, filter_zeroes, filter_missings, filter_corr, filter_var)
}else {
}
