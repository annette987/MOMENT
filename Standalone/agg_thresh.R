#------------------------------------------------------------------------------------------------------------------------------
# Aggregators
#------------------------------------------------------------------------------------------------------------------------------
agg_mean = list("H-mean-rank")
agg_value = list("H-mean-value")
agg_rra = list("H-RRA")
agg_freq = list("H-freq")
agg_salience = list("H-salience")
agg_cons = list("H-consrank")
agg_union = list("H-union")
agg_intersect = list("H-intersection")
aggregators = list(agg_mean, agg_value, agg_freq, agg_rra)

aggregators_rra = list("E-RRA")
aggregators_wma = list("E-wma")
aggregators_mean = list("E-mean")
aggregators_freq = list("E-freq")
agg_het = list("E-mean", "E-wma", "E-freq", "E-RRA")
agg_het_sparse = list("E-freq", "E-union", "E-intersection")

						 
#------------------------------------------------------------------------------------------------------------------------------
# Thresholds
#------------------------------------------------------------------------------------------------------------------------------
#get_cutoffs = function(task) {
#	cutoffs = list(log2(getTaskNFeats(task))/getTaskNFeats(task), 0.1, 0.25, 0.33, 0.5)
#	return(cutoffs)
#}
cutoffs = list(0.05, 0.1, 0.25, 0.33, 0.5)
cutoffs_auto = list(1.0)
						 
#------------------------------------------------------------------------------------------------------------------------------
# Aggregator/Threshold Pairs
#------------------------------------------------------------------------------------------------------------------------------
agg_thresh = list(
	list("name" = "MR_fixed_log2n", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[1]]),
	list("name" = "MR_fixed_0.1", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[2]]),
	list("name" = "MR_fixed_0.2", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[3]]),
	list("name" = "MR_fixed_0.33", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[4]]),
	list("name" = "MR_fixed_0.5", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[5]]),
	list("name" = "MR_kde", "agg" = "H-mean-rank", "auto_thresh" = "kde", cutoff = cutoffs_auto[[1]]),
	list("name" = "MR_probe_best_agg", "agg" = "H-mean-rank", "auto_thresh" = "probe_best_agg", cutoff = cutoffs_auto[[1]]),
	list("name" = "MR_probe_mprobes", "agg" = "H-mean-rank", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
	list("name" = "MR_probe_mprobes_agg", "agg" = "H-mean-rank", "auto_thresh" = "probe_mprobes_agg_5", cutoff = cutoffs_auto[[1]]),
	list("name" = "MR_probe_mprobes_agg_10", "agg" = "H-mean-rank", "auto_thresh" = "probe_mprobes_agg_10", cutoff = cutoffs_auto[[1]]),	
	list("name" = "MR_quantile25", "agg" = "H-mean-rank", "auto_thresh" = "quantile25", cutoff = cutoffs_auto[[1]]),
	list("name" = "MR_quantile50", "agg" = "H-mean-rank", "auto_thresh" = "quantile50", cutoff = cutoffs_auto[[1]]),
	list("name" = "MR_quantile75", "agg" = "H-mean-rank", "auto_thresh" = "quantile75", cutoff = cutoffs_auto[[1]]),
	list("name" = "MR_none", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),

	list("name" = "MV_fixed_log2n", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs[[1]]),
	list("name" = "MV_fixed_0.1", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs[[2]]),
	list("name" = "MV_fixed_0.25", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs[[3]]),
	list("name" = "MV_fixed_0.33", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs[[4]]),
	list("name" = "MV_fixed_0.5", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs[[5]]),
	list("name" = "MV_kde", "agg" = "H-mean-value", "auto_thresh" = "kde", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_probe_best_agg", "agg" = "H-mean-value", "auto_thresh" = "probe_best_agg", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_probe_mprobes", "agg" = "H-mean-value", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_probe_mprobes_agg", "agg" = "H-mean-value", "auto_thresh" = "probe_mprobes_agg_5", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_probe_mprobes_agg_10", "agg" = "H-mean-value", "auto_thresh" = "probe_mprobes_agg_10", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_quantile25", "agg" = "H-mean-value", "auto_thresh" = "quantile25", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_quantile50", "agg" = "H-mean-value", "auto_thresh" = "quantile50", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_quantile75", "agg" = "H-mean-value", "auto_thresh" = "quantile75", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_none", "agg" = "H-mean-value", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),

	list("name" = "RRA", "agg" = "H-RRA", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),
	list("name" = "Threshold", "agg" = "H-threshold", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),	
	list("name" = "Medrank", "agg" = "H-medrank", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]]),
	list("name" = "MedThreshold", "agg" = "H-medthreshold", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]])
)

agg_thresh_fwer = list(
	list("name" = "MR_probe_mprobes", "agg" = "H-mean-rank", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
	list("name" = "MR_probe_mprobes_agg", "agg" = "H-mean-rank", "auto_thresh" = "probe_mprobes_agg", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_probe_mprobes", "agg" = "H-mean-value", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_probe_mprobes_agg", "agg" = "H-mean-value", "auto_thresh" = "probe_mprobes_agg", cutoff = cutoffs_auto[[1]]),
	list("name" = "Freq_probe_mprobes", "agg" = "H-freq", "auto_thresh" = "probe_mprobes", cutoff = cutoffs_auto[[1]]),
	list("name" = "Freq_probe_mprobes_agg", "agg" = "H-freq", "auto_thresh" = "probe_mprobes_agg", cutoff = cutoffs_auto[[1]])
)	
	
agg_thresh_freq = list(
	list("name" = "MR_freq", "agg" = "H-mean-rank", "auto_thresh" = "frequency", cutoff = cutoffs_auto[[1]]),
	list("name" = "MV_freq", "agg" = "H-mean-value", "auto_thresh" = "frequency", cutoff = cutoffs_auto[[1]]),
	list("name" = "RRA_freq", "agg" = "H-RRA", "auto_thresh" = "frequency", cutoff = cutoffs_auto[[1]])
)
	
agg_thresh_ind_sparse = list(
	list("name" = "ind", "agg" = "", "auto_thresh" = "none", cutoff = cutoffs_auto[[1]])
)
	
agg_thresh_ind_filter = list(
	list("name" = "ind_log2n", "agg" = "", "auto_thresh" = "none", cutoff = cutoffs[[1]]),
	list("name" = "ind_0.1", "agg" = "", "auto_thresh" = "none", cutoff = cutoffs[[2]]),
	list("name" = "ind_0.25", "agg" = "", "auto_thresh" = "none", cutoff = cutoffs[[3]]),
	list("name" = "ind_kde", "agg" = "", "auto_thresh" = "kde", cutoff = cutoffs_auto[[1]]),
	list("name" = "ind_0.33", "agg" = "", "auto_thresh" = "none", cutoff = cutoffs[[4]]),
	list("name" = "ind_0.5", "agg" = "", "auto_thresh" = "none", cutoff = cutoffs[[5]])
)
		
agg_thresh_het = list(
	list("name" = "het", "agg" = "H-mean-rank", "auto_thresh" = "none", cutoff = cutoffs[[2]])
)
