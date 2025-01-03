train_size = 78
test_size = 20

corrected_resampled_ttest = function(data1, data2, n_train, n_test) {
    n = length(data1)
    diffs = data2 - data1
    d_bar = mean(diffs, na.rm = TRUE)
    sigma2 = var(diffs, na.rm = TRUE)
    sigma2_mod = sigma2 * (1/n + n_test/n_train)
#    print(paste0(str(d_bar), ", ", str(sigma2_mod)))
    t_stat = d_bar / sqrt(sigma2_mod)
    pval = (1 - pt(abs(t_stat), n-1))
    return(pval)
} 

filename = file.choose()
print(filename)
dir_name = dirname(filename)
base = tools::file_path_sans_ext(basename(filename))
results_df = read.csv(filename, header = TRUE)
#results_df = results_df[, -1]
nmodels = ncol(results_df)
model_names = colnames(results_df)
pvals_wilcox = data.frame(matrix(0L, ncol = nmodels, nrow = nmodels), row.names = model_names)
colnames(pvals_wilcox) = model_names
pvals_ttest = data.frame(matrix(0L, ncol = nmodels, nrow = nmodels), row.names = model_names)
colnames(pvals_ttest) = model_names

for (i in 1:(ncol(results_df) - 1)) {
  for (j in (i + 1):ncol(results_df)) {
    wilres = wilcox.test(results_df[,i], results_df[,j], paired = TRUE)
    pvals_wilcox[i, j] = wilres$p.value
  }
}
write.csv(pvals_wilcox, file = paste(dir_name, paste0(base, "_wilcox.csv"), sep="/"))

pvals = unlist(pvals_wilcox, use.names = F)
pvals_adj = p.adjust(pvals, "fdr")
pvals_wilcox_adj = data.frame(matrix(pvals_adj, ncol = nmodels, nrow = nmodels), row.names = model_names)
colnames(pvals_wilcox_adj) = model_names
write.csv(pvals_wilcox_adj, file = paste(dir_name, paste0(base, "_wilcox_adj.csv"), sep="/"))

#for (i in 1:(ncol(results_df) - 1)) {
#  for (j in (i + 1):ncol(results_df)) {
#    pvals_ttest[i, j] = corrected_resampled_ttest(results_df[,i], results_df[,j], train_size, test_size)
#  }
#}
for (i in 1:ncol(results_df)) {
  for (j in 1:ncol(results_df)) {
    if (i != j) {
      pvals_ttest[i, j] = corrected_resampled_ttest(results_df[,i], results_df[,j], train_size, test_size)
    }
  }
}
write.csv(pvals_ttest, file = paste(dir_name, paste0(base, "_ttest.csv"), sep="/"))

for (i in 1:ncol(results_df)) {
  for (j in 1:ncol(results_df)) {
    #    print(paste(colnames(results_df)[i], " ", colnames(results_df)[j]))
    pvals_ttest[i, j] = corrected_resampled_ttest(results_df[,i], results_df[,j], train_size, test_size)
  }
}
write.csv(pvals_ttest, file = paste(dir_name, paste0(base, "_ttest_full.csv"), sep="/"))


for (i in 1:1) {
  for (j in 2:2) {
    print(paste(colnames(results_df)[i], " ", colnames(results_df)[j]))
    pvals_ttest[i, j] = corrected_resampled_ttest(results_df[,i], results_df[,j], train_size, test_size)
    pvals_ttest[j, i] = corrected_resampled_ttest(results_df[,j], results_df[,i], train_size, test_size)
    print(pvals_ttest[i, j])
    print(pvals_ttest[j, i])
  }
}

pvals = unlist(pvals_ttest, use.names = F)
pvals_adj = p.adjust(pvals, "fdr")
pvals_ttest_adj = data.frame(matrix(pvals_adj, ncol = nmodels, nrow = nmodels), row.names = model_names)
colnames(pvals_ttest_adj) = model_names
write.csv(pvals_ttest_adj, file = paste(dir_name, paste0(base, "_ttest_adj.csv"), sep="/"))

