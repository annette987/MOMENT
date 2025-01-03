#-------------------------------------------------------------
# AUTOMATIC THRESHOLDING
# Determine which features are most important, automatically
# All functions are passed a data.frame, with a "value" column
# and return the modified data.frame
#-------------------------------------------------------------

library(NbClust)
library(expm)
library(matrixStats)


###############################################################################
# threshold_auto: 
# Select the appropriate auto thresholding function to call

threshold_auto = function(dat, raw_dat, col_name, autothresh, param = NULL)
{
		print(paste0("threshold_auto: ", autothresh, ", ", col_name))
		if (autothresh == "cluster") {
				dat = threshold_cluster(dat, col_name)
		} else if (autothresh == "kde") {
				dat = threshold_kde(dat, col_name)
		} else if (autothresh == "mapdp") {
				dat = threshold_mapdp(dat, col_name)
		} else if (autothresh == "mapdp_counts") {
				dat = threshold_mapdp_counts(dat, col_name)
		} else if (autothresh == "top_half") {
				dat = threshold_topset(dat, raw_dat, col_name, 0.5)
		} else if (autothresh == "top_other") {
				dat = threshold_topset(dat, raw_dat, col_name, 0.2)
		} else if (autothresh == "quantile25") {
				dat = threshold_quantile(dat, col_name, '25%')
		} else if (autothresh == "quantile50") {
				dat = threshold_quantile(dat, col_name, '50%')
		} else if (autothresh == "quantile75") {
				dat = threshold_quantile(dat, col_name, '75%')
		} else if (autothresh == "outlier") {
				dat = threshold_quantile(dat, col_name, 'out')
		} else if (autothresh == "probe_best_agg") {
				dat = threshold_probe_best(dat, col_name)
		} else if (autothresh == "probe_best_raw") {
				dat = threshold_probe_raw_best(dat, raw_dat, col_name)
		} else if (autothresh == "probe_mprobes") {
				dat = threshold_mprobes(dat, raw_dat, col_name, 0.1)
		} else if (autothresh == "probe_mprobes_agg_20") {
				dat = threshold_mprobes_agg(dat, raw_dat, col_name, 0.2)
		} else if (autothresh == "probe_mprobes_agg_10") {
				dat = threshold_mprobes_agg(dat, raw_dat, col_name, 0.1)
		} else if (autothresh == "probe_mprobes_agg_5") {
				dat = threshold_mprobes_agg(dat, raw_dat, col_name, 0.05)
		} else if (autothresh == "probe_avg") {
				dat = threshold_probe_avg(dat, col_name)
		} else if (autothresh == "r2vim") {
				dat = threshold_r2vim(dat, col_name, param)
		} else if (autothresh == "salience") {
				dat = threshold_salience(dat, col_name)
		} else if (autothresh == "stddev") {
				dat = threshold_sd(dat, col_name)
		} else if (autothresh == "none") {
		} else {
			print("Unknown autothresh")
		}
		return(dat)
}


###############################################################################
# threshold_cluster: 
# Use a 1-dimensional k-means cluster to divide the data at points where 
# there are large differences between values.
# Data passed in must already be sorted - NO!
# NbCust is used to determine the optimal number of clusters

threshold_cluster = function(dat, col_name)
{
		dat = as.data.frame(dat)
		max_clust = min(10, nrow(na.omit(dat[,col_name])))
		if (max_clust < 10)
			print(na.omit(dat[,col_name]))

		clust = tryCatch({
			NbClust(data = na.omit(dat[,col_name]), distance = "euclidean", min.nc = 2, max.nc = max_clust, method = "kmeans", index = "cindex")
		}, 
		error = function(cond) {
				print(paste("NbClust returned error: ", cond))
				return(NULL)		
		}, 
		warning = function(cond) {
				print(paste("NbClust returned warning: ", cond))
				return(NULL)		
		})

		if (!is.null(clust)) {
			print(paste0("Number of clusters chosen: ", clust$Best.nc['Number_clusters']))
			if (clust$Best.nc['Number_clusters'] > 1) {
				km = kmeans(as.vector(na.omit(dat[,col_name])), clust$Best.nc['Number_clusters'], nstart = 25)	
				max.ind = km$cluster[1]
				discard = which(km$cluster != km$cluster[max.ind])
				print(paste0("Number of features selected is : ", length(which(km$cluster == km$cluster[max.ind]))))
				dat[discard, col_name] = NA	
			} else {
				print("Cluster - only one cluster assigned")
			}
		}
		return(dat)
}



###############################################################################
# threshold_kde: 
# Use kernel density estimation to divide the data 
# Overcomes the limitations of k-means - no need to choose the number of clusters in advance

threshold_kde = function(data, col_name)
{
		non_na = na.omit(as.data.frame(data))
		print(paste0("Number of features selected: ", nrow(non_na)))
		omitted = attr(non_na, 'na.action')
		
		dat = as.data.frame(non_na)
		adj = 1.0
		repeat {
			print(paste0("Adjust = ", adj))
			d = density(dat[,col_name], bw="nrd0", adjust = adj)
			kmin = which(diff(sign(diff(d$y)))== 2)+1		# indexes of local minima
			kmax = which(diff(sign(diff(d$y)))== -2)+1	# indexes of local maxima
			print(kmin)
#			print("Y values of local minima:")
#			print(d$y[kmin])
#			print("Y values of local maxima:")
#			print(d$y[kmax])
			if ((length(kmax) > 1 && length(kmin) > 0) || adj < 0.05)  # Need at least 2 maxima to get a minimum
				break
			else
				adj = adj / 2.0
		}
		
		if (adj >= 0.05) {
				m = which.max(d$y[kmax])
				print(paste0("Max of maxima: ", m))
				cuts = d$x[kmin]                           # points at which to divide data
				print("Cuts:")
				print(cuts)
		#		cutoff = cuts[which.max(cuts)]
		#		cutoff = cuts[1]												# Take the first cutoff, assuming this is to te right of the peak
				if (m <= length(cuts))
					cutoff = cuts[m]
				else
					cutoff = cuts[length(cuts)]
				print(paste0("Cutoff = ", cutoff))
				
				irr_vars = dat[dat[,col_name] < cutoff, col_name]
				print(paste0("Number of irrelevant vars = ", length(irr_vars)))
				if ((nrow(dat) - length(irr_vars)) > 1)
							dat[dat[,col_name] < cutoff, col_name] = NA	
				else {
					print("KDE - attempting to set all/all but one feature(s) to NA. Returning best 2.")
					dat[3:nrow(dat), col_name] = NA				
				}
		}
		else {
			print("KDE - no maxima found. Returning all.")	
		}

#		print(rbind(dat, data[omitted,]))
		return(rbind(dat, data[omitted,]))
}



###############################################################################
# threshold_mapdp: 
# Use map-dp clustering to divide the data at points where 
# there are large differences between values.
# Overcomes the limitations of k-means - no need to choose the number of clusters in advance

threshold_mapdp = function(dat, col_name)
{
		dat = as.data.frame(dat)
#		print(dat)
		X = data.matrix(na.omit(dat[,col_name]))
		X = t(X)		# Transpose to get into correct format
		
		# Set up Wishart MAP-DP prior parameters
		# For our data, which is 1D (1 row), m0 ad B0 must be 1x1 matrices
		N = dim(X)[2]
		N0 <- 1;                           	 # Prior count (concentration parameter)
		m0 <- rowMeans(X);                   # Normal-Wishart prior mean
		a0 <- 10;                            # Normal-Wishart prior scale
		c0 <- 10/N;                          # Normal-Wishart prior degrees of freedom
		B0 = matrix(0.005)								 	 # Normal-Wishart prior precision

		r = tryCatch({
			clustMapDP(X,N0,m0,a0,c0,B0);
		}, 
		error = function(cond) {
				print(paste("clustMapDP returned error: ", cond))
				return(NULL)	
		}
		)
		
#		print(r$z)
		print(paste0("Number of clusters chosen: ", r$K))
		for (i in 1:r$K) {
#			print(paste0("Cluster id: ", i))
			cluster_content = which(r$z == i)
#			print(unlist(dat[cluster_content, 1]))
		}
		
		first.ind = r$z[1]
		print(paste0("Using index ", first.ind))
#		discard = which(r$z != r$z[first.ind])
		discard = which(r$z != first.ind)
		print(paste0("Length of discard = ", length(discard)))
		print(paste0("Number of features selected is : ", length(which(r$z == r$z[first.ind]))))
		if (length(discard) == 0 || length(discard) == N) {
			print("Mapdp - only one cluster assigned")
		} else {
			dat[discard, col_name] = NA	
		}
		return(dat)
}


###############################################################################
# threshold_mapdp_counts: 
# Use map-dp clustering on binary data to divide the data at points where 
# there are large differences between values.
# Overcomes the limitataions of k-means - no need to choose the number of clusters in advance

threshold_mapdp_counts = function(dat, col_name)
{
		dat = as.data.frame(dat)
		X = data.matrix(na.omit(dat[,col_name]))
		X = t(X)		# Transpose to get into correct format
		
		# Set up Binomial MAP-DP prior parameters
		N0 = 2                     	# Prior count (concentration parameter)
		alpha0 = 15             	  # cluster beta-binomial prior parameter; bigger values lead to less but larger clusters of counts
		beta0 = 22                  # cluster beta-binomial prior parameter; look at the shape of the pdf of a beta distribution,
																				# if alpha and beta are larger than 1,this implies that the expected success probability will be random
																				# with more support towards values that generate more common counts 
		n = 55 											# The number of trials parameter, should be bigger than the maximum integer in your data

		r = tryCatch({
			clustMapDP_counts(X,N0,alpha0,beta0,n)
		}, 
		error = function(cond) {
				print(paste("clustMapDP returned error: ", cond))
				return(NULL)	
		}
		)
		
#		print(r$z)
		print(paste0("Number of clusters chosen: ", r$K))
		for (i in 1:r$K) {
#			print(paste0("Cluster id: ", i))
			cluster_content = which(r$z == i)
#			print(unlist(dat[cluster_content, 1]))
		}

		first.ind = r$z[1]
#		print(paste0("Using index ", first.ind))
#		discard = which(r$z != r$z[first.ind])		# I think this should just be first.ind!
		discard = which(r$z != first.ind)
		print(paste0("Length of discard = ", length(discard)))
		print(paste0("Number of features selected is : ", length(which(r$z == r$z[first.ind]))))

		if (length(discard) == 0 || length(discard) == n) {
			print("Mapdp - only one cluster assigned")
		} else {
			dat[discard, col_name] = NA	
		}
		return(dat)
}


##########################################################################################################
# threshold_quantile: 
# Only keep features that score above the given quantile
# Can only be used by sparse feature selectors ??
# Make sure not to return all NA's

threshold_quantile = function(dat, col_name, quant)
{
		dat = as.data.frame(dat)
		print(paste0("Number of features selected: ", nrow(na.omit(dat))))
		q = quantile(dat[dat[,col_name] != 0, col_name], na.rm = TRUE)
		print(q)
		if (quant == 'out') {
			thresh = q['75%'] + 1.5 * IQR(dat[dat[,col_name] != 0, col_name], na.rm = TRUE)
		} else {
			thresh = q[quant]
		}
		print(paste0('thresh = ', thresh))		

		if (!is.na(thresh)) {
			x = !is.na(dat[, col_name])
			dat[dat[x, col_name] < thresh, col_name] = NA
		}
		return(dat)
}


##########################################################################################################
# threshold_topset: 
# Only keep features that are selected frequently 
# Can only be used by sparse feature selectors
# Make sure not to return all NA's

threshold_topset = function(dat, raw_dat, col_name, thresh)
{
		num_feats = nrow(dat)
		frequencies = rep(0, num_feats)
		names(frequencies) = unlist(dat[,'name'])
		raw_dat = as.data.frame(raw_dat)

		# Lists are already sorted in order of highest rank/value first
		# Score 1 if a feature is ranked in top 'thresh' % 		
		first = 1
		last  = num_feats
		while (last <= nrow(raw_dat)) {
#			print(raw_dat[first:last, ])
			top = na.omit(raw_dat[first:last, ])
			topnames = unlist(top[, 'name'])
			frequencies[topnames] = frequencies[topnames] + 1
			
			first = first + num_feats
			last = last + num_feats
		}
		frequencies = sort(frequencies, decreasing = TRUE, na.last = TRUE)
#		print(frequencies)
		if (nrow(raw_dat) > nrow(dat)) {
			frequencies[frequencies < (NUM_BOOTSTRAP_SAMPLES * thresh)] = 0
		}
	  irr_var_idx = which(frequencies == 0)
		irr_var_names = names(frequencies[irr_var_idx])
#		print("Irrelevant variables:")
#		print(irr_var_names)
		dat[dat$name %in% irr_var_names, col_name] = NA
#		print(dat)		
		return(dat)
}


##########################################################################################################
# threshold_probe_best: 
# Only keep features whose aggregated score is "better" than the highest aggregated score of a random probe 
# N.B. "better" mostly means higher, but in the case of RF-MD it means lower.
# Random probes are identified by names beginning with 'shadow_'
# Make sure not to return all NA's

threshold_probe_best = function(dat, col_name)
{
		dat = as.data.frame(dat)
		print(paste0("Number of features selected: ", nrow(na.omit(dat))))
		ind = grep("^shadow_*", dat[,1], value=FALSE)
		probes = dat[ind, ]
		orig = dat[-ind,]
		
		best = max(probes[, col_name], na.rm = TRUE)
		if (is.infinite(best)) {
			print("All probes were NA")
		} else {
			print(paste0("Best probe score = ", best))
			print(paste0("Best probe name = ", probes[which.max(probes[, col_name]), 1]))
		}
		
		# If best = 0 then values are in the reverse order, with best smallest e.g for RF-MD
		reverse_best = FALSE
		if (best == 0) {
			print("Best probe is zero!")
			best = min(probes[, col_name], na.rm = TRUE)
			reverse_best = TRUE
		}
		print(paste0("Best probe score = ", best))
		
		# Best might be -Inf if all probes had a value of NA - i.e. were not selected by the FS
		# In that case, the relevant variables are only those that the FS did not score as NA
		orig = orig[order(orig[,col_name], decreasing = TRUE, na.last = TRUE),] 
		if (is.na(best) || is.infinite(best)) {
			irr_var_idx = which(is.na(orig[,col_name]))
		} else {
			if (reverse_best)
				x = orig[,col_name] > best * -1				
			else
				x = orig[,col_name] < best
#			print("Finding irrelevant ones:")
#			print(x)
			irr_var_idx = c(which(x), which(is.na(x)))			# Which ones are TRUE or NA
		}
#		print(irr_var_idx)

		print(paste0("Number of irrelevant variables = ", length(irr_var_idx)))
		if ((nrow(orig) - length(irr_var_idx)) > 1) {
				orig[irr_var_idx, col_name] = NA
		} else {
			print("Probe best - attempting to set all/all but one feature(s) to NA. Returning best 2.")
			orig[3:nrow(orig), col_name] = NA				
		}
		return(orig)
}


##################################################################################################
# threshold_probe_raw_best: 
# Only keep features whose aggregated score is better than the highest raw score of a random probe
# Best of the best strategy 
# Random probes are identified by names beginning with 'shadow_'
# Make sure not to return all NA's

threshold_probe_raw_best = function(data, raw_dat, col_name)
{
		non_na = na.omit(as.data.frame(data))
		omitted = attr(non_na, 'na.action')
		dat = as.data.frame(non_na)
		raw_dat = as.data.frame(raw_dat)
		
		ind = grep("^shadow_*", dat[,1], value=FALSE)
		raw_ind = grep("^shadow_*", raw_dat[,1], value=FALSE)
		probes = raw_dat[raw_ind, ]
		best = max(probes[, col_name], na.rm = TRUE)
		print(paste0("Best raw probe score = ", best))
		
		# For each feature, 
		orig = dat[-ind,]
		num_feats = nrow(orig)
	
		# Best might be NA if all probes had a value of NA - i.e. were not selected by the FS
		# In that case, the relevant varaibles are only those that the FS did not score as NA
		orig = orig[order(orig[,col_name], decreasing = TRUE, na.last = TRUE),] 
		if (is.na(best) || is.infinite(best)) {
			irr_var_idx = which(is.na(orig[,col_name]))
		} else {		
			x = orig[,col_name] < best
			irr_var_idx = c(which(x), which(is.na(x)))			# Which ones are TRUE or NA
		}

		print(paste0("Number of irrelevant variables = ", length(irr_var_idx)))
		if ((nrow(orig) - length(irr_var_idx)) > 1) {
				orig[irr_var_idx, col_name] = NA
		} else {
			print("Probe best - attempting to set all/all but one feature(s) to NA. Returning best 2.")
			orig[3:nrow(orig), col_name] = NA				
		}	
		return(rbind(orig, data[omitted,]))
}


##################################################################################################
# threshold_mprobes: 
# Uses the method of Huynh-Thu et al 2012:
#   - Count the number of times each feature scores a value below the best shadow in that iteration
#   - Calculate this as a proportion of the total 
#   - Only keep features for which that proportion is very low i.e. < FDR e.g. 10%
# Random probes are identified by names beginning with 'shadow_'
# Make sure not to return all NA's

threshold_mprobes = function(dat, raw_dat, col_name, alpha = 0.1)
{
		dat = as.data.frame(dat)
		print(paste0("Number of features selected: ", nrow(na.omit(dat))))
		ind = grep("^shadow_*", dat[,1], value=FALSE)
		raw_dat = as.data.frame(raw_dat)

		# For each feature, count the number of times that feature scored below the best shadow for that iteration
		num_feats = nrow(dat[-ind,])
		num_shadows = nrow(dat[ind,])
		fwer = rep(0.0, num_feats)
		names(fwer) = dat[-ind,1]
	
		# Iterate over each group of results
		first = 1
		last  = num_feats + num_shadows
		print(paste0("Num_feats = ", num_feats, ", num_shadows = ", num_shadows))
		while (last <= nrow(raw_dat)) {
#			print(paste0("First = ", first, ", Last = ", last))
			curr_dat = raw_dat[first:last, ]
			raw_ind = grep("^shadow_*", curr_dat[,1], value=FALSE)
			probes = curr_dat[raw_ind, ]
			orig_raw = curr_dat[-raw_ind, ]
			
			best = max(probes[, col_name], na.rm = TRUE)
			if (is.infinite(best)) {
				print("All probes were NA")
			} else {
				print(paste0("Best probe score = ", best))
				print(paste0("Best probe name = ", probes[which.max(probes[, col_name]), 1]))
			}
		
			# If best = 0 then values are in the reverse order, with best smallest e.g for RF-MD
			reverse_best = FALSE
			if (best == 0) {
				print("Best probe is zero!")
				best = min(probes[, col_name], na.rm = TRUE)
				reverse_best = TRUE
			}
			print(paste0("Best probe score = ", best))	
			
			# Best might be NA if all probes had a value of NA - i.e. were not selected by the FS
			# In that case, the relevant varaibles are only those that the FS did not score as NA
			if (is.na(best) || is.infinite(best)) {
				irr_var_idx = which(is.na(orig_raw[,col_name]))
			} else {		
				if (reverse_best)
					x = orig_raw[,col_name] > best * -1				
				else
					x = orig_raw[,col_name] < best
				irr_var_idx = c(which(x), which(is.na(x)))			# Which ones are TRUE or NA
			}
			irr_var_names = orig_raw[irr_var_idx,1]     
			fwer[irr_var_names] = fwer[irr_var_names] + 1
			first = first + num_feats + num_shadows
			last = last + num_feats + num_shadows
		}
		fwer = sort(fwer / NUM_BOOTSTRAP_SAMPLES, na.last = TRUE)

		orig = dat[-ind,]
		orig = orig[match(names(fwer), orig$name), ]   #Sort in order of fwer
		print(paste0("Length = ", length(fwer[fwer <= alpha])))
		if (length(fwer[fwer <= alpha]) > 1) {
				to_keep = names(fwer[fwer <= alpha])
				orig[!orig[,1] %in% to_keep, col_name] = NA
		} else {
				#Keep top 2 variables to prevent crashing
				print("Probe mprobes - attempting to set all/all but one feature(s) to NA. Returning best 2.")
				orig[3:nrow(orig), col_name] = NA				
		}
		
		return(orig)
}


##################################################################################################
# threshold_mprobes_agg: 
# Uses the method of Huynh-Thu et al 2012 MODIFIED TO HANDLE WEAK SIGNAL:
#   - Count the number of times each feature scores a value below the best aggregated shadow value
#   - Calculate this as a proportion of the total 
#   - Only keep features for which that proportion is very low i.e. < FDR e.g. 10%
# Random probes are identified by names beginning with 'shadow_'
# Make sure not to return all NA's

threshold_mprobes_agg = function(dat, raw_dat, col_name, alpha = 0.1)
{
		dat = as.data.frame(dat)
		print(paste0("Number of features selected: ", nrow(na.omit(dat))))
		raw_dat = as.data.frame(raw_dat)
		ind = grep("^shadow_*", dat[,1], value=FALSE)
		probes = dat[ind, ]
		
		best = max(probes[, col_name], na.rm = TRUE)
		if (is.infinite(best)) {
			print("All probes were NA")
		} else {
			print(paste0("Best aggregated probe score = ", best))
			print(paste0("Best probe name = ", probes[which.max(probes[, col_name]), 1]))
		}
		
		# If best = 0 then values are in the reverse order, with best smallest e.g for RF-MD
		reverse_best = FALSE
		if (best == 0) {
			print("Best probe is zero!")
			best = min(probes[, col_name], na.rm = TRUE)
			reverse_best = TRUE
		}
		print(paste0("Best probe score = ", best))

		# For each feature, count the number of times that feature scored below the best agregated shadow score
		num_feats = nrow(dat[-ind,])
		num_shadows = nrow(dat[ind,])
		fwer = rep(0.0, num_feats)
		names(fwer) = dat[-ind,1]
	
		# Iterate over each group of results
		print(paste0("Length = ", length(raw_dat), ", Num feats = ", num_feats, ", Num Shadows = ", num_shadows))
		first = 1
		last  = num_feats + num_shadows
		while (last <= nrow(raw_dat)) {
			curr_dat = raw_dat[first:last, ]
			raw_ind = grep("^shadow_*", curr_dat[,1], value=FALSE)
			orig_raw = curr_dat[-raw_ind, ]
			
			# Best might be -Inf if all probes had a value of NA - i.e. were not selected by the FS
			# In that case, the relevant varaibles are only those that the FS did not score as NA
			if (is.na(best) || is.infinite(best)) {
				irr_var_idx = which(is.na(orig_raw[,col_name]))
			} else {		
				if (reverse_best)
					x = orig_raw[,col_name] > best * -1				
				else
					x = orig_raw[,col_name] < best
				irr_var_idx = c(which(x), which(is.na(x)))			# Which ones are TRUE or NA
			}
      irr_var_names = orig_raw[irr_var_idx,1]
			fwer[irr_var_names] = fwer[irr_var_names] + 1
			first = first + num_feats + num_shadows
			last = last + num_feats + num_shadows
		}
		fwer = sort(fwer / NUM_BOOTSTRAP_SAMPLES, na.last = TRUE)

		orig = dat[-ind,]
		orig = orig[match(names(fwer), orig$name), ]   #Sort in order of fwer
		if (length(fwer[fwer <= alpha]) > 1) {
				to_keep = names(fwer[fwer <= alpha])
				orig[!orig[,1] %in% to_keep, col_name] = NA
		} else {
				#Keep top 2 variables to prevent crashing
				print("Probe mprobes aggregated - attempting to set all/all but one feature(s) to NA")
				orig[3:nrow(orig), col_name] = NA				
		}
		
		return(orig)
}

###############################################################################
# threshold_probe_avg: 
# Only keep features that score better than the highest-scoring random probe
# Random probes are identified by names beginning with 'shadow_'

threshold_probe_avg = function(dat, col_name)
{
		dat = as.data.frame(dat)
		ind = grep("^shadow_*", dat[,1], value=FALSE)
		probes = dat[ind, ]
		orig = dat[-ind,]
		avg = mean(probes[, col_name], na.rm = TRUE)
		print(paste0("Avg probe score = ", avg))
		
		if (!is.infinite(avg) && !is.nan(avg)) {
			x = sapply(orig[, col_name] < avg, all, na.rm=TRUE)
			cnt = sum(x, na.rm = TRUE)
			if (cnt < length(x))
				orig[x, col_name] = NA
			else
				print("Probe best - attempting to set all features to NA")
		} else {
			print("No change")
		}
		return(orig)
}


###############################################################################
# threshold_r2vim: 
# Uses method from paper "r2VIM: A new variable selection method for random forests in genome-wide association studies", Szymczak et al 2016
# Divides each importance score by the absolute value of the minimum importance score.
# Keeps only those whose relative score is greater than some factor - 1, 3, 5

threshold_r2vim = function(dat, col_name, factor)
{
		thresh = min(abs(dat[,col_name]), na.rm = TRUE)
		print(paste0("R2vim thresh = ", thresh))
		threshed = data.frame(dat)
		threshed[,col_name] = threshed[,col_name] / thresh
		
		# Set to NA any features 
		x = sapply(threshed[, col_name] < factor, all, na.rm=TRUE)
		if (class(x) == "list")
			x = unlist(x, use.names=FALSE)
		cnt = sum(x, na.rm = TRUE)
		print(paste0("Number of TRUE values: ", cnt))
		if (cnt < length(x))
			dat[x, col_name] = NA
		else
			print("R2VIM - attempting to set all features to NA")
		return(dat)
}


###############################################################################
# threshold_stddev: 
# Only keep those values >= 1 std dev below the mean
# Doesn't work with a skewed distribution

threshold_sd = function(dat, col_name)
{
		mean_val = mean(dat[,col_name], na.rm = TRUE)
		sd_val = sd(dat[,col_name], na.rm = TRUE)
		print(paste0("Mean: ", str(mean_val), " Std Dev: ", str(sd_val)))
		thresh_upper = mean_val + sd_val
		thresh_lower = mean_val - sd_val
		dat[na.omit(dat[,col_name]) < thresh_upper, col_name] = NA
    return(dat)
}

