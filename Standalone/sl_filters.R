###########################################################################
# ADDITIONAL LEARNERS AND FEATURE SELECTORS FOR USE WITH THE SUPER LEARNER
###########################################################################

screen.Boruta <- function(Y, X, family, obsWeights, id, withTentative = TRUE, mustKeep = NULL, ...) {
  library(Boruta)

	boruta_res = Boruta(x = X, 
											y = Y,
											getImp = Boruta::getImpRfZ,
											doTrace = 0)
															
	# selected features
	to_keep = Boruta::getSelectedAttributes(boruta_res, withTentative = withTentative)
	if(!is.null(mustKeep)){
		to_keep = union(to_keep, mustKeep)
	}
	
	if(length(to_keep) == 0){
		warning('Boruta selected 0 features! Using 10 random features')
		to_keep = sample(colnames(data), 10)
	}
	
	message('Number of features selected: ', length(to_keep))

  # whichVariable is a logical vector,
  # TRUE indicates variable will be used
  whichVariable <- rep(FALSE, ncol(X))
	names(whichVariable) = names(X)
	whichVariable[to_keep] = TRUE
  return(whichVariable)
}