# MOMENT - Multi-Omics Modelling by ENsemble Techniques

MOMENT provides a set of ensemble machine learning algorithms that can successfully model multi-omics multi-class data. The algorithms are based on a technique known as late integration, training a machine learning model on each modality independently and aggregating the results in different ways to give a final prediction. 

In addition, MOMENT allows users to model individual modalities, a concatenation of all modalities or a selected subset of modalities, for comparison with the ensemble models, employing either classification or survival analysis. Furthermore, an incremental model can determine a minimal set of modalities for accurate modelling of the data and an exploration model allows the user to plot 2-dimensional representations of the data using principal component analysis (PCA), t-distributed stochastic neighbor embedding (t-SNE) or Uniform Manifold Approximation and Projection (UMAP).

MOMENT is fully customisable and highly flexible, using an object-oriented design. It allows the user to model multi-omics multi-class data, without the need for a detailed understanding of the specifics of machine learning, nor of the mlr (Machine Learning in R) package, on which MOMENT relies.

### Example:

library(MOMENT)  
subset = NULL  
cc = config("your_config_file.xlsx")  
vote_ens_h = MM_Voting$new(cc, decision = 'hard', subset = subset, validate = FALSE)  
res_ens_h = vote_ens_h$learn()  
res_ens_h$write("voting_hard  
