#' R6 Class for exploring the raw data
#'
#' @description
#' Allow the user to generate PCA, TSNE and UMAP plots
#' for each modality in the data.
#'
#' @name MM_Explorer
#' @docType package
NULL


MM_Explorer = R6::R6Class("MM_Explorer", 
	inherit = MM_Model,
	public = list(
		
    #' @description 
		#' Create a new MM_Explorer object.
		#' @param config (MM_Config)\cr
		#' Configuration object, specifying how the model should be constructed.
    #' @param model_type (character)\cr
		#' Type of model - "CLASSIF" for classification or "SURV" for survival analysis. 
		#' @param decision (character)\cr
		#' Type of prediction - 'response' or 'prob'.
		#' @param subset (integer)\cr
		#' @param concat (logical(1))\cr
		#' Should the tasks be concatenated to form a single, large dataset?
		#' @param balance (logical(1))\cr
		#' Should the tasks be balanced during training?
		#' @param validate (logical(1))\cr
		#' Should the model be validated with validation data provided in the config file.
		#' @param filter_zeroes (double(1))\cr
		#' Features with this percentage of zero values or greater will not be included in the model.
		#' @param filter_missings (double(1))\cr
		#' Features with this percentage of missing values or greater will not be included in the model.
		#' @param filter_corr (double(1))\cr
		#' Should correlated features be included in the model? If FALSE, one feature from each correlated pair is eliminated.
		#' @param filter_var (double(1))\cr
		#' Should low variance features be included in the model?
    #' @return A new [MM_Explorer] object.
		#' @export
		initialize = function(config, decision = "prob", subset = NULL, balance = FALSE, filter_zeroes = FALSE, filter_missings = FALSE, filter_corr = FALSE, filter_var = FALSE) {
			super$initialize(config, "VOTE", decision, subset, FALSE, balance, filter_zeroes, filter_missings, filter_corr, filter_var)
		},		


		#' @description
		#' Create UMAP, t-SNE and PCA plots for one modality of  data.
		#' Each plot is geerated with a range of parameters.
		#' @param plot_type (character)\cr
		#' The type of plot to produce. Can be either "PCA", "TSNE" or "UMAP".
		#' @param data (data.frame)\cr
		#' The raw data to be explored.
		#' @param target (character)\cr
		#' The target variable in the data.frame.
		#' @param file_prefix (character)\cr
		#' The prefix (including path) of the filename to which the plots should be saved.
    #' @return Nothing.
		#' @export
		exploreData = function(plot_type, data, target, file_prefix)
		{
			checkmate::assertChoice(plot_type, choices = c("PCA", "TSNE", "UMAP"))
			checkmate::assertClass(data, data.frame)
			checkMate::assertString(target)
			checkMate::assertStrinf(file_prefix)
			if (!requireNamespace("ggplot2", quietly = TRUE)) {
				stop("Package \'ggplot2\' must be installed to generate plots")
			}
			
			names(target) = 'Label'
			numeric_cols = unlist(lapply(data, function(x) {is.numeric(x) && !all(x %in% c(0,1))}))
			dat = data[, numeric_cols]

			if (plot_type == "TSNE") {
				 if (!requireNamespace("Rtsne", quietly = TRUE)) {
					 stop("Package \'Rtsne\' must be installed to generate TSNE plots")
				 }
				 tryCatch({
						
						tsne_2 <- Rtsne::Rtsne(X = data.matrix(dat),
																	perplexity = 2,
																	theta = 0.0,
																	eta = 10,
																	max_iter = 5000,
																	dims = 2,
																	check_duplicates = FALSE)
						tsne_3 <- Rtsne::Rtsne(X = data.matrix(dat),
																	perplexity = 3,
																	theta = 0.0,
																	eta = 10,
																	max_iter = 5000,
																	dims = 2,
																	check_duplicates = FALSE)
						tsne_5 <- Rtsne::Rtsne(X = data.matrix(dat),
																	perplexity = 5,
																	theta = 0.0,
																	eta = 10,
																	max_iter = 5000,
																	dims = 2,
																	check_duplicates = FALSE)
						tsne_10 <- Rtsne::Rtsne(X = data.matrix(dat),
																	perplexity = 10,
																	theta = 0.0,
																	eta = 10,
																	max_iter = 5000,
																	dims = 2,
																	check_duplicates = FALSE)
						tsne_30 <- Rtsne::Rtsne(X = data.matrix(dat),
																	perplexity = 30,
																	theta = 0.0,
																	eta = 10,
																	max_iter = 5000,
																	dims = 2,
																	check_duplicates = FALSE)

						tsne_plot <- data.frame(x = tsne_2$Y[,1], y = tsne_2$Y[,2])
						p = ggplot2::ggplot(tsne_plot) + ggplot2::geom_point(ggplot2::aes(x=x,y=y,col=target))
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_tsne_2.jpg"), plot = p, dpi = 300, device = "jpeg")

						tsne_plot <- data.frame(x = tsne_3$Y[,1], y = tsne_3$Y[,2])
						p = ggplot2::ggplot(tsne_plot) + ggplot2::geom_point(ggplot2::aes(x=x,y=y,col=target))
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_tsne_3.jpg"), plot = p, dpi = 300, device = "jpeg")

						tsne_plot <- data.frame(x = tsne_5$Y[,1], y = tsne_5$Y[,2])
						p = ggplot2::ggplot(tsne_plot) + ggplot2::geom_point(ggplot2::aes(x=x,y=y,col=target))
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_tsne_5.jpg"), plot = p, dpi = 300, device = "jpeg")
						
						tsne_plot <- data.frame(x = tsne_10$Y[,1], y = tsne_10$Y[,2])
						p = ggplot2::ggplot(tsne_plot) + ggplot2::geom_point(ggplot2::aes(x=x,y=y,col=target))
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_tsne_10.jpg"), plot = p, dpi = 300, device = "jpeg")
										
						tsne_plot <- data.frame(x = tsne_30$Y[,1], y = tsne_30$Y[,2])
						p = ggplot2::ggplot(tsne_plot) + ggplot2::geom_point(ggplot2::aes(x=x,y=y,col=target))
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_tsne_30.jpg"), plot = p, dpi = 300, device = "jpeg")
				 }, 
				 error = function(cond) {
						warning(paste("Rtsne::Rtsne returned error: ", cond))
				 })
			} else if (plot_type == "PCA") {
				 tryCatch({
						pca_res = prcomp(dat)
						df = data.frame(x = pca_res$x[,1], 
														y = pca_res$x[,2],
														Label = target)
						p = ggplot2::ggplot(df, ggplot2::aes(x, y, colour = Label)) + ggplot2::geom_point()
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_pca.jpg"), plot = p, dpi = 300, device = "jpeg")
				 }, 
				 error = function(cond) {
						warning(paste("prcomp returned error: ", cond))
				 })
			} else if (plot_type == "UMAP") {
				  if (!requireNamespace("umap", quietly = TRUE)) {
					  stop("Package \'umap\' must be installed to generate UMAP plots")
				  }
					tryCatch({
						umap_5  = umap::umap(dat, n_components = 2, n_neighbors = 5, min_dist = 0.1)
						umap_10 = umap::umap(dat, n_components = 2, n_neighbors = 10, min_dist = 0.1)
						umap_15 = umap::umap(dat, n_components = 2, n_neighbors = 15, min_dist = 0.1)
						umap_30 = umap::umap(dat, n_components = 2, n_neighbors = 30, min_dist = 0.1)
						umap_50 = umap::umap(dat, n_components = 2, n_neighbors = 50, min_dist = 0.1)
						
						df = data.frame(x = umap_5$layout[,1],
														y = umap_5$layout[,2],
														Label = target)
						p = ggplot2::ggplot(df, ggplot2::aes(x, y, colour = Label)) + ggplot2::geom_point()
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_umap_5.jpg"), plot = p, dpi = 300, device = "jpeg")
						
						df = data.frame(x = umap_10$layout[,1],
														y = umap_10$layout[,2],
														Label = target)
						p = ggplot2::ggplot(df, ggplot2::aes(x, y, colour = Label)) + ggplot2::geom_point()
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_umap_10.jpg"), plot = p, dpi = 300, device = "jpeg")
						
						df = data.frame(x = umap_15$layout[,1],
														y = umap_15$layout[,2],
														Label = target)
						p = ggplot2::ggplot(df, ggplot2::aes(x, y, colour = Label)) + ggplot2::geom_point()
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_umap_15.jpg"), plot = p, dpi = 300, device = "jpeg")
						
						df = data.frame(x = umap_30$layout[,1],
														y = umap_30$layout[,2],
														Label = target)
						p = ggplot2::ggplot(df, ggplot2::aes(x, y, colour = Label)) + ggplot2::geom_point()
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_umap_30.jpg"), plot = p, dpi = 300, device = "jpeg")
						
						df = data.frame(x = umap_50$layout[,1],
														y = umap_50$layout[,2],
														Label = target)
						p = ggplot2::ggplot(df, ggplot2::aes(x, y, colour = Label)) + ggplot2::geom_point()
						p = p + ggplot2::scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
						ggplot2::ggsave(filename = paste0(file_prefix, "_umap_50.jpg"), plot = p, dpi = 300, device = "jpeg")
				 }, 
				 error = function(cond) {
						warning(paste("umap returned error: ", cond))
				 })
			}
		},

		
  	#' @description
		#' Explore each modality of data.
		#' PCA, TSNE and UMAP plots with varying parameters are generated for each modality and saved to disk.
		#' @param config (config)\cr
		#' The config object contain details of the data to be explored.
		#' @param file_prefix (character)\cr
		#' The prefix (including path) of the filename to which the plots should be saved.
    #' @return Nothing.
		#' @export
		learn = function(config, file_prefix) 
		{
			assertClass(config, "config")
			assertString(file_prefix)
			
			for (i in 1:length(self$tasks)) {
				task_id = self$tasks[[i]]$task.desc$id
				dat = mlr::getTaskData(self$tasks[[i]], target.extra = TRUE)
				imp_data = imputeData(dat$data, config$baseModels[[i]]$imputation, NULL)
				if (any(is.na(imp_data))) {
					warning("Missing values present after imputation!")
				} else {
					filename = paste(file_prefix, task_id, sep = "_")
					self$exploreData("PCA", imp_data$data, dat$target, filename)
					self$exploreData("TSNE", imp_data$data, dat$target, filename)
					self$exploreData("UMAP", imp_data$data, dat$target, filename)
				}
			}
		}
	)
)