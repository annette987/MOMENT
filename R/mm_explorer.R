#' R6 Class representing a multi-modal voting ensemble classifier
#'
#' @description
#' Creates a multi-modal ensemble classifier that uses 
#' hard or soft voting to determine the final outcome of the classifier.
#'
#' @details
#' Trains a classifier on each modality then combines the
#' predictions from those modalities using a hard vote
#' (majority voting) or a soft vote (average of probabilities)
#' to give a final prediction.
#'
#' @name MM_Explorer
#' @docType package
NULL

source("imputation.R")

MM_Explorer = R6::R6Class("MM_Explorer", 
	inherit = MM_Model,
	public = list(
		
    #' @description 
		#' Create a new MM_Explorer object.
    #' @param config Model parameters (MM_Config).
    #' @return A new `MM_Explorer`object.
		#' @examples
		#' mod = MM_Explorer$new(config)
		#' @export
		initialize = function(config, decision = "prob", subset = NULL, balance = FALSE, filter_zeroes = FALSE, filter_missings = FALSE, filter_corr = FALSE, filter_var = FALSE) {
			super$initialize(config, "VOTE", decision, subset, FALSE, balance, filter_zeroes, filter_missings, filter_corr, filter_var)
			print("Explorer initialised")
		},		


		#
		# Create UMAP, t-SNE and PCA plots to explore data
		#
		exploreData = function(plot_type, data, target, file_prefix)
		{
			names(target) = 'Label'
			numeric_cols = unlist(lapply(data, function(x) {is.numeric(x) && !all(x %in% c(0,1))}))
			dat = data[, numeric_cols]

			if (plot_type == "TSNE") {
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
						print(paste("Rtsne::Rtsne returned error: ", cond))
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
						print(paste("prcomp returned error: ", cond))
				 })
			} else if (plot_type == "UMAP") {
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
						print(paste("umap returned error: ", cond))
				 })
			}
		},

		
  	#' @description 
		#' @export
		learn = function(config, file_prefix) 
		{
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