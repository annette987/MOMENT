####################################################################################
# EXPLORATION
# CPO to run various exploration methods within the CV loop
#
####################################################################################

library(mlrCPO)
library(Rtsne)
library(ggfortify)
library(umap)

#
# Create UMAP, t-SNE and PCA plots to explore data
#
exploreData = function(plot_type, data, target, dir, file_prefix)
{
	print(paste0("In explore_data: ", plot_type, ", ", file_prefix))
	names(target) = 'Label'
	numeric_cols = unlist(lapply(data, function(x) {is.numeric(x) && !all(x %in% c(0,1))}))
	dat = data[, numeric_cols]

	if (plot_type == "TSNE") {
		 tryCatch({
				
				tsne_2 <- Rtsne(X = data.matrix(dat),
													perplexity = 2,
													theta = 0.0,
													eta = 10,
													max_iter = 5000,
													dims = 2,
													check_duplicates = FALSE)
				tsne_3 <- Rtsne(X = data.matrix(dat),
													perplexity = 3,
													theta = 0.0,
													eta = 10,
													max_iter = 5000,
													dims = 2,
													check_duplicates = FALSE)
				tsne_5 <- Rtsne(X = data.matrix(dat),
													perplexity = 5,
													theta = 0.0,
													eta = 10,
													max_iter = 5000,
													dims = 2,
													check_duplicates = FALSE)
				tsne_10 <- Rtsne(X = data.matrix(dat),
													perplexity = 10,
													theta = 0.0,
													eta = 10,
													max_iter = 5000,
													dims = 2,
													check_duplicates = FALSE)
				tsne_30 <- Rtsne(X = data.matrix(dat),
													perplexity = 30,
													theta = 0.0,
													eta = 10,
													max_iter = 5000,
													dims = 2,
													check_duplicates = FALSE)

				print("T-SNE 2")
				tsne_plot <- data.frame(x = tsne_2$Y[,1], y = tsne_2$Y[,2])
				jpeg(paste0(dir, "/", file_prefix, "_tsne_2.jpg"))
				p = ggplot2::ggplot(tsne_plot) + geom_point(aes(x=x,y=y,col=target))
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()

				print("T-SNE 3")
				tsne_plot <- data.frame(x = tsne_3$Y[,1], y = tsne_3$Y[,2])
				jpeg(paste0(dir, "/", file_prefix, "_tsne_3.jpg"))
				p = ggplot2::ggplot(tsne_plot) + geom_point(aes(x=x,y=y,col=target))
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()

				print("T-SNE 5")
				tsne_plot <- data.frame(x = tsne_5$Y[,1], y = tsne_5$Y[,2])
				jpeg(paste0(dir, "/", file_prefix, "_tsne_5.jpg"))
				p = ggplot2::ggplot(tsne_plot) + geom_point(aes(x=x,y=y,col=target))
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()
				
				print("T-SNE 10")
				tsne_plot <- data.frame(x = tsne_10$Y[,1], y = tsne_10$Y[,2])
				jpeg(paste0(dir, "/", file_prefix, "_tsne_10.jpg"))
				p = ggplot2::ggplot(tsne_plot) + geom_point(aes(x=x,y=y,col=target))
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()
								
				print("T-SNE 30")
				tsne_plot <- data.frame(x = tsne_30$Y[,1], y = tsne_30$Y[,2])
				jpeg(paste0(dir, "/", file_prefix, "_tsne_30.jpg"))
				p = ggplot2::ggplot(tsne_plot) + geom_point(aes(x=x,y=y,col=target))
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()				
		 }, 
		 error = function(cond) {
				print(paste("RTsne returned error: ", cond))
		 })
	} else if (plot_type == "PCA") {
		 tryCatch({
				print("PCA")
				jpeg(paste0(dir, "/", file_prefix, "_pca.jpg"))
				pca_res = prcomp(dat)
				df = data.frame(x = pca_res$x[,1], 
												y = pca_res$x[,2],
												Label = target)

				p = ggplot2::ggplot(df, aes(x, y, colour = Label)) + geom_point()
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()
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
				
				print("UMAP 5")
				jpeg(paste0(dir, "/", file_prefix, "_umap_5.jpg"))
				df = data.frame(x = umap_5$layout[,1],
												y = umap_5$layout[,2],
												Label = target)
				p = ggplot2::ggplot(df, aes(x, y, colour = Label)) + geom_point()
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()
				
				print("UMAP 10")
				jpeg(paste0(dir, "/", file_prefix, "_umap_10.jpg"))
				df = data.frame(x = umap_10$layout[,1],
												y = umap_10$layout[,2],
												Label = target)
				p = ggplot2::ggplot(df, aes(x, y, colour = Label)) + geom_point()
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()
				
				print("UMAP 15")
				jpeg(paste0(dir, "/", file_prefix, "_umap_15.jpg"))
				df = data.frame(x = umap_15$layout[,1],
												y = umap_15$layout[,2],
												Label = target)
				p = ggplot2::ggplot(df, aes(x, y, colour = Label)) + geom_point()
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()
				
				print("UMAP 30")
				jpeg(paste0(dir, "/", file_prefix, "_umap_30.jpg"))
				df = data.frame(x = umap_30$layout[,1],
												y = umap_30$layout[,2],
												Label = target)
				p = ggplot2::ggplot(df, aes(x, y, colour = Label)) + geom_point()
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()
				
				print("UMAP 50")
				jpeg(paste0(dir, "/", file_prefix, "_umap_50.jpg"))
				df = data.frame(x = umap_50$layout[,1],
												y = umap_50$layout[,2],
												Label = target)
				p = ggplot2::ggplot(df, aes(x, y, colour = Label)) + geom_point()
				p = p + scale_color_manual(values=c("red", "purple", "cyan", "chartreuse", "black"))
				print(p)
				dev.off()
		 }, 
		 error = function(cond) {
				print(paste("umap returned error: ", cond))
		 })
	}
}

cpoExplore = makeCPOExtendedTrafo("explore",  # nolint
  pSS(method   = "TSNE": character,
	    dir      = ".": character,
      modality = "METH": character,
			exp_num  = 0: integer),
  dataformat = "numeric",
	properties.data = c("numerics", "factors", "ordered", "missings"),
	properties.adding = "missings",
  cpo.trafo = function(data, target, method, dir, modality, exp_num) {
			exploreData(method, data, target, dir, paste0(modality, "_", exp_num))
			control = list(method = method)
			return(data)
  }, 
	cpo.retrafo = function(data, control, method, dir, modality, exp_num) {
			exploreData(method, data, target, dir, paste0(modality, "_", exp_num))
			return(data)
  })
