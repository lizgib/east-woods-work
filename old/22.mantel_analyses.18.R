#analyses on 2018  

library(ggplot2)
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/20.phylo_dist_matrices.R')
env_data <- read.csv('~/Documents/morton arb/east_woods_phylogeny/DATA/liz_data.csv')
env_data$plots <- gsub('-', '', env_data$plots)

herb_plots <- env_data[which(env_data$plots %in% intersect(env_data$plots, rownames(dat.mat.understory.18))),]

euc_herbs <- dist(herb_plots, method = 'euclidean')
dist_slope <- dist(herb_plots$slope, method = 'euclidean')
dist_elevation <- dist(herb_plots$elevation, method = 'euclidean')
dist_aspect <- dist(herb_plots$aspect, method = 'euclidean')
herb_plots$aspect2 <- ifelse(herb_plots$aspect > 180, 360 - herb_plots$aspect, herb_plots$aspect)
herb_plots$drought <- (herb_plots$aspect2 * herb_plots$slope) + herb_plots$elevation
dist_drought <- dist(herb_plots$drought, method = 'euclidean')

tree_plots <- env_data[which(env_data$plots %in% intersect(env_data$plots, rownames(dat.mat.trees.18 ))),]
euc_trees <- dist(tree_plots, method = 'euclidean')


##################################################################################################################################################
#HERB ANALYSES 
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/mantelMultiple.R')


dist_aspect.2 <- ifelse(dist_aspect > 180, 360 - dist_aspect, dist_aspect)
herb_mantel <- mantelMultiple(beta_Dpw_herbs.pa, X = list(aspect = dist_aspect.2, slope = dist_slope, 
                                                          elevation = dist_elevation, drought = dist_drought))

################################################################################################################################################

#PLOTS 

ggplot()+
  geom_point(aes(beta_Dnn_trees.pa, beta_Dnn_herbs.pa), color = 'light blue')+
  xlab('Tree Diversity')+
  ylab('Herb Diversity') + 
  ggtitle('Tree Diversity Effect on Herb Diversity 2018') +
  theme(
    plot.title = element_text(size = 25),
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))


ggplot()+
  geom_point(aes(dist_slope, beta_Dnn_all.pa), color = 'light blue') + 
  xlab('Slope') + 
  ylab('Phylobetadiversity')+
  ggtitle('Slope effect on all species 2018') +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))

ggplot()+
  geom_point(aes(dist_aspect.2, beta_Dnn_all.pa), color = 'light blue') + 
  xlab('Aspect') + 
  ylab('Phylobetadiversity')+
  ggtitle('Aspect effect on all species 2018') +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))

ggplot()+
  geom_point(aes(dist_elevation, beta_Dnn_all.pa), color = 'light blue') + 
  xlab('Elevation') + 
  ylab('Phylobetadiversity')+
  ggtitle('Elevation effect on all species 2018') +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))

ggplot()+ 
  geom_point(aes(dist_drought, beta_Dnn_all.pa), color = 'light blue') + 
  xlab('Drought index') + 
  ylab('Phylobetadiversity') + 
  ggtitle('Drough Index effect on all species 2018') +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))
