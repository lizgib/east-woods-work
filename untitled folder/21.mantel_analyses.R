#analyses on 2007  
library(ggplot2)
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/20.phylo_dist_matrices.R')
marlin_data <- read.csv('~/Documents/morton arb/east_woods_phylogeny/DATA/marlins_data.csv')

marlin_herb_plots <- marlin_data[which(marlin_data$Plot %in% intersect(marlin_data$Plot, rownames(dat.mat.understory.07))),]

euc_herbs <- dist(marlin_herb_plots, method = 'euclidean')
dist_slope <- dist(marlin_herb_plots$Slope, method = 'euclidean')
dist_elevation <- dist(marlin_herb_plots$Elevation, method = 'euclidean')
dist_aspect <- dist(marlin_herb_plots$Aspect, method = 'euclidean')
dist_invasives_ratio <- dist(marlin_herb_plots$AlienNativeRatio, method = 'euclidean')
dist_canopy <- dist(marlin_herb_plots$CanopyOpenness, method = 'euclidean')
marlin_herb_plots$aspect2 <- ifelse(marlin_herb_plots$Aspect > 180, 360 - marlin_herb_plots$Aspect, marlin_herb_plots$Aspect)
marlin_herb_plots$drought <- (marlin_herb_plots$aspect2 * marlin_herb_plots$Slope) + marlin_herb_plots$Elevation
dist_drought <- dist(marlin_herb_plots$drought, method = 'euclidean')

marlin_tree_plots <- marlin_data[which(marlin_data$Plot %in% intersect(marlin_data$Plot, rownames(dat.mat.trees.07))),]
euc_trees <- dist(marlin_tree_plots, method = 'euclidean') 


##################################################################################################################
#HERB ANALYSES 
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/mantelMultiple.R')


dist_aspect.2 <- ifelse(dist_aspect > 180, 360 - dist_aspect, dist_aspect)
herb_mantel <- mantelMultiple(beta_Dpw_herbs.pa, X = list(aspect = dist_aspect.2, slope = dist_slope, 
                                                          elevation = dist_elevation, canopy = dist_canopy, drought = dist_drought))
a <- lm(beta_Dnn_herbs.pa ~ dist_slope)
b <- lm(beta_Dnn_herbs.pa ~ dist_aspect.2)
c <- lm(beta_Dnn_herbs.pa ~ dist_elevation)
d <- lm(beta_Dnn_herbs.pa ~ dist_canopy)
e <- lm(beta_Dnn_herbs.pa ~ dist_invasives_ratio)
f <- lm(beta_Dnn_herbs.pa ~ dist_aspect.2 + dist_slope + dist_elevation)
g <- lm(beta_Dnn_herbs.pa ~ beta_Dnn_trees.pa + dist_canopy)
h <- lm(beta_Dnn_herbs.pa ~ dist_aspect.2 + dist_slope + dist_elevation + 
          dist_invasives_ratio + beta_Dnn_trees.pa + dist_canopy)

#if DPw is nonsignificant....maybe try a terminal metric (see p 150 in Cadotte)
#terminal metrics sensitive to turnover at tips of tree
#basal metrics (such as Dpw) are sensitive to turnover deeper in phylogeny
#in the book, with his sample dataset he found that the terminal metrics were more significantly 
#related to environmental distances
##################################################################################################################

tree_mantel <- mantelMultiple(beta_Dnn_trees.pa, X = list(aspect = dist_aspect.2, slope = dist_slope, 
                                                          elevation = dist_elevation, canopy = dist_canopy))

i <- lm(beta_Dnn_trees.pa ~ dist_slope)
j <- lm(beta_Dnn_trees.pa ~ dist_aspect.2)
k <- lm(beta_Dnn_trees.pa ~ dist_elevation)
l <- lm(beta_Dnn_trees.pa ~ dist_canopy)
m <- lm(beta_Dnn_trees.pa ~ dist_invasives_ratio)
n <- lm(beta_Dnn_trees.pa ~ abs(180 - dist_aspect) + dist_slope + dist_elevation)
o <- lm(beta_Dnn_trees.pa ~ beta_Dnn_trees.pa + dist_canopy)
p <- lm(beta_Dnn_trees.pa ~ abs(180 - dist_aspect) + dist_slope + dist_elevation + 
          dist_invasives_ratio + beta_Dnn_trees.pa + dist_canopy)

#################################################################################################################
#alllll species

all_mantel <- mantelMultiple(beta_Dnn_all.pa, X = list(aspect = dist_aspect.2, slope = dist_slope, 
                                                                 elevation = dist_elevation, canopy = dist_canopy, drought = dist_drought))

plot(beta_Dnn_herbs.pa, beta_Dnn_trees.pa)

#################################################################################################################

#PLOTS 



ggplot()+
  geom_point(aes(beta_Dnn_trees.pa, beta_Dnn_herbs.pa), color = 'light blue')+
  xlab('Tree Diversity')+
  ylab('Herb Diversity') + 
  ggtitle('Tree Diversity Effect on Herb Diversity 2007') +
  theme(
    plot.title = element_text(size = 25),
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))


ggplot()+
  geom_point(aes(dist_slope, beta_Dnn_all.pa), color = 'light blue') + 
  xlab('Slope') + 
  ylab('Phylobetadiversity')+
  ggtitle('Slope effect on all species 2007') +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))

ggplot()+
  geom_point(aes(dist_aspect.2, beta_Dnn_all.pa), color = 'light blue') + 
  xlab('Aspect') + 
  ylab('Phylobetadiversity')+
  ggtitle('Aspect effect on all species 2007') +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))

ggplot()+
  geom_point(aes(dist_elevation, beta_Dnn_all.pa), color = 'light blue') + 
  xlab('Elevation') + 
  ylab('Phylobetadiversity')+
  ggtitle('Elevation effect on all species 2007') +
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))

ggplot()+
  geom_point(aes(dist_canopy, beta_Dnn_all.pa), color = 'light blue') +
  xlab('Canopy Openness') +
  ylab('Beta Diversity')+
  ggtitle('Canopy openness effect on all species 2007') +
  theme(
    axis.title.x = element_text(size = 20),     #I REDID THIS PLOT WITH THE OUTLIERS REMOVED
    axis.title.y = element_text(size = 20))

ggplot()+ 
  geom_point(aes(dist_drought, beta_Dnn_all.pa), color = 'light blue') + 
  xlab('Drought index') + 
  ylab('Phylobetadiversity') + 
  ggtitle('Drough Index effect on all species 2007') +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))
