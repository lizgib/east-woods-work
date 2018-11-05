#analyses on 2007  
library(ggplot2)
source('~/Documents/GitHub/east_woods_work/scripts/10.phylo_dist_matrices.R')
source('~/Documents/GitHub/east_woods_work/scripts/09.envt_data.R')
source('~/Documents/GitHub/east_woods_work/scripts/11.analyses.R')

invasives_18 <- dist(liz_data$invasive_ratio_18)
invasives_07 <- dist(liz_data$invasive_ratio_07)
canopy <- dist(liz_data$canopy_18)
# pdiversity_07 <- dist(phylo_all) <- this one shold use the phylogenetic distance metric not euclidean
# percent_ACM <-dist(liz_data$percent_ACM)
# percent_ECM <- dist(liz_data$percent_ECM)  # NEDD TO CALCULATE THESE STILL 
# tree phylogenetic diversity

##################################################################################################################

biotic_mantel <- mantelMultiple(beta_Dpw_herbs.pa, X = list(aspect = dist_aspect.2, slope = dist_slope, 
                                                          elevation = dist_elevation, canopy = dist_canopy, drought = dist_drought))

#if DPw is nonsignificant....maybe try a terminal metric (see p 150 in Cadotte)
#terminal metrics sensitive to turnover at tips of tree
#basal metrics (such as Dpw) are sensitive to turnover deeper in phylogeny
#in the book, with his sample dataset he found that the terminal metrics were more significantly 
#related to environmental distances
##################################################################################################################

ggplot()+
  geom_point(aes(beta_Dnn_trees.pa, beta_Dnn_herbs.pa), color = 'light blue')+
  xlab('Tree Diversity')+
  ylab('Herb Diversity') + 
  ggtitle('Tree Diversity Effect on Herb Diversity 2007') +
  theme(
    plot.title = element_text(size = 25),
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))


