#analyses on 2007  

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/newstuff/00.read_data.R')
source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/phylo_dist_matrices.R')
#im getting some error here : Error in if (any(x < 0, na.rm = na.rm)) { : 
#missing value where TRUE/FALSE needed
#i dont know where it's coming from bc the phylo_dist_matrices script runs fine on its own when I 
#dont source it.... 
#the pa matrices were successfully generated though so for today (8/2) ill just use those
source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/newstuff/eco_dist_matrices.R')

marlin_herb_plots <- marlin_data[which(rownames(marlin_data) %in% intersect(rownames(marlin_data), rownames(dat.mat.herbs))),]
euc_herbs <- dist(marlin_herb_plots, method = 'euclidean')
dist_slope <- dist(marlin_herb_plots$Slope, method = 'euclidean')
dist_elevation <- dist(marlin_herb_plots$Elevation, method = 'euclidean')
dist_aspect <- dist(marlin_herb_plots$Aspect, method = 'euclidean')
dist_invasives_ratio <- dist(marlin_herb_plots$AlienNativeRatio, method = 'euclidean')
dist_canopy <- dist(marlin_herb_plots$CanopyOpenness, method = 'euclidean')
marlin_herb_plots$aspect2 <- ifelse(marlin_herb_plots$Aspect > 180, 360 - marlin_herb_plots$Aspect, marlin_herb_plots$Aspect)
marlin_herb_plots$drought <- (marlin_herb_plots$aspect2 * marlin_herb_plots$Slope) + marlin_herb_plots$Elevation
dist_drought <- dist(marlin_herb_plots$drought, method = 'euclidean')

marlin_tree_plots <- marlin_data[which(rownames(marlin_data) %in% intersect(rownames(marlin_data), rownames(dat.mat.trees))),]
euc_trees <- dist(marlin_tree_plots, method = 'euclidean')
###############################################################################################
#make some graphs to show if phylogenetic measures more sensitive than ecological ones 

eco.herbs.v.phy.herbs <- plot(beta_Dpw_herbs.pa, herbs_eco_dist.pa, xlab = 'Herb Phylogenetic Diversity', ylab = 'Herb Ecological Diversity (SR)')

eco.trees.v.phy.trees <- plot(beta_Dpw_trees.pa, trees_eco_dist.pa, xlab = 'Tree Phylogenetic Diversity', ylab = 'Tree Ecological Diversity (SR)')

####################################################################################################
#compare the effect of PD in canopy on PD in understory 

canopyPD.v.herbPD <- plot(trees_for_herbs.pa, herbs_for_trees.pa, xlab = 'Tree PD', ylab = 'Herb PD')
canopyPD_herbPD <- lm(trees_for_herbs.pa ~ herbs_for_trees.pa)

####################################################################################################
#HERB ANALYSES 
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/mantelMultiple.R')


dist_aspect.2 <- ifelse(dist_aspect > 180, 360 - dist_aspect, dist_aspect)
herb_mantel <- mantelMultiple(beta_Dnn_herbs.pa, X = list(aspect = dist_aspect.2, slope = dist_slope, 
               elevation = dist_elevation, canopy = dist_canopy))#, invasives_ratio = dist_invasives_ratio))
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
####################################################################################################

tree_mantel <- mantelMultiple(beta_Dnn_trees.pa, X = list(aspect = dist_aspect.2, slope = dist_slope, 
            elevation = dist_elevation, canopy = dist_canopy))#, invasives_ratio = dist_invasives_ratio))

i <- lm(beta_Dnn_trees.pa ~ dist_slope)
j <- lm(beta_Dnn_trees.pa ~ dist_aspect.2)
k <- lm(beta_Dnn_trees.pa ~ dist_elevation)
l <- lm(beta_Dnn_trees.pa ~ dist_canopy)
m <- lm(beta_Dnn_trees.pa ~ dist_invasives_ratio)
n <- lm(beta_Dnn_trees.pa ~ abs(180 - dist_aspect) + dist_slope + dist_elevation)
o <- lm(beta_Dnn_trees.pa ~ beta_Dnn_trees.pa + dist_canopy)
p <- lm(beta_Dnn_trees.pa ~ abs(180 - dist_aspect) + dist_slope + dist_elevation + 
          dist_invasives_ratio + beta_Dnn_trees.pa + dist_canopy)

####################################################################################################
#alllll species

all_mantel_w_drought <- mantelMultiple(beta_Dnn_all.pa, X = list(aspect = dist_aspect.2, slope = dist_slope, 
              elevation = dist_elevation, canopy = dist_canopy, drought = dist_drought))#, invasives_ratio = dist_invasives_ratio))


####################################################################################################
#SO i have not found any significant correlation between the phylogenetic distances and my environmental 
#distances
#NOW looking at correlation between the ecological (jaccard) distances and the environmental distances
####################################################################################################

eco_all_mantel <- mantelMultiple(all_eco_dist.pa, X = list(aspect = dist_aspect.2, slope = dist_slope, 
             elevation = dist_elevation, canopy = dist_canopy))#, invasives_ratio = dist_invasives_ratio))

eco_herb_mantel <- mantelMultiple(herbs_eco_dist.pa, X = list(aspect = dist_aspect.2, slope = dist_slope, 
             elevation = dist_elevation, canopy = dist_canopy))#, invasives_ratio = dist_invasives_ratio))

####################################################################################################
#PLOTS 



ggplot()+
  geom_point(aes(beta_Dnn_trees.pa, beta_Dnn_herbs.pa), color = 'orange')+
  xlab('Tree Diversity')+
  ylab('Herb Diversity') + 
  ggtitle('Tree Diversity Effect on Herb Diversity') +
  theme(
    plot.title = element_text(size = 25),
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))


ggplot()+
  geom_point(aes(dist_slope, beta_Dnn_all.pa), color = 'orange') + 
  xlab(' ') + 
  ylab(' ')+
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))

ggplot()+
  geom_point(aes(dist_aspect.2, beta_Dnn_all.pa), color = 'orange') + 
  xlab(' ') + 
  ylab(' ')+
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))

ggplot()+
  geom_point(aes(dist_elevation, beta_Dnn_all.pa), color = 'orange') + 
  xlab(' ') + 
  ylab(' ')+
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))

# ggplot()+
#   geom_point(aes(dist_canopy, beta_Dnn_all.pa), color = 'orange') + 
#   xlab('Canopy Openness') + 
#   ylab('Beta Diversity')+
#   theme(
#     axis.title.x = element_text(size = 20),     #I REDID THIS PLOT WITH THE OUTLIERS REMOVED
#     axis.title.y = element_text(size = 20))

ggplot()+ 
  geom_point(aes(all_eco_dist.pa, beta_Dnn_all.pa), color = 'orange') + 
  xlab(' ') + 
  ylab(' ') 

ggplot()+ 
  geom_point(aes(dist_drought, beta_Dnn_all.pa), color = 'orange') + 
  xlab(' ') + 
  ylab(' ') 


####################################################################################################
#Spatial effects 
source('~/Documents/morton arb/east_woods_phylogeny/analyses/spatial_dist.R')

PD_all_space.pa <- mantelMultiple(beta_Dnn_all.pa, X = list(dist_drought))


####################################################################################################
#SAVE MANTEL TESTS
sink('mantel_test_outputs.txt')
print("herb mantel")
print(herb_mantel)
print('tree mantel')
print(tree_mantel)
print('all mantel')
print(all_mantel)
print('eco mantels')
print('all')
print(eco_all_mantel)
print('herb')
print(eco_herb_mantel)
sink()



