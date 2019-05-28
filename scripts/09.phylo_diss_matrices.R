#phylogenetic distance matrices 
#tells you how phylogenetically dissimilar each plot is from each other

library(picante)
library(vegan)
# trying to make things faster... cophenetic tree to use here 

gibbons_data <- read.csv('data/gibbons_data.csv')
dat.mat.all.07 <- read.csv('data/dat.mat.all.07.csv', row.names = 1)
dat.mat.all.18 <- read.csv('data/dat.mat.all.18.csv', row.names = 1)
dat.mat.understory.07 <- read.csv('data/dat.mat.understory.07.csv', row.names = 1)
dat.mat.understory.18 <- read.csv('data/dat.mat.understory.18.csv', row.names = 1)
dat.mat.trees.07 <- read.csv('data/dat.mat.trees.07.csv', row.names = 1)
dat.mat.trees.18 <- read.csv('data/dat.mat.trees.18.csv', row.names = 1)
tr.ewv4 <- read.tree('outputs/tr.ewv4')
tree <- cophenetic(tr.ewv4)

#------------------------------------------------------------------------------------------------------------

dis_mat <- function(dat.mat, gibbons_data, tree){
  dat.mat <- dat.mat[which(rownames(dat.mat) %in% intersect(gibbons_data$plots, rownames(dat.mat))),]
#  pairwise nearest neighbor similarity.. using this one because it is a more terminal metric (better picks up on envt sensitivity)
  beta_dnn <- comdistnt(dat.mat, tree)
# if nearest neighbor is having problems (bc it was in the past and I never figured it out) try mean pairwise distance 
#  beta_dpw <- comdist(dat.mat, tree)
  return(beta_dpw)
}

jaccard_mat <- function(dat.mat, gibbons_data, tree){
  dat.mat <- dat.mat[which(rownames(dat.mat) %in% intersect(gibbons_data$plots, rownames(dat.mat))),]
  jac_mat <- vegdist(dat.mat, method = 'jaccard')
  return(jac_mat)
}


#--------------------------------------------------------------------------------------------------------

# 2007 
phylo_mat_07 <- dis_mat(dat.mat.all.07, gibbons_data, tree)
phylo_mat_under07 <- dis_mat(dat.mat.understory.07, gibbons_data, tree)
phylo_mat_tree07 <- dis_mat(dat.mat.trees.07, gibbons_data, tree)

eco_mat07 <- jaccard_mat(dat.mat.all.07, gibbons_data, tree)
eco_mat_under07 <- jaccard_mat(dat.mat.understory.07, gibbons_data, tree)
eco_mat_tree07 <- jaccard_mat(dat.mat.trees.07, gibbons_data, tree)

# 2018 
phylo_mat_18 <- dis_mat(dat.mat.all.18, gibbons_data, tree)
phylo_mat_under18 <- dis_mat(dat.mat.understory.18, gibbons_data, tree)
phylo_mat_tree18 <- dis_mat(dat.mat.trees.18, gibbons_data, tree)

eco_mat18 <- jaccard_mat(dat.mat.all.18, gibbons_data, tree)
eco_mat_under18 <- jaccard_mat(dat.mat.understory.18, gibbons_data, tree)
eco_mat_tree18 <- jaccard_mat(dat.mat.trees.18, gibbons_data, tree)


#--------------------------------------------------------------------------------------------------------
# we gotta look at all the environmental variables in terms of dissimilarity between plots for Mantel Test

aspect <- dist(gibbons_data$aspect)
slope <- dist(gibbons_data$slope)
elevation <- dist(gibbons_data$elevation)
burn_count <- dist(gibbons_data$burn_count)
invasives18 <- dist(gibbons_data$inv_ratio18)
invasives07 <- dist(gibbons_data$inv_ratio07)
canopy18 <- dist(gibbons_data$canopy18)
canopy07 <- dist(gibbons_data$canopy07)
soilindex <- dist(gibbons_data$soil_index)
drainage <- dist(gibbons_data$geo_drainage)




