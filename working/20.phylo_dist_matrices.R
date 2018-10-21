#phylogenetic distance matrices 
#tells you how phylogenetically dissimilar each plot is from each other

library(picante)
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/19.1.community_data_matrices.pres_abs.R')
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/17.envt_data.R')
 ##################################################################################################################
 # will probably move this part to a different script later on... need to filter out which plots are
 # included in the analyses bc there are a number of different "filters" i need to look at
 #            1. the marlin plots --> DONe in this script here 
 #            2. only plots within the east woods
 #            3. only plots with trees in them 
 #            4. plots with greater than 3 species in them 
 
dat.mat.understory.07 <- dat.mat.understory.07[which(rownames(dat.mat.understory.07) %in% intersect(liz_data$plots, rownames(dat.mat.understory.07))),]
dat.mat.trees.07 <- dat.mat.trees.07[which(rownames(dat.mat.trees.07) %in% intersect(liz_data$plots, rownames(dat.mat.trees.07))),]
dat.mat.all.07 <- dat.mat.all.07[which(rownames(dat.mat.all.07) %in% intersect(liz_data$plots, rownames(dat.mat.all.07))),]
##################################################################################################################

#pairwise phylogenetic dissimilarity (for presence absence)
beta_Dpw_herbs.pa.07 <- comdist(dat.mat.understory.07, cophenetic(tr.ewv4))

beta_Dpw_trees.pa.07 <- comdist(dat.mat.trees.07, cophenetic(tr.ewv4))

beta_Dpw_all.pa.07 <- comdist(dat.mat.all.07, cophenetic(tr.ewv4))

##################################################################################################################
#i've added some "terminal" metrics here just to see if the significance of environment goes up for 
#the different method (cadotte suggested it would)

#pairwise nearest neighbor similarity 
beta_Dnn_herbs.pa.07 <- comdistnt(dat.mat.understory.07, cophenetic(tr.ewv4))

beta_Dnn_trees.pa.07 <- comdistnt(dat.mat.trees.07, cophenetic(tr.ewv4))

beta_Dnn_all.pa.07 <- comdistnt(dat.mat.all.07, cophenetic(tr.ewv4))

##################################################################################################################

#########
# 2018
########


dat.mat.understory.18 <- dat.mat.understory.18[which(rownames(dat.mat.understory.18) %in% intersect(liz_data$plots, rownames(dat.mat.understory.18))),]
dat.mat.trees.18 <- dat.mat.trees.18[which(rownames(dat.mat.trees.18) %in% intersect(liz_data$plots, rownames(dat.mat.trees.18))),]
dat.mat.all.18 <- dat.mat.all.18[which(rownames(dat.mat.all.18) %in% intersect(liz_data$plots, rownames(dat.mat.all.18))),]
##################################################################################################################

#pairwise phylogenetic dissimilarity (for presence absence)
beta_Dpw_herbs.pa.18 <- comdist(dat.mat.understory.18, cophenetic(tr.ewv4))

beta_Dpw_trees.pa.18 <- comdist(dat.mat.trees.18, cophenetic(tr.ewv4))

beta_Dpw_all.pa.18 <- comdist(dat.mat.all.18, cophenetic(tr.ewv4))

##################################################################################################################
#i've added some "terminal" metrics here just to see if the significance of environment goes up for 
#the different method (cadotte suggested it would)

#pairwise nearest neighbor similarity 
beta_Dnn_herbs.pa.18 <- comdistnt(dat.mat.understory.18, cophenetic(tr.ewv4))

beta_Dnn_trees.pa.18 <- comdistnt(dat.mat.trees.18, cophenetic(tr.ewv4))

beta_Dnn_all.pa.18 <- comdistnt(dat.mat.all.18, cophenetic(tr.ewv4))



