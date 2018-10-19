#phylogenetic distance matrices 
#tells you how phylogenetically dissimilar each plot is from each other

library(picante)
setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
source('SCRIPTS/newstuff/01.1.community_data_matrices_pres_abs.R')

herbs_rows <- rownames(dat.mat.herbs[which(rowSums(dat.mat.herbs) > 2),])
shrubs_rows <- rownames(dat.mat.shrubs[which(rowSums(dat.mat.shrubs) > 2),])
trees_rows <- rownames(dat.mat.trees[which(rowSums(dat.mat.trees) > 2),])
herbs_trees_rows <- intersect(herbs_rows, trees_rows)

#pairwise phylogenetic dissimilarity (for presence absence)
beta_Dpw_herbs.pa <- comdist(dat.mat.herbs#[which(rowSums(dat.mat.herbs) > 2),]
                             , cophenetic(tr.ewv3))
beta_Dpw_shrubs.pa <- comdist(dat.mat.shrubs#[which(rowSums(dat.mat.shrubs) > 2),]
                              , cophenetic(tr.ewv3))
beta_Dpw_trees.pa <- comdist(dat.mat.trees#[which(rowSums(dat.mat.trees) > 2),]
                             , cophenetic(tr.ewv3))
herbs_for_trees.pa <- comdist(dat.mat.herbs[which(rownames(dat.mat.herbs) %in% herbs_trees_rows),],
                             cophenetic(tr.ewv3))
trees_for_herbs.pa <- comdist(dat.mat.trees[which(rownames(dat.mat.trees) %in% herbs_trees_rows),],
                             cophenetic(tr.ewv3))

beta_Dpw_all.pa <- comdist(dat.mat.all, cophenetic(tr.ewv3))

#######################################################################################
#i've added some "terminal" metrics here just to see if the significance of environment goes up for 
#the different method (cadotte suggested it would)

#pairwise nearest neighbor similarity 
beta_Dnn_herbs.pa <- comdistnt(dat.mat.herbs#[which(rowSums(dat.mat.herbs) > 2),]
                             , cophenetic(tr.ewv3))
beta_Dnn_shrubs.pa <- comdistnt(dat.mat.shrubs#[which(rowSums(dat.mat.shrubs) > 2),]
                              , cophenetic(tr.ewv3))
beta_Dnn_trees.pa <- comdistnt(dat.mat.trees#[which(rowSums(dat.mat.trees) > 2),]
                             , cophenetic(tr.ewv3))
beta_Dnn_all.pa <- comdistnt(dat.mat.all, cophenetic(tr.ewv3))
########################################################################################################
##these pa graphs look exactly the same as the cover ones ....
########################################################################################################

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
source('SCRIPTS/newstuff/01.2.community_data_matrices_cover.R')
#pairwise phylogenetic dissimilarity (for cover)
beta_Dpw_herbs.c <- comdist(dat.mat.herbs.c,#[which(rownames(dat.mat.herbs) %in% herbs_rows),],
                          cophenetic(tr.ewv3))
#beta_Dpw_shrubs.c <- comdist(dat.mat.shrubs,#[which(rownames(dat.mat.shrubs) %in% shrubs_rows),],
#                          cophenetic(tr.ewv3))
beta_Dpw_trees.c <- comdist(dat.mat.trees.c,#[which(rownames(dat.mat.trees) %in% trees_rows),],
                          cophenetic(tr.ewv3))

herbs_for_trees.c <- comdist(dat.mat.herbs.c[which(rownames(dat.mat.herbs.c) %in% herbs_trees_rows),],
                             cophenetic(tr.ewv3))
trees_for_herbs.c <- comdist(dat.mat.trees.c[which(rownames(dat.mat.trees.c) %in% herbs_trees_rows),],
                            cophenetic(tr.ewv3))

