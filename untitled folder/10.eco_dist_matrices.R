#make ecological dissimilarity matrix 
#calculates how dissimilar plots are simply by the species in them (no phylogenetic context)

library(vegan)
setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
source('SCRIPTS/newstuff/01.1.community_data_matrices_pres_abs.R')
herbs_plots <- rownames(dat.mat.herbs)[which(rowSums(dat.mat.herbs) > 2)]
shrubs_plots <- rownames(dat.mat.shrubs)[which(rowSums(dat.mat.shrubs) > 2)]
trees_plots <- rownames(dat.mat.trees)[which(rowSums(dat.mat.trees) > 2)]

herbs_eco_dist.pa <- vegdist(dat.mat.herbs, method = 'jaccard') #[which(rowSums(dat.mat.herbs) > 2),], method = 'jaccard')
shrubs_eco_dist.pa <- vegdist(dat.mat.shrubs, method = 'jaccard')#[which(rowSums(dat.mat.shrubs) > 2),], method = 'jaccard')
trees_eco_dist.pa <- vegdist(dat.mat.trees, method = 'jaccard')#[which(rowSums(dat.mat.trees) > 2),], method = 'jaccard')
all_eco_dist.pa <- vegdist(dat.mat.all, method = 'jaccard')

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
source('SCRIPTS/newstuff/01.2.community_data_matrices_cover.R')
herbs_eco_dist.c <- vegdist(dat.mat.herbs.c#[which(rownames(dat.mat.herbs.c) %in% herbs_plots),]
                           , method = 'jaccard')
shrubs_eco_dist.c <- vegdist(dat.mat.shrubs.c#[which(rownames(dat.mat.shrubs.c) %in% shrubs_plots),] 
                            , method = 'jaccard')
trees_eco_dist.c <- vegdist(dat.mat.trees.c#[which(rownames(dat.mat.trees.c) %in% trees_plots),]
                            , method = 'jaccard')


