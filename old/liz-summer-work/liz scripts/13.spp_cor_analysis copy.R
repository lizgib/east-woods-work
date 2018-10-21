#species correlation with environmental data

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/newstuff/01.2.community_data_matrices_cover.R')
source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/newstuff/00.read_data.R')

##############################################################################################################
marlin_data.h <- marlin_data[intersect(rownames(marlin_data), rownames(dat.mat.all)),]
marlin_data.h[is.na(marlin_data.h)] <- 0

all.r.aspect <- apply(dat.mat.all, 2, cor, marlin_data.h$Aspect)
all.r.slope <- apply(dat.mat.all, 2, cor, marlin_data.h$Slope)
all.r.elevation <- apply(dat.mat.all, 2, cor, marlin_data.h$Elevation)
all.r.canopy <- apply(dat.mat.all, 2, cor, marlin_data.h$CanopyOpenness)
all.r.invasive_ratio <- apply(dat.mat.all, 2, cor, marlin_data.h$AlienNativeRatio)


all_cor <- data.frame(all.r.aspect)
all_cor$slope <- cbind(all.r.slope)
all_cor$elevation <- cbind(all.r.elevation)
all_cor$canopy <- cbind(all.r.canopy)
all_cor$invasive_ratio <- cbind(all.r.invasive_ratio)

#write.csv(all_cor, 'amended_spp_cor.csv')

#Herbs

marlin_data.h <- marlin_data[intersect(rownames(marlin_data), rownames(dat.mat.herbs.c)),]
marlin_data.h[is.na(marlin_data.h)] <- 0

herbs.r.aspect <- apply(dat.mat.all, 2, cor, marlin_data.h$Aspect)
herbs.r.slope <- apply(dat.mat.all, 2, cor, marlin_data.h$Slope)
herbs.r.elevation <- apply(dat.mat.all, 2, cor, marlin_data.h$Elevation)
herbs.r.canopy <- apply(dat.mat.all, 2, cor, marlin_data.h$CanopyOpenness)
herbs.r.invasive_ratio <- apply(dat.mat.all, 2, cor, marlin_data.h$AlienNativeRatio)

#why am i getting NAs for correlation????? is it not there? 
herb_cor <- data.frame(herbs.r.aspect)
herb_cor$slope <- cbind(herbs.r.slope)
herb_cor$elevation <- cbind(herbs.r.elevation)
herb_cor$canopy <- cbind(herbs.r.canopy)
herb_cor$invasive_ratio <- cbind(herbs.r.invasive_ratio)


marlin_data.t <- marlin_data[intersect(rownames(marlin_data), rownames(dat.mat.trees.c)),]
trees.r.aspect <- apply(dat.mat.trees.c, 2, cor, marlin_data.t$Aspect)
trees.r.slope <- apply(dat.mat.trees.c, 2, cor, marlin_data.t$Slope)
trees.r.elevation <- apply(dat.mat.trees.c, 2, cor, marlin_data.t$Elevation)
trees.r.canopy <- apply(dat.mat.trees.c, 2, cor, marlin_data.t$CanopyOpenness)
trees.r.invasive_ratio <- apply(dat.mat.trees.c, 2, cor, marlin_data.t$AlienNativeRatio)

tree_cor <- data.frame(trees.r.aspect)
tree_cor$slope <- cbind(trees.r.slope)
tree_cor$elevation <- cbind(trees.r.elevation)
tree_cor$canopy <- cbind(trees.r.canopy)
tree_cor$invasive_ratio <- cbind(trees.r.invasive_ratio)

marlin_data.s <- marlin_data[intersect(rownames(marlin_data), rownames(dat.mat.shrubs.c)),]
shrubs.r.aspect <- apply(dat.mat.shrubs.c, 2, cor, marlin_data.s$Aspect)
shrubs.r.slope <- apply(dat.mat.shrubs.c, 2, cor, marlin_data.s$Slope)
shrubs.r.elevation <- apply(dat.mat.shrubs.c, 2, cor, marlin_data.s$Elevation)
shrubs.r.canopy <- apply(dat.mat.shrubs.c, 2, cor, marlin_data.s$CanopyOpenness)
shrubs.r.invasive_ratio <- apply(dat.mat.shrubs.c, 2, cor, marlin_data.s$AlienNativeRatio)

shrub_cor <- data.frame(shrubs.r.aspect)
shrub_cor$slope <- cbind(shrubs.r.slope)
shrub_cor$elevation <- cbind(shrubs.r.elevation)
shrub_cor$canopy <- cbind(shrubs.r.canopy)
shrub_cor$invasive_ratio <- cbind(shrubs.r.invasive_ratio)


all_cor <- herb_cor
names(herb_cor) <- c('aspect', 'slope', 'elevation', 'canopy', 'invasive_ratio')
names(tree_cor) <- names(herb_cor)
names(shrub_cor) <- names(herb_cor)
all_cor <- rbind(all_cor, tree_cor, shrub_cor)


