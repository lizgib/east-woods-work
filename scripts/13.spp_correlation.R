#species correlation with environmental data

setwd('~/Documents/GitHub/east_woods_work/scripts/')
source('~/Documents/GitHub/east_woods_work/scripts/08.community_data_matrices.pres_abs.R')
source('~/Documents/GitHub/east_woods_work/scripts/11.analyzing_data.R')

library(ggtree)

##############################################################################################################
liz_data <- liz_data[which(liz_data$plots %in% rownames(dat.mat.all.18)),]

all.r.aspect <- apply(dat.mat.all.18, 2, cor, liz_data$aspect)
all.r.slope <- apply(dat.mat.all.18, 2, cor, liz_data$slope)
all.r.elevation <- apply(dat.mat.all.18, 2, cor, liz_data$elevation)
all.r.canopy18 <- apply(dat.mat.all.18, 2, cor, liz_data$canopy_18)
all.r.invasive_ratio18 <- apply(dat.mat.all.18, 2, cor, liz_data$invasive_ratio_18)

all_cor <- data.frame(all.r.aspect)
all_cor$slope <- cbind(all.r.slope)
all_cor$elevation <- cbind(all.r.elevation)
all_cor$canopy18 <- cbind(all.r.canopy18)
all_cor$invasive_ratio18 <- cbind(all.r.invasive_ratio18)

# will continue adding to this as I generate more continous variables 
tree <- ggtree(tr.ewv4)
p <- gheatmap(tree, all_cor, width = 0.25,
              colnames_position = 'bottom',
              colnames_angle = 270,
              font.size = 1.5, hjust = 0)
p

