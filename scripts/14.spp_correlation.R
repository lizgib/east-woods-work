#species correlation with environmental data

setwd('~/Documents/GitHub/east_woods_work/scripts/')
source('~/Documents/GitHub/east_woods_work/scripts/08.community_data_matrices.pres_abs.R')
source('~/Documents/GitHub/east_woods_work/scripts/11.analyses.R')

##############################################################################################################
liz_data <- liz_data[which(liz_data$plots %in% rownames(dat.mat.all.18)),]

all.r.aspect <- apply(dat.mat.all.18, 2, cor, liz_data$aspect)
all.r.slope <- apply(dat.mat.all.18, 2, cor, liz_data$slope)
all.r.elevation <- apply(dat.mat.all.18, 2, cor, liz_data$elevation)
all.r.canopy <- apply(dat.mat.all.18, 2, cor, liz_data$canopy_18)
all.r.invasive_ratio <- apply(dat.mat.all.18, 2, cor, liz_data$invasive_ratio_18)

all_cor <- data.frame(all.r.aspect)
all_cor$slope <- cbind(all.r.slope)
all_cor$elevation <- cbind(all.r.elevation)
all_cor$canopy <- cbind(all.r.canopy)
all_cor$invasive_ratio <- cbind(all.r.invasive_ratio)

# will continue adding to this as I generate more continous variables 





