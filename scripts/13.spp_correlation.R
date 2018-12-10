#species correlation with environmental data

setwd('~/Documents/GitHub/east_woods_work/')
dat.mat.all.07 <- read.csv('data/dat.mat.all.07.csv', row.names = 1)
dat.mat.all.18 <- read.csv('data/dat.mat.all.18.csv', row.names = 1)
liz_data <- read.csv('data/liz_data.csv')
library(ggtree)

##############################################################################################################
dat.mat.all.18 <- dat.mat.all.18[which(rownames(dat.mat.all.18) %in% liz_data$plots),]
liz_data <- liz_data[which(liz_data$plots %in% rownames(dat.mat.all.18)),]
liz_data$burn_count <- as.numeric(as.character(liz_data$burn_count))
liz_data$soil_index <- as.numeric(as.character(liz_data$soil_index))
liz_data$geo_drainage <- as.numeric(as.character(liz_data$geo_drainage))
liz_data$canopy18 <- as.numeric(as.character(liz_data$canopy18))
liz_data$inv_ratio18 <- as.numeric(as.character(liz_data$inv_ratio18))

all.r.burn_count <- apply(dat.mat.all.18, 2, cor, liz_data$burn_count)
all.r.soil_index <- apply(dat.mat.all.18, 2, cor, liz_data$soil_index)
all.r.drainage <- apply(dat.mat.all.18, 2, cor, liz_data$geo_drainage)
all.r.canopy18 <- apply(dat.mat.all.18, 2, cor, liz_data$canopy18)
all.r.invasive_ratio18 <- apply(dat.mat.all.18, 2, cor, liz_data$inv_ratio18)

all_cor <- data.frame(all.r.burn_count)
all_cor$soil_index <- cbind(all.r.soil_index)
all_cor$drainage <- cbind(all.r.drainage)
all_cor$canopy18 <- cbind(all.r.canopy18)
all_cor$invasive_ratio18 <- cbind(all.r.invasive_ratio18)

# will continue adding to this as I generate more continous variables 
tree <- ggtree(tr.ewv4)
p <- gheatmap(tree, all_cor, width = 0.25,
              colnames_position = 'bottom',
              colnames_angle = 270,
              font.size = 1.5, hjust = 0)
p

