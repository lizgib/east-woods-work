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

all.r.aspect <- apply(dat.mat.all.18, 2, cor, liz_data$aspect)
all.r.burn_count <-cor(dat.mat.all.18, liz_data$burn_count, use = "complete.obs")
all.r.soil_index <- cor(dat.mat.all.18, liz_data$soil_index, use = "complete.obs")
all.r.drainage <- cor(dat.mat.all.18, liz_data$drainage, use = "complete.obs")
all.r.canopy18 <- cor(dat.mat.all.18, liz_data$canopy18, use = "complete.obs")
all.r.invasive_ratio18 <- cor(dat.mat.all.18, liz_data$inv_ratio18, use = "complete.obs")

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
              low = 'yellow', 
              high = 'mediumblue',
              font.size = 3, hjust = 0)
p

