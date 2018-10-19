library(vegan)
marlin_data <- read.csv('DATA/marlins_data.csv', row.names = 2)
metrics <-  read.csv('OUTPUTS/diversity_metrics.csv', row.names = 1) #instead of reading in csv source in the div_met script

rownames(marlin_data) <- gsub('-', '', rownames(marlin_data))

newdf <- cbind(metrics, marlin_data[rownames(metrics), ])
newdf <- na.omit(newdf)
comm_dat_mat <- read.csv('OUTPUTS/prez_abs_mat_18.csv', row.names = 1)

x <- lm(PD ~ Aspect + Slope + CanopyOpenness + Elevation, newdf)

x <- lm(PD ~ Aspect + Slope + CanopyOpenness + Elevation, newdf)

# library(vegan)
# comm_dat.mds <- metaMDS(comm_dat.mds)
# comm_dat.mds <- metaMDS(comm_dat_mat)
# comm_dat_mat.noNA <- na.omit(comm_dat_mat)
# which(rowSums(comm_dat_mat) == 0)
# comm_dat_mat.noNA <- comm_dat_mat[rowSums(comm_dat_mat) > 0]
# comm_dat_mat.noNA <- comm_dat_mat[rowSums(comm_dat_mat) > 0, ]
# comm_dat.mds <- metaMDS(comm_dat_mat.noNA)
# plot(comm_dat.mds)
# which(comm_dat.mds$points[, 2] > 600)
# which(comm_dat.mds$points[, 1] > 600)
# comm_dat_mat['EE127',]
# which(comm_dat_mat['EE127',] == 1)
# names(comm_dat_mat)[which(comm_dat_mat['EE127',] == 1)]
# which(comm_dat_mat$Phryma_leptostachya == 1)
# which(comm_dat.mds$points[, 1] > 600)
# comm_dat_mat.noNA <- comm_dat_mat.noNA[row.names(comm_dat_mat.noNA) ]
# comm_dat_mat.noNA <- comm_dat_mat.noNA[!row.names(comm_dat_mat.noNA) %in%
# c('EE127', 'HD136')]
# comm_dat_mat.noNA <- comm_dat_mat.noNA[!names(comm_dat_mat.noNA) %in%
# c('EE127', 'HD136')]
# comm_dat.mds <- metaMDS(comm_dat_mat.noNA)
# plot(comm_dat.mds)
# which(comm_dat.mds$points[,1] > 6000)
# row.names(comm_dat_mat)
# comm_dat_mat.noNA <- comm_dat_mat.noNA[which(!row.names(comm_dat_mat.noNA) %in%
# c('EE127', 'HD136'))]
# comm_dat_mat.noNA <- comm_dat_mat.noNA[which(!row.names(comm_dat_mat.noNA) %in%
# c('EE127', 'HD136')),]
# comm_dat.mds <- metaMDS(comm_dat_mat.noNA)
# plot(comm_dat.mds)
# which(comm_dat.mds$points[,1] > 500)
# comm_dat_mat.noNA <- comm_dat_mat.noNA[which(!row.names(comm_dat_mat.noNA) %in%
# c('EE127', 'HD136', 'YY146')),]
# comm_dat.mds <- metaMDS(comm_dat_mat.noNA)
# plot(comm_dat.mds)
# dist(comm_dat.mds$points)
# names(marlin_data)
# marlin_data$Grp
# x <- lm(PD ~ Aspect + Slope + CanopyOpenness + Elevation + Grp, newdf)
# summary(x)
# anova(x)
# summary(x)
# anova(x)
# x <- lm(PD ~ Aspect + Slope + CanopyOpenness + Elevation,  newdf) %>% summary
# summary(x)
# x <- lm(PD ~ Aspect + Slope + CanopyOpenness + Elevation,  newdf)
# summary(x)
# x <- lm(SR ~ Aspect + Slope + CanopyOpenness + Elevation + Grp, newdf)
# summary(x)
# x <- lm(PD ~ Aspect + Slope + CanopyOpenness + Elevation + SR,  newdf)
# summary(x)
# x <- lm(PD ~ Aspect + Slope + CanopyOpenness + Elevation + SR + Grp,  newdf)
# summary(x)
# anova(x)
# x <- lm(PD ~ SR + Grp,  newdf)
# summary(x)
# anova(x)
# marlin_data <- read.csv('DATA/marlins_data.csv', row.names = 2)
