library(picante)
library(ggplot2)
library(ape)


setwd('~/Documents/GitHub/east_woods_work/')
# gibbons_data <- read.csv('data/gibbons_data.csv')
dat.mat.all.07 <- read.csv('data/dat.mat.all.07.csv', row.names = 1)
dat.mat.all.18 <- read.csv('data/dat.mat.all.18.csv', row.names = 1)
dat.mat.understory.07 <- read.csv('data/dat.mat.understory.07.csv', row.names = 1)
dat.mat.understory.18 <- read.csv('data/dat.mat.understory.18.csv', row.names = 1)
dat.mat.trees.07 <- read.csv('data/dat.mat.trees.07.csv', row.names = 1)
dat.mat.trees.18 <- read.csv('data/dat.mat.trees.18.csv', row.names = 1)
tr.ewv4 <- read.tree('outputs/tr.ewv4') 

#--------------------------------------------------------------------------------------------------------
# PHYLOGENETIC DIVERSITY OF EACH PLOT

# phylogenetic diversity of plot canopy (probably not very important I may remove later)
phylo_trees_07 <- ses.pd(dat.mat.trees.07, tr.ewv4, include.root = F)
# phylogenetic diversity of plot understory (this one will probably keep for invasive spp analyses and 
# comparisons between years... see if this is more sensitive to change when excluding the tree layer)
phylo_understory_07 <- ses.pd(dat.mat.understory.07, tr.ewv4, include.root = F)
# phylogenetic diversity of plot (inlcuding trees and understory layer)
phylo_all_07 <- ses.pd(dat.mat.all.07, tr.ewv4, include.root = F)
print('completed 07 PD')

# do the same for 2018
phylo_understory_18 <- ses.pd(dat.mat.understory.18, tr.ewv4, include.root = F)
phylo_trees_18 <- ses.pd(dat.mat.trees.18, tr.ewv4, include.root = F)
phylo_all_18 <- ses.pd(dat.mat.all.18, tr.ewv4, include.root = F)
print('completed 18 PD')

#--------------------------------------------------------------------------------------------------------
# PHYLO-BETA-DIVERSITY OF EACH PLOT (using MNTD)
pbd_understory_07 <- ses.mntd(dat.mat.understory.07, cophenetic(tr.ewv4))
pbd_trees_07 <- ses.mntd(dat.mat.trees.07, cophenetic(tr.ewv4))
pbd_all_07 <- ses.mntd(dat.mat.all.07, cophenetic(tr.ewv4))
print('completed 07 MNTD')

pbd_understory_18 <- ses.mntd(dat.mat.understory.18, cophenetic(tr.ewv4))
pbd_trees_18 <- ses.mntd(dat.mat.trees.18, cophenetic(tr.ewv4))
pbd_all_18 <- ses.mntd(dat.mat.all.18, cophenetic(tr.ewv4))
print('completed 18 MNTD')

#--------------------------------------------------------------------------------------------------------
# Add these metrics (plot by plot to gibbons_data and write out)

gibbons_data$SR07 <- phylo_all_07$ntaxa[match(gibbons_data$plots, phylo_all_07$Plot)]
gibbons_data$PD07 <- phylo_all_07$pd.obs[match(gibbons_data$plots, phylo_all_07$Plot)]
gibbons_data$PDunder07 <- phylo_understory_07$pd.obs[match(gibbons_data$plots, phylo_understory_07$Plot)]
gibbons_data$PDtree07 <- phylo_trees_07$pd.obs[match(gibbons_data$plots, phylo_trees_07$Plot)]
gibbons_data$PBD07 <- pbd_all_07$mntd.obs[match(gibbons_data$plots, phylo_all_07$Plot)]
gibbons_data$PBDunder07 <- pbd_understory_07$mntd.obs[match(gibbons_data$plots, pbd_understory_07$Plot)]
gibbons_data$PBDtree07 <- pbd_trees_07$mntd.obs[match(gibbons_data$plots, pbd_understory_07$Plot)]

gibbons_data$SR18 <- phylo_all_18$ntaxa[match(gibbons_data$plots, phylo_all_18$Plot)]
gibbons_data$PD18 <- phylo_all_18$pd.obs[match(gibbons_data$plots, phylo_all_18$Plot)]
gibbons_data$PDunder18 <- phylo_understory_18$pd.obs[match(gibbons_data$plots, phylo_understory_18$Plot)]
gibbons_data$PDtree18 <- phylo_trees_18$pd.obs[match(gibbons_data$plots, phylo_trees_18$Plot)]
gibbons_data$PBD18 <- pbd_all_18$mntd.obs[match(gibbons_data$plots, phylo_all_18$Plot)]
gibbons_data$PBDunder18 <- pbd_understory_18$mntd.obs[match(gibbons_data$plots, pbd_understory_18$Plot)]
gibbons_data$PBDtree18 <- pbd_trees_18$mntd.obs[match(gibbons_data$plots, pbd_understory_18$Plot)]


write.csv(gibbons_data, 'data/gibbons_data.csv')

#--------------------------------------------------------------------------------------------------------
library(vegan)


sr.07 <- data.frame(row.names(dat.mat.all.07))
sr.07$ntaxa07 <- rowSums(dat.mat.all.07)
sr.07$shannons07 <- diversity(dat.mat.all.07)
rownames(sr.07) <- sr.07$row.names.dat.mat.all.07.
sr.07$row.names.dat.mat.all.07. <- NULL


sr.18 <- data.frame(row.names(dat.mat.all.18))
sr.18$ntaxa18 <- rowSums(dat.mat.all.18)
sr.18$shannons18 <- diversity(dat.mat.all.18)
rownames(sr.18) <- sr.18$row.names.dat.mat.all.18.
sr.18$row.names.dat.mat.all.18. <- NULL

plots.env <- read.csv('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Rollinson/data_processed/point_info_GIS.csv')
plots.env$PlotID <- gsub('-', '', plots.env$PlotID)

diversity <- cbind(sr.18, sr.07)
diversity$lon <- plots.env$lon[match(rownames(diversity) ,plots.env$PlotID)]
diversity$lat <- plots.env$lat[match(rownames(diversity) ,plots.env$PlotID)]

write.csv(diversity, 'data/diversity.csv', quote = F)

ew_plots <- plots.env$PlotID[which(plots.env$wooded== 'East Woods')]  
# -----------------
ggplot(diversity[ew_plots,], aes(x = lon, y = lat, size = ntaxa07)) + 
  geom_point(pch = 21, col = 'chartreuse', bg = 'darkgreen') +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Num Spp (SR) 2007')

ggplot(diversity[ew_plots,], aes(x = lon, y = lat, size = ntaxa18)) + 
  geom_point(pch = 21, col = 'cyan', bg = 'darkcyan') +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Num Spp (SR) 2018')

# -----------------

sr.trees.07 <- data.frame(row.names(dat.mat.trees.07))
sr.trees.07$ntaxatrees07 <- rowSums(dat.mat.trees.07)
sr.trees.07$shannonstrees07 <- diversity(dat.mat.trees.07)
rownames(sr.trees.07) <- sr.trees.07$row.names.dat.mat.trees.07.
sr.trees.07$row.names.dat.mat.trees.07. <- NULL

sr.trees.18 <- data.frame(row.names(dat.mat.trees.18))
sr.trees.18$ntaxatrees18 <- rowSums(dat.mat.trees.18)
sr.trees.18$shannonstrees18 <- diversity(dat.mat.trees.18)
rownames(sr.trees.18) <- sr.trees.18$row.names.dat.mat.trees.18.
sr.trees.18$row.names.dat.mat.trees.18. <- NULL


sr.trees.07$lon <- plots.env$lon[match(rownames(sr.trees.07) ,plots.env$PlotID)]
sr.trees.07$lat <- plots.env$lat[match(rownames(sr.trees.07) ,plots.env$PlotID)]

sr.trees.18$lon <- plots.env$lon[match(rownames(sr.trees.18) ,plots.env$PlotID)]
sr.trees.18$lat <- plots.env$lat[match(rownames(sr.trees.18) ,plots.env$PlotID)]

write.csv(sr.trees.07, 'sr.trees.07.csv')
write.csv(sr.trees.18, 'sr.trees.18.csv')


# -----------------
ggplot(sr.trees.07[ew_plots,], aes(x = lon, y = lat, size = ntaxatrees07)) + 
  geom_point(pch = 21, col = 'chartreuse', bg = 'darkgreen') +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Num Tree Spp (SR) 2007')

ggplot(sr.trees.18[ew_plots,], aes(x = lon, y = lat, size = ntaxatrees18)) + 
  geom_point(pch = 21, col = 'cyan', bg = 'darkcyan') +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Num Tree Spp (SR) 2018')

# -----------------

sr.understory.07 <- data.frame(row.names(dat.mat.understory.07))
sr.understory.07$ntaxaunderstory07 <- rowSums(dat.mat.understory.07)
sr.understory.07$shannonsunderstory07 <- diversity(dat.mat.understory.07)
rownames(sr.understory.07) <- sr.understory.07$row.names.dat.mat.understory.07.
sr.understory.07$row.names.dat.mat.understory.07. <- NULL

sr.understory.18 <- data.frame(row.names(dat.mat.understory.18))
sr.understory.18$ntaxaunderstory18 <- rowSums(dat.mat.understory.18)
sr.understory.18$shannonsunderstory18 <- diversity(dat.mat.understory.18)
rownames(sr.understory.18) <- sr.understory.18$row.names.dat.mat.understory.18.
sr.understory.18$row.names.dat.mat.understory.18. <- NULL


sr.understory.07$lon <- plots.env$lon[match(rownames(sr.understory.07) ,plots.env$PlotID)]
sr.understory.07$lat <- plots.env$lat[match(rownames(sr.understory.07) ,plots.env$PlotID)]

sr.understory.18$lon <- plots.env$lon[match(rownames(sr.understory.18) ,plots.env$PlotID)]
sr.understory.18$lat <- plots.env$lat[match(rownames(sr.understory.18) ,plots.env$PlotID)]

write.csv(sr.understory.07, 'sr.understory.07.csv')
write.csv(sr.understory.18, 'sr.understory.18.csv')

ggplot(sr.understory.07[ew_plots,], aes(x = lon, y = lat, size = ntaxaunderstory07)) + 
  geom_point(pch = 21, col = 'chartreuse', bg = 'darkgreen') +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Num Tree Spp (SR) 2007')

ggplot(sr.understory.18[ew_plots,], aes(x = lon, y = lat, size = ntaxaunderstory18)) + 
  geom_point(pch = 21, col = 'cyan', bg = 'darkcyan') +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Num Tree Spp (SR) 2018')


