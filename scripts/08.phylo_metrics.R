library(picante)
library(ggplot2)
library(ape)
library(vegan)
library(xlsx)
library(tidyr)


setwd('~/Documents/GitHub/east_woods_work/')
data('plots.env')
dat.mat.all.07 <- read.csv('data/Community_Matrix/2007/dat.mat.all.07.csv', row.names = 1)
dat.mat.all.18 <- read.csv('data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)
dat.mat.understory.07 <- read.csv('data/Community_Matrix/2007/dat.mat.understory.07.csv', row.names = 1)
dat.mat.understory.18 <- read.csv('data/Community_Matrix/2018/dat.mat.understory.18.csv', row.names = 1)
dat.mat.trees.07 <- read.csv('data/Community_Matrix/2007/dat.mat.trees.07.csv', row.names = 1)
dat.mat.trees.18 <- read.csv('data/Community_Matrix/2018/dat.mat.trees.18.csv', row.names = 1)
tr.ewv4 <- read.tree('outputs/tr.ew.Spring19') 

#--------------------------------------------------------------------------------------------------------
# PHYLOGENETIC DIVERSITY OF EACH PLOT

# phylogenetic diversity of plot trees 
phylo_trees_07 <- ses.pd(dat.mat.trees.07, tr.ewv4, include.root = F)
# phylogenetic diversity of plot understory
phylo_understory_07 <- ses.pd(dat.mat.understory.07, tr.ewv4, include.root = F)
# phylogenetic diversity of plot (inlcuding trees and understory layer)
phylo_all_07 <- ses.pd(dat.mat.all.07, tr.ewv4, include.root = F)
print('completed 07 PD')

# do the same for 2018
phylo_understory_18 <- ses.pd(dat.mat.understory.18, tr.ewv4, include.root = F)
phylo_trees_18 <- ses.pd(dat.mat.trees.18, tr.ewv4, include.root = F)
phylo_all_18 <- ses.pd(dat.mat.all.18, tr.ewv4, include.root = F)
print('completed 18 PD')


plots.env$SR07 <- phylo_all_07$ntaxa[match(rownames(plots.env), rownames(phylo_all_07))]
plots.env$SR18 <- phylo_all_18$ntaxa[match(rownames(plots.env), rownames(phylo_all_18))]
plots.env$PD07 <- phylo_all_07$pd.obs[match(rownames(plots.env), rownames(phylo_all_07))]
plots.env$PD18 <- phylo_all_18$pd.obs[match(rownames(plots.env), rownames(phylo_all_18))]
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

plots.env$MNTD07 <- pbd_all_07$mntd.obs[match(rownames(plots.env), rownames(phylo_all_07))]
plots.env$MNTD18 <- pbd_all_18$mntd.obs[match( rownames(plots.env), rownames(phylo_all_18))]
#-------------------------------------------------------------------------------------------------------
# PHYLOGENETIC SPECIES VARIANCE 

phyvar07 <- psd(dat.mat.all.07, tr.ewv4)
phyvar18 <- psd(dat.mat.all.18, tr.ewv4)

phyvar_trees_18 <- psd(dat.mat.trees.18, cophenetic(tr.ewv4))
phyvar_trees_07 <- psd(dat.mat.trees.07, cophenetic(tr.ewv4))

phyvar_understory_18 <- psd(dat.mat.understory.18, cophenetic(tr.ewv4))
phyvar_understory_07 <- psd(dat.mat.understory.07, cophenetic(tr.ewv4))

plots.env$PSV07 <- phyvar07$PSV[match(rownames(plots.env), rownames(phyvar07))]
plots.env$PSV18 <- phyvar18$PSV[match(rownames(plots.env), rownames(phyvar18))]
save(plots.env, file = 'data/plots.env.RData')
save(phylo_all_07, phylo_all_18, phylo_understory_07, phylo_understory_18, phylo_trees_07, phylo_trees_18,
     pbd_all_07, pbd_all_18, pbd_understory_07, pbd_understory_18, pbd_trees_07, pbd_trees_18,
     phyvar07, phyvar18, phyvar_understory_07, phyvar_understory_18, phyvar_trees_07, phyvar_trees_18,
     file = 'data/phylogenetic_diversity.RData')
#-------------------------------------------------------------------------------------------------------
# SPECIES RICHNESS 
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

diversity <- cbind(sr.18, sr.07)

diversity$PD07 <- phylo_all_07$pd.obs[match(rownames(diversity), rownames(phylo_all_07))]
diversity$PD18 <- phylo_all_18$pd.obs[match(rownames(diversity), rownames(phylo_all_18))]
diversity$MNTD07 <- pbd_all_07$mntd.obs[match(rownames(diversity), rownames(phylo_all_07))]
diversity$MNTD18 <- pbd_all_18$mntd.obs[match( rownames(diversity), rownames(phylo_all_18))]
diversity$PSV07 <- phyvar07$PSV[match(rownames(diversity), rownames(phyvar07))]
diversity$PSV18 <- phyvar18$PSV[match(rownames(diversity), rownames(phyvar18))]

diversity$lon <- plots.env$lon[match(rownames(diversity), rownames(plots.env))]
diversity$lat <- plots.env$lat[match(rownames(diversity), rownames(plots.env))]

write.xlsx(diversity, file = '/Volumes/GoogleDrive/My Drive/East Woods/URF 2018 Gibbons/Data/diversity.xlsx', sheetName = 'All_Plots')


ew_plots <- plots.env$PlotID[which(plots.env$wooded== 'East Woods')]
# -----------------
png('/Volumes/GoogleDrive/My Drive/East Woods/URF 2018 Gibbons/Data/2007 num spp.png')
ggplot(diversity[ew_plots,], aes(x = lon, y = lat, size = ntaxa07)) +
  geom_point(pch = 21, col = 'chartreuse', bg = 'darkgreen') +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Species Richness 2007')
dev.off()

png('/Volumes/GoogleDrive/My Drive/East Woods/URF 2018 Gibbons/Data/2018 num spp.png')
ggplot(diversity[ew_plots,], aes(x = lon, y = lat, size = ntaxa18)) +
  geom_point(pch = 21, col = 'cyan', bg = 'darkcyan') +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Species Richness 2018')
dev.off()

png('/Volumes/GoogleDrive/My Drive/East Woods/URF 2018 Gibbons/Data/phylo_diversity07.png')
ggplot(diversity[ew_plots,], aes(x = lon, y = lat, size = )) +
  geom_point(pch = 21, col = 'chartreuse', bg = 'darkgreen') +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Phylogenetic Diversity 2007')
dev.off()

png('/Volumes/GoogleDrive/My Drive/East Woods/URF 2018 Gibbons/Data/phylo_diversity18.png')
ggplot(diversity[ew_plots,], aes(x = lon, y = lat, size = )) +
  geom_point(pch = 21, col = 'cyan', bg = 'darkcyan') +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Phylogenetic Diversity 2018')
dev.off()


# -----------------
# SPECIES RICHNESS TREES

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

# not the same number of plots between 07 and 18 
sr.trees.07 <- which(!rownames(sr.trees.07) %in% rownames(sr.trees.18))
temp <- sr.trees.18[which(!rownames(sr.trees.07) %in% rownames(sr.trees.18)),]
temp <- NA
sr.trees.07 <- rbind(sr.trees.07, )



diversity.trees <- cbind(sr.trees.18, sr.trees.07)

diversity.trees$PD07 <- phylo_all_07$pd.obs[match(rownames(diversity.trees), rownames(phylo_all_07))]
diversity.trees$PD18 <- phylo_all_18$pd.obs[match(rownames(diversity.trees), rownames(phylo_all_18))]
diversity.trees$MNTD07 <- pbd_all_07$mntd.obs[match(rownames(diversity.trees), rownames(phylo_all_07))]
diversity.trees$MNTD18 <- pbd_all_18$mntd.obs[match( rownames(diversity.trees), rownames(phylo_all_18))]
diversity.trees$PSV07 <- phyvar07$PSV[match(rownames(diversity.trees), rownames(phyvar07))]
diversity.trees$PSV18 <- phyvar18$PSV[match(rownames(diversity.trees), rownames(phyvar18))]

diversity.trees$lon <- plots.env$lon[match(rownames(diversity.trees), plots.env$PlotID)]
diversity.trees$lat <- plots.env$lat[match(rownames(diversity.trees), plots.env$PlotID)]


write.xlsx(diversity.trees, file = '/Volumes/GoogleDrive/My Drive/East Woods/URF 2018 Gibbons/Data/diversity.xlsx', sheetName = 'Trees', append = T)

# -----------------
# SPECIES RICHNESS UNDERSTORY 

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

diversity.understory <- cbind(sr.understory.18, sr.understory.07)

diversity.understory$PD07 <- phylo_all_07$pd.obs[match(rownames(diversity.understory), rownames(phylo_all_07))]
diversity.understory$PD18 <- phylo_all_18$pd.obs[match(rownames(diversity.understory), rownames(phylo_all_18))]
diversity.understory$MNTD07 <- pbd_all_07$mntd.obs[match(rownames(diversity.understory), rownames(phylo_all_07))]
diversity.understory$MNTD18 <- pbd_all_18$mntd.obs[match( rownames(diversity.understory), rownames(phylo_all_18))]
diversity.understory$PSV07 <- phyvar07$PSV[match(rownames(diversity.understory), rownames(phyvar07))]
diversity.understory$PSV18 <- phyvar18$PSV[match(rownames(diversity.understory), rownames(phyvar18))]

diversity.understory$lon <- plots.env$lon[match(rownames(diversity.understory), plots.env$PlotID)]
diversity.understory$lat <- plots.env$lat[match(rownames(diversity.understory), plots.env$PlotID)]


write.xlsx(diversity.understory, file = '/Volumes/GoogleDrive/My Drive/East Woods/URF 2018 Gibbons/Data/diversity.xlsx', sheetName = 'Understory', append = T)




