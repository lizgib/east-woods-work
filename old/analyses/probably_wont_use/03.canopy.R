#determine affect of canopy of phylodiversity 

source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/newstuff/00.read_data.R')
herbs_metrics <- read.csv('OUTPUTS/ herbs_mat _metrics.csv', as.is = T, row.names = 1)
trees_metrics <- read.csv('OUTPUTS/trees_mat_metrics.csv', as.is = T, row.names = 1)
herbs_metrics_1 <- herbs_metrics[which(rownames(trees_metrics) %in% rownames(herbs_metrics)),]
trees_metrics_1 <- trees_metrics[which(rownames(herbs_metrics) %in% rownames(trees_metrics)),]

#compare herb phylogenetic diversity as a function of tree phylogenetic diversity 
tree_PD_affect_on_herb_PD <- lm(herbs_metrics_1$PD ~ trees_metrics_1$PD)
tPD_on_hPD <- summary(tree_PD_affect_on_herb_PD)
tree_MPD_affect_on_herb_MPD <- lm(herbs_metrics_1$mpd.obs ~ trees_metrics_1$mpd.obs)
tMPD_on_hMPD <- summary(tree_MPD_affect_on_herb_MPD)
tree_MNTD_affect_on_herb_MNTD <- lm(herbs_metrics_1$mntd.obs ~ trees_metrics_1$mntd.obs)
tMNTD_on_hMNTD <- summary(tree_MNTD_affect_on_herb_MNTD)

#compare herb and shrub phylo_d as a function of tree phylogenetic diversity 

#compare herb phylo_d as a fucntion of tree group (from marlin data)
herbs_metrics_2 <- herbs_metrics[which(rownames(marlin_data) %in% rownames(herbs_metrics)),]
trgrp_on_herb_PD <-lm(herbs_metrics_2$PD ~ marlin_data$Grp)
wat <- summary(trgrp_on_herb_PD)

#compare herb phylo_d as a funciton of canopy cover 
trcover_on_herb_PF <- lm(herbs_metrics_2$PD ~ marlin_data$CanopyOpenness)
cov_on_hPD <- summary(trcover_on_herb_PF)

ggplot() + 
  geom_point(aes(trees_metrics_1$PD, herbs_metrics_1$PD)) +
  geom_smooth(method = 'lm') +
  xlab('tree cover') +
  ylab('herb diversity')

cor.test(newdf$PD, newdf$canopy, method = 'pearson')
x <- lm(PD ~ canopy, newdf)
  

