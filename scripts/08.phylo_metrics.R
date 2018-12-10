setwd('~/Documents/GitHub/east_woods_work/')
liz_data <- read.csv('liz_data.csv')
library(picante)
library(ggplot2)
library(ape)

# PHYLOGENETIC DIVERSITY OF EACH PLOT
tr.ewv4 <- read.tree('~/Documents/GitHub/east_woods_work/outputs/tr.ewv4')
# phylo_understory_07 <- ses.pd(dat.mat.understory.07, tr.ewv4, include.root = F)
# phylo_trees_07 <- ses.pd(dat.mat.trees.07, tr.ewv4, include.root = F) 
phylo_all_07 <- ses.pd(dat.mat.all.07, tr.ewv4, include.root = F)

# phylo_understory_18 <- ses.pd(dat.mat.understory.18, tr.ewv4, include.root = F)
# phylo_trees_18 <- ses.pd(dat.mat.trees.18, tr.ewv4, include.root = F)
phylo_all_18 <- ses.pd(dat.mat.all.18, tr.ewv4, include.root = F)

# PHYLO-BETA-DIVERSITY OF EACH PLOT (using MNTD)
# pbd_understory_07 <- ses.mntd(dat.mat.understory.07, cophenetic(tr.ewv4))
# pbd_trees_07 <- ses.mntd(dat.mat.trees.07, cophenetic(tr.ewv4))
pbd_all_07 <- ses.mntd(dat.mat.all.07, cophenetic(tr.ewv4))

# pbd_understory_18 <- ses.mntd(dat.mat.understory.18, cophenetic(tr.ewv4))
# pbd_trees_18 <- ses.mntd(dat.mat.trees.18, cophenetic(tr.ewv4))
pbd_all_18 <- ses.mntd(dat.mat.all.18, cophenetic(tr.ewv4))


# Add these metrics (plot by plot to liz_data and write out)
write.csv(liz_data, 'liz_data.csv')



