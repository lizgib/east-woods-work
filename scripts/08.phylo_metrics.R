setwd('~/Documents/GitHub/east_woods_work/')
liz_data <- read.csv('data/liz_data.csv')
dat.mat.all.07 <- read.csv('data/dat.mat.all.07.csv', row.names = 1)
dat.mat.all.18 <- read.csv('data/dat.mat.all.18.csv', row.names = 1)
library(picante)
library(ggplot2)
library(ape)

# PHYLOGENETIC DIVERSITY OF EACH PLOT
tr.ewv4 <- read.tree('outputs/tr.ewv4') 
# phylo_understory_07 <- ses.pd(dat.mat.understory.07, tr.ewv4, include.root = F)
# phylo_trees_07 <- ses.pd(dat.mat.trees.07, tr.ewv4, include.root = F) 
phylo_all_07 <- ses.pd(dat.mat.all.07, tr.ewv4, include.root = F, runs = 20)
pd07 <- pd(dat.mat.all.07, tr.ewv4)
# phylo_understory_18 <- ses.pd(dat.mat.understory.18, tr.ewv4, include.root = F)
# phylo_trees_18 <- ses.pd(dat.mat.trees.18, tr.ewv4, include.root = F)
phylo_all_18 <- ses.pd(dat.mat.all.18, tr.ewv4, include.root = F, runs = 20)
pd18 <- pd(dat.mat.all.18, tr.ewv4)
# PHYLO-BETA-DIVERSITY OF EACH PLOT (using MNTD)
# pbd_understory_07 <- ses.mntd(dat.mat.understory.07, cophenetic(tr.ewv4))
# pbd_trees_07 <- ses.mntd(dat.mat.trees.07, cophenetic(tr.ewv4))
pbd_all_07 <- ses.mntd(dat.mat.all.07, cophenetic(tr.ewv4), runs = 20)

# pbd_understory_18 <- ses.mntd(dat.mat.understory.18, cophenetic(tr.ewv4))
# pbd_trees_18 <- ses.mntd(dat.mat.trees.18, cophenetic(tr.ewv4))
pbd_all_18 <- ses.mntd(dat.mat.all.18, cophenetic(tr.ewv4), runs = 20)


# ughhh I should be able to do ntaxa for SR but kill me its not working 
sr07 <- as.data.frame(rowSums(dat.mat.all.07))
sr18 <- as.data.frame(rowSums(dat.mat.all.18))


# Add these metrics (plot by plot to liz_data and write out)
#liz_data$SR07 <- phylo_all_07$ntaxa[match(liz_data$plots, phylo_all_07$Plot)]
liz_data$SR07 <- sr07$`rowSums(dat.mat.all.07)`[match(liz_data$plots, rownames(sr07))]
#liz_data$PD07 <- phylo_all_07$pd.obs[match(phylo_all_07, liz_data$plots)]
liz_data$PD07 <- pd07$PD[match(liz_data$plots, rownames(pd07))]
#liz_data$PBD07 <- pbd_all_07$mntd.obs[match(phylo_all_07$Plot, liz_data$plots)]

#liz_data$SR18 <- phylo_all_18$ntaxa[match(liz_data$plots, phylo_all_18$Plot)]
liz_data$SR18 <- sr18$`rowSums(dat.mat.all.18)`[match(liz_data$plots, rownames(sr18))]
liz_data$PD18 <- pd18$PD[match(liz_data$plots, rownames(pd18))]
#liz_data$PD18 <- phylo_all_18$pd.obs[match(liz_data$plots, phylo_all_18$Plot)]
#liz_data$PBD18 <- pbd_all_18$mntd.obs[match(liz_data$plots, phylo_all_18$Plot)]



write.csv(liz_data, 'data/liz_data.csv')



