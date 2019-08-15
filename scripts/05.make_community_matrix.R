#COMMUNITY DATA MATRICES PRESENCE ABSENCE
#creates a presense absence based community matrix for herbs, shrubs, and trees for 
# 2007
# 2018 

setwd('~/Documents/GitHub/east_woods_work/')
dat.all <- read.csv('data/Species/dat.all.csv')
library(ape)
library(reshape2)
library(vegan)
data("plots.env")
tr.ewv4 <- read.tree('data/Phylogeny/tr.ew.Spring19')
#---------------------------------------------------------------------------------------------------
# FUNCTIONS 
source('scripts/05.community_matrix_function.R')

#---------------------------------------------------------------------------------------------------
# FUNCTION CALL
dat.18 <- dat.all[which(dat.all$year == '2018'),]
dat.07 <- dat.all[which(dat.all$year == '2007'),]
dat.sum.07 <- dat.07[which(dat.07$sample_period == 'SUM'),]

# separate the matrices into understory and trees for 2007 and 2018
dat.sum.understory.18 <- dat.18[which(dat.18$sample_period == 'SUM'),]

dat.07.trees <- dat.07[dat.07$datset == 'T',]
dat.07.understory <-dat.07[which(dat.07$datset == 'H' | dat.07$datset == 'S'),]

dat.sum.understory.07 <- dat.07[which(dat.07.understory$sample_period == 'SUM'),]

dat.18.trees <- dat.18[dat.18$datset == 'T',]
dat.18.understory <- dat.18[which(dat.18$datset == 'H' | dat.18$datset == 'S'),]

dat.sum.18 <- rbind(dat.sum.understory.18, dat.18.trees)

dat.mat.all.07 <- com_mat(dat.07, tr.ewv4)
dat.mat.all.18 <- com_mat(dat.18, tr.ewv4)

dat.mat.understory.18 <- com_mat(dat.18.understory, tr.ewv4) 
dat.mat.trees.18 <- com_mat(dat.18.trees, tr.ewv4)

dat.mat.understory.07 <- com_mat(dat.07.understory, tr.ewv4)
dat.mat.trees.07 <- com_mat(dat.07.trees, tr.ewv4)

com.mat.understory.sum.07 <- com_mat(dat.sum.understory.07, tr.ewv4)
com.mat.understory.sum.18 <- com_mat(dat.sum.understory.18, tr.ewv4)

com.mat.sum.07 <- com_mat(dat.sum.07, tr.ewv4)
com.mat.sum.18 <- com_mat(dat.sum.18, tr.ewv4)

# exclude Hidden Lake, just east woods plots 
ew_plots <- rownames(plots.env)[which(plots.env$wooded == 'East Woods')]
hl_plots <- rownames(plots.env)[which(plots.env$wooded == 'Hidden Lake')]

# East Woods Community Matrix
ew_all_07 <- dat.mat.all.07[which(rownames(dat.mat.all.07) %in% ew_plots),]
ew_all_18 <- dat.mat.all.18[which(rownames(dat.mat.all.18) %in% ew_plots),]

ew_trees_07 <- dat.mat.trees.07[which(rownames(dat.mat.trees.07) %in% ew_plots),]
ew_trees_18 <- dat.mat.trees.18[which(rownames(dat.mat.trees.18) %in% ew_plots),]

ew_under_07 <- dat.mat.understory.07[which(rownames(dat.mat.understory.07) %in% ew_plots),]
ew_under_18 <- dat.mat.understory.18[which(rownames(dat.mat.understory.18) %in% ew_plots),]

# Hidden Lake Community Matrix
hl_all_07 <- dat.mat.all.07[which(rownames(dat.mat.all.07) %in% hl_plots),]
hl_all_18 <- dat.mat.all.18[which(rownames(dat.mat.all.18) %in% hl_plots),]

hl_trees_07 <- dat.mat.trees.07[which(rownames(dat.mat.trees.07) %in% hl_plots),]
hl_trees_18 <- dat.mat.trees.18[which(rownames(dat.mat.trees.18) %in% hl_plots),]

hl_under_07 <- dat.mat.understory.07[which(rownames(dat.mat.understory.07) %in% hl_plots),]
hl_under_18 <- dat.mat.understory.18[which(rownames(dat.mat.understory.18) %in% hl_plots),]

write.csv(com.mat.understory.sum.07, 'data/Community_Matrix/2007/summer.under.07.csv')
write.csv(com.mat.understory.sum.18, 'data/Community_Matrix/2018/summer.under.18.csv')

write.csv(com.mat.sum.07, 'data/Community_Matrix/2007/summer.07.csv')
write.csv(com.mat.sum.18, 'data/Community_Matrix/2018/summer.18.csv')

write.csv(dat.mat.all.07, 'data/Community_Matrix/2007/dat.mat.all.07.csv')
write.csv(dat.mat.all.18, 'data/Community_Matrix/2018/dat.mat.all.18.csv')

write.csv(dat.mat.understory.07, 'data/Community_Matrix/2007/dat.mat.understory.07.csv')
write.csv(dat.mat.understory.18, 'data/Community_Matrix/2018/dat.mat.understory.18.csv')

write.csv(dat.mat.trees.07, 'data/Community_Matrix/2007/dat.mat.trees.07.csv')
write.csv(dat.mat.trees.18, 'data/Community_Matrix/2018/dat.mat.trees.18.csv')

write.csv(ew_all_07, 'data/Community_Matrix/2007/ew.07.csv')
write.csv(ew_all_18, 'data/Community_Matrix/2018/ew.18.csv')

write.csv(ew_trees_07, 'data/Community_Matrix/2007/ew.trees.07.csv')
write.csv(ew_trees_18, 'data/Community_Matrix/2018/ew.trees.18.csv')

write.csv(ew_under_07, 'data/Community_Matrix/2007/ew.understory.07.csv')
write.csv(ew_under_18, 'data/Community_Matrix/2018/ew.understory.18.csv')

write.csv(hl_all_07, 'data/Community_Matrix/2007/hl.07.csv')
write.csv(hl_all_18, 'data/Community_Matrix/2018/hl.18.csv')

write.csv(hl_trees_07, 'data/Community_Matrix/2007/hl.trees.07.csv')
write.csv(hl_trees_18, 'data/Community_Matrix/2018/hl.trees.18.csv')

write.csv(hl_under_07, 'data/Community_Matrix/2007/hl.understory.07.csv')
write.csv(hl_under_18, 'data/Community_Matrix/2018/hl.understory.18.csv')









