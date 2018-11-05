#tree 

setwd("~/Documents/GitHub/east_woods_work/")
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/makeMat.R')
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/weldTaxa.R')
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/label.elements.R')
source('~/Documents/GitHub/east_woods_work/scripts/05.speciesinfo.R')

if(!exists('tr.zanne')) tr.zanne <- read.tree('phylo.zanne.tre')

dat.all$accepted_name <- gsub('[ ,.] ', '_', dat.all$accepted_name)

tre.ewv1 <- make.matAndTree(tr.zanne, dat.all, name.column = 'accepted_name')

write.csv(tre.ewv1$matrix.renaming, '../outputs/tree_taxa_4.csv')
tre.ewv2 <- weldTaxa(tre.ewv1)
tre.ewv2.pruned <- drop.tip(tre.ewv2, which(!tre.ewv2$tip.label %in% row.names(tre.ewv1$matrix.renaming)))
write.tree(tre.ewv2.pruned, '../outputs/tr.ewv4')

