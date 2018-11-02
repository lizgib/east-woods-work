#tree 

setwd("~/Documents/morton arb/east_woods_phylogeny/")
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/makeMat.R')
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/weldTaxa.R')
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/label.elements.R')
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/16.speciesinfo.R')

if(!exists('tr.zanne')) tr.zanne <- read.tree('../DATA/phylo.zanne.tre')

dat.all$accepted_name <- gsub('[ ,.] ', '_', dat.all$accepted_name)

tre.ewv1 <- make.matAndTree(tr.zanne, dat.all, name.column = 'accepted_name')

write.csv(tre.ewv1$matrix.renaming, 'tree_taxa_3.csv')
tre.ewv2 <- weldTaxa(tre.ewv1)
tre.ewv2.pruned <- drop.tip(tre.ewv2, which(!tre.ewv2$tip.label %in% row.names(tre.ewv1$matrix.renaming)))
write.tree(tre.ewv2.pruned, '../OUTPUTS/tr.ewv4')

