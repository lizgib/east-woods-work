#tree 

setwd("~/Documents/GitHub/east_woods_work/")
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/makeMat.R')
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/weldTaxa.R')
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/label.elements.R')
library(ggtree)
dat.all <- read.csv('data/species/dat.all.csv')

if(!exists('tr.zanne')) tr.zanne <- read.tree('data/phylo.zanne.tre')

dat.all <- dat.all[!is.na(dat.all),]
dat.all$accepted_name <- gsub('[ ,.] ', '_', dat.all$accepted_name)

tre.ew <- make.matAndTree(tr.zanne, dat.all, name.column = 'accepted_name')
write.csv(tre.ew$matrix.renaming, 'data/species/tree_taxa.csv')
tre.ew2 <- weldTaxa(tre.ew)
tre.ew2.pruned <- drop.tip(tre.ew2, which(!tre.ew2$tip.label %in% row.names(tre.ew$matrix.renaming)))
write.tree(tre.ew2.pruned, 'outputs/tr.ew.Spring19')


tiff('outputs/tr.ew.Spring19.tiff', units = 'in', height = 10, width = 12, res = 400)
ggtree(tre.ew2.pruned, layout = 'rectangular') + 
  geom_tiplab(fontface = 'italic', size = 0.5)
dev.off()
