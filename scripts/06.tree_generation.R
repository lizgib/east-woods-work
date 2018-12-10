#tree 

setwd("~/Documents/GitHub/east_woods_work/")
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/makeMat.R')
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/weldTaxa.R')
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/label.elements.R')
library(ggtree)
dat.all <- read.csv('data/dat.all.csv')

if(!exists('tr.zanne')) tr.zanne <- read.tree('data/phylo.zanne.tre')

dat.all$accepted_name <- gsub('[ ,.] ', '_', dat.all$accepted_name)

tre.ewv1 <- make.matAndTree(tr.zanne, dat.all, name.column = 'accepted_name')

write.csv(tre.ewv1$matrix.renaming, 'outputs/tree_taxa_4.csv')
tre.ewv2 <- weldTaxa(tre.ewv1)
tre.ewv2.pruned <- drop.tip(tre.ewv2, which(!tre.ewv2$tip.label %in% row.names(tre.ewv1$matrix.renaming)))
write.tree(tre.ewv2.pruned, 'outputs/tr.ewv4')

p <- ggtree(tre.ewv2.pruned, layout = 'circular')
p <- p + geom_tiplab(fontface = 'italic', size = 2, aes(angle = angle))
p <- p + geom_label(aes(x = branch), label = tre.ewv2.pruned$node.label, size = 2)
print(p)
