#tree 

install.packages('phytools')


setwd("~/Documents/morton arb/east_woods_phylogeny/")
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/makeMat.R')
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/weldTaxa.R')
source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/label.elements.R')


if(!exists('tr.zanne')) tr.zanne <- read.tree('../phylogeny_generation/DATA/phylo.zanne.tre')

spp.vect.trees <- read.csv('../data_processing/OUTPUTS/alltrees.csv', as.is = T)
spp.vect.herbs <- read.csv('../data_processing/OUTPUTS/allherbs.csv', as.is = T)
spp.vect.shrubs <- read.csv('../data_processing/OUTPUTS/allshrubs.csv', as.is = T)
spp.vect <- spp.vect.trees
names(spp.vect) <- names(spp.vect.herbs)
spp.vect <- rbind(spp.vect, spp.vect.herbs)
names(spp.vect.shrubs) <- names(spp.vect.herbs)
spp.vect <- rbind(spp.vect, spp.vect.shrubs)

# spp.vect <- read.csv('../data_processing/OUTPUTS/alltrees.csv', as.is = T)
# spp.vect <- rbind(spp.vect, read.csv('../data_processing/OUTPUTS/allherbs.csv', as.is = T))
# spp.vect <- rbind(spp.vect, read.csv('../data_processing/OUTPUTS/allshrubs.csv', as.is = T))
#spp.vect <- unique(spp.vect$Accepted_name)
spp.vect$accepted_name <- gsub('[ ,.] ', '_', spp.vect$accepted_name)

tre.ewv1 <- make.matAndTree(tr.zanne, spp.vect, name.column = 'accepted_name')

write.csv(tre.ewv1$matrix.renaming, 'tree_taxa_3.csv')
tre.ewv2 <- weldTaxa(tre.ewv1)
tre.ewv2.pruned <- drop.tip(tre.ewv2, which(!tre.ewv2$tip.label %in% row.names(tre.ewv1$matrix.renaming)))
write.tree(tre.ewv2.pruned, 'OUTPUTS/tr.ewv3')

