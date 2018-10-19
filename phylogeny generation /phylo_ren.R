library(tidyverse)
library(magrittr)
setwd("~/Documents/morton arb/east_woods_phylogeny/data_processing/DATA/")
#########################################################################################
# read in the survey csvs

uncleaned <- data.frame(read.csv('../OUTPUTS/unq_sort_all_18.csv', as.is = T))
trans_key <- data.frame(read.csv('../OUTPUTS/edited_translation_key18.csv', as.is = T))
uncleaned$Accepted_name <- trans_key$Accepted_name[match(uncleaned$Original_name, 
                                                         trans_key$Original_name)]
spp.vect <- gsub(" ", "_", uncleaned$Accepted_name)

#########################################################################################

library(ape)

if(!exists('tr.zanne')) tr.zanne <- read.tree('../phylo.zanne.tre')


tips <- tr.zanne$tip.label
tr.pruned <- drop.tip(tr.zanne, which(!tr.zanne$tip.label %in% spp.vect))
tr.pruned

library(ggtree)
if(length(tr.pruned$node.label) == tr.pruned$Nnode) {
  tr.pruned$node.label <- c(rep(NA, length(tr.pruned$tip.label)), 
                            tr.pruned$node.label)
  tr.pruned$node.label[tr.pruned$node.label == ''] <- NA
}

p <- ggtree(tr.pruned, layout = 'rectangular')
p <- p + geom_tiplab(fontface = 'italic', size = 1)
p <- p + geom_label(aes(x = branch), label = tr.pruned$node.label, size = 2)
print(p)


phylo_ren.tre <- write.tree(tr.pruned, '../../phylogeny_generation/OUTPUTS/phylo_ren.tre')


