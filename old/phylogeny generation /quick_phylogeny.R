library(ape)

setwd('~/Documents/morton arb/east_woods_phylogeny/phylogeny_generation/')

#get the species list --> think I can just use the json accepted names list.. 
#I probably should go back and read just this list into its own dataframe...
#will need to modify it though with the updated names based on what we decide after manually 
#going through the list 
#will use only 18 spp list for preliminary tree?? or will combine 07 and 18? 

if(!exists('tr.zanne')) tr.zanne <-read.tree('/DATA/phylo.zanne.tre')

#plot out the already existing tree without showing the taxa at tips...optional
#plot(tr.zanne, 'fan', show.tip.label = F)

#tree pruning... this is the longer part
#first remove all taxa not in the species list
tr.pruned <- drop.tip(tr.zanne, which(!tr.zanne$tip.label %in% spp.vect))


#not sure about this part... visualizing the tree again
library(ggtree)
if(length(tr.pruned$node.label) == tr.pruned$Nnode) {
  tr.pruned$node.label <- c(rep(NA, length(tr.pruned$tip.label)),
                            tr.pruned$node.label)
  tr.pruned$node.label[tr.pruned$node.label == ''] <- NA
}
p <- ggtree(tr.pruned, layout = 'circular')
p <- p + geom_tiplab(fontface = 'italic', size = 2, aes(angle = angle))
p <- p + geom_label(aes(x = branch), label = tr.pruned$node.label, size = 2)
print(p)

