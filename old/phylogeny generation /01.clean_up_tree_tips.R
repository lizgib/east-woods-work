#clean up tree tips so they match matrices
library(ape)

setwd('~/Documents/morton arb/east_woods_phylogeny/phylogeny_generation/')
tr.ewv3 <- read.tree('OUTPUTS/tr.ewv3')

tr.ewv3$tip.label <- gsub('_NA', '', tr.ewv3$tip.label)
tr.ewv3$tip.label <- gsub('-', '_', tr.ewv3$tip.label)

write.tree(tr.ewv3, 'OUTPUTS/tr.ewv3.edited')
