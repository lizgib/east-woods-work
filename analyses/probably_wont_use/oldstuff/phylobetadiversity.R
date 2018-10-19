#phylobetadiversity

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
tr.ewv2 <- read.tree('../phylogeny_generation/OUTPUTS/tr.ewv2')
comm_dat_mat <- read.csv('OUTPUTS/prez_abs_mat_18.csv', row.names = 1, as.is = T) #select either pres/abs matrix or cover matrix

#phylobetadiversity (or spp turnover)
#for my gamma (lanscape scale diversity) use the entire spp pool of east woods
gam <- sum(tr.ewv2$edge.length)
#for my alpha use the plot spp pool
PD <- pd(comm_dat_mat, tr.ewv2)
PBD <- c()
for(r in PD[,1]){
  phybet_d <- (gam - r)/r
  PBD <- c(PBD, phybet_d)
}

