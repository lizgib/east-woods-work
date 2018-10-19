library(picante)

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
tr.ewv2 <- read.tree('../phylogeny_generation/OUTPUTS/tr.ewv2')
comm_dat_mat <- read.csv('OUTPUTS/prez_abs_mat_18.csv', row.names = 1, as.is = T) #select either pres/abs matrix or cover matrix
cover_dat_mat <- read.csv('OUTPUTS/cover_mat_18.csv', row.names = 1, as.is = T)

#calculate phylogenetic diversity for each plot 
phylo_d <- pd(comm_dat_mat, tr.ewv2, include.root = T) #could be true or false...single spp plots
                                                        #make this weird.. basically asking if we 
                                                        #want to include the branch root with most 
diversity_mat <- data.frame(phylo_d)

#calculate mntd and mpd

#THESE ARENT WORKING BECAUSE THE DATAFRAME HAS MORE SPP THAN THE TREE 
#THE PROBLEM SPP IS Anemonella_thalictroides
#I AM JUST DELETING THIS COLUMN FROM THE DATA FRAME BC I WANT TO DO THE ANALYSIS
#IT WILL PROBABLY JUST HAVE TO BE RENAMED AND ADDED BACK INTO THE TREE BUT THAT  
#WILL BE DONE IN THE TREE GENERATION SCRIPT
cover_dat_mat[,'Anemonella_thalictroides'] <- NULL
comm_dat_mat[,'Anemonella_thalictroides'] <- NULL
dimnames(comm_dat_mat)[[2]] <-gsub('[-.]', '_', dimnames(comm_dat_mat)[[2]])

tr.ewv2$tip.label <- gsub('_NA', '', tr.ewv2$tip.label)
tr.ewv2$tip.label <-gsub('[-.]', '_', tr.ewv2$tip.label)#also here there is a hypenated name 
                                                     #(Symphyotrichum_novae-angliae) in the 
                                                     #tree that was being substituted for a '.' 
                                                     #in the dfs... subbing them both to '_'
tr.ewv2$tip.label[which(tr.ewv2$tip.label == )]
#mean_pair_d <- mpd(cover_dat_mat, cophenetic(tr.ewv2))
dist.tr <- cophenetic(tr.ewv2)
dist.tr <- dist.tr[lower.tri(dist.tr, diag = F)]
mean_pair_d <- mean(dist.tr)

#mean_near_tax_d <- mntd(comm_dat_mat, cophenetic(tr.ewv2))
#this one i still dont have working yet for 
# b <- cophenetic(tr.ewv2)
# diag(b) <- NA
# x <- apply(b, margin = 2, min(b), na.rm = T)
# mean_near_tax_d <- mean()

#diversity_mat$MNTD <- mean_near_tax_d
#diversity_mat$MPD <- mean_pair_d

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
diversity_mat$PBD <- PBD

write.csv(diversity_mat, 'OUTPUTS/diversity_metrics.csv')
#jaccard index (or the number of spp shared between two sites) <- dont think i will use 
#jaccard <- vegdist(comm_dat_mat, 'jaccard')
