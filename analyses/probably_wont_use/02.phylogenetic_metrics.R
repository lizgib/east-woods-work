#PHYLOGENETIC METRICS
#calculate the pd, mntd, mpd, and beta diversity for any given community data matrix

library(picante)
setwd('~/Documents/morton arb/east_woods_phylogeny/analyses')

##########################################################################################
f = 'herbs_matrix_cover' #enter which file to open
#open up a matrix 
filetoopen <- paste('~/Documents/morton arb/east_woods_phylogeny/analyses/OUTPUTS/community_matrices/', f, '.csv', sep = '')
dat.mat <- read.csv(filetoopen, as.is = T, row.names = 1)
dat.mat <- na.omit(dat.mat)

#get the tree in here 
tr.ewv3 <- read.tree('../phylogeny_generation/OUTPUTS/tr.ewv3.edited')

#calculate phylogenetic diversity 
phylo_d <- pd(dat.mat, tr.ewv3, include.root = F)
diversity_metrics <- data.frame(phylo_d)

#calculate mntd 
mean_near_taxa <- mntd(dat.mat, cophenetic(tr.ewv3))
s.mean_near_taxa <- ses.mntd(dat.mat, cophenetic(tr.ewv3), null.model = 'taxa.labels', runs = 99, iterations = 100)
diversity_metrics <- cbind(diversity_metrics, s.mean_near_taxa)

#calculate mpd
mean_pair_dist <- mpd(dat.mat, cophenetic(tr.ewv3))
s.mean_pair_dist <- ses.mpd(dat.mat, cophenetic(tr.ewv3), null.model = 'taxa.labels', runs = 99, iterations = 100)
diversity_metrics <- cbind(diversity_metrics, s.mean_pair_dist)

#put it in a csv
newfilename <- paste('OUTPUTS/',f,'_metrics.csv', sep = '')
write.csv(diversity_metrics, newfilename)

##########################################################################################
f = 'herbs__shrubs_matrix_cover' #enter which file to open
#open up a matrix 
filetoopen <- paste('~/Documents/morton arb/east_woods_phylogeny/analyses/OUTPUTS/community_matrices/', f, '.csv', sep = '')
dat.mat <- read.csv(filetoopen, as.is = T, row.names = 1)
dat.mat <- na.omit(dat.mat)

#get the tree in here 
tr.ewv3 <- read.tree('../phylogeny_generation/OUTPUTS/tr.ewv3.edited')

#calculate phylogenetic diversity 
phylo_d <- pd(dat.mat, tr.ewv3, include.root = F)
diversity_metrics <- data.frame(phylo_d)

#calculate mntd 
mean_near_taxa <- mntd(dat.mat, cophenetic(tr.ewv3))
s.mean_near_taxa <- ses.mntd(dat.mat, cophenetic(tr.ewv3), null.model = 'taxa.labels', runs = 99, iterations = 100)
diversity_metrics <- cbind(diversity_metrics, s.mean_near_taxa)

#calculate mpd
mean_pair_dist <- mpd(dat.mat, cophenetic(tr.ewv3))
s.mean_pair_dist <- ses.mpd(dat.mat, cophenetic(tr.ewv3), null.model = 'taxa.labels', runs = 99, iterations = 100)
diversity_metrics <- cbind(diversity_metrics, s.mean_pair_dist)

#put it in a csv
newfilename <- paste('OUTPUTS/',f,'_metrics.csv', sep = '')
write.csv(diversity_metrics, newfilename)

##########################################################################################
f = 'trees_matrix_cover' #enter which file to open
#open up a matrix 
filetoopen <- paste('~/Documents/morton arb/east_woods_phylogeny/analyses/OUTPUTS/community_matrices/', f, '.csv', sep = '')
dat.mat <- read.csv(filetoopen, as.is = T, row.names = 1)
dat.mat <- na.omit(dat.mat)

#get the tree in here 
tr.ewv3 <- read.tree('../phylogeny_generation/OUTPUTS/tr.ewv3.edited')

#calculate phylogenetic diversity 
phylo_d <- pd(dat.mat, tr.ewv3, include.root = F)
diversity_metrics <- data.frame(phylo_d)

#calculate mntd 
mean_near_taxa <- mntd(dat.mat, cophenetic(tr.ewv3))
s.mean_near_taxa <- ses.mntd(dat.mat, cophenetic(tr.ewv3), null.model = 'taxa.labels', runs = 99, iterations = 100)
diversity_metrics <- cbind(diversity_metrics, s.mean_near_taxa)

#calculate mpd
mean_pair_dist <- mpd(dat.mat, cophenetic(tr.ewv3))
s.mean_pair_dist <- ses.mpd(dat.mat, cophenetic(tr.ewv3), null.model = 'taxa.labels', runs = 99, iterations = 100)
diversity_metrics <- cbind(diversity_metrics, s.mean_pair_dist)

#put it in a csv
newfilename <- paste('OUTPUTS/',f,'_metrics.csv', sep = '')
write.csv(diversity_metrics, newfilename)
