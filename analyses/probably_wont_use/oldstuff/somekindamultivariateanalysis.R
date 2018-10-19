#ordination script for herbs community matrix (using cover based one)

require(vegan)

source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/newstuff/00.read_data.R')
herbs_cover <- read.csv('OUTPUTS/community_matrices/herbs_matrix_cover.csv', as.is = T, row.names =  1)
 herbs_cover <- na.omit(herbs_cover)
herbs_cover <- herbs_cover[-which(apply(herbs_cover, 1, function(x) sum(x != 0)) <= 2),]
# 
herbs_cover.mds <- metaMDS(herbs_cover)


prcomp(marlin_data[,c('Slope', 'Aspect','Elevation')]) #pca these variables are pretty independent of each other

phylo_d <- read.csv('OUTPUTS/ herbs_mat _metrics.csv')
phylo_d <- phylo_d[which(phylo_d$X %in% rownames(marlin_data)),]
x <-boxplot(phylo_d$PD ~ marlin_data$Grp)

vegdist()
