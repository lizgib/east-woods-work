#phylogenetic distance matrices 
#tells you how phylogenetically dissimilar each plot is from each other

library(picante)
library(vegan)
source('~/Documents/GitHub/east_woods_work/scripts/08.community_data_matrices.pres_abs.R') # gets the dat.mat objects 
source('~/Documents/GitHub/east_woods_work/scripts/09.envt_data.R')  # makes the liz_data object

# trying to make things faster... cophenetic tree to use here 

tree <- cophenetic(tr.ewv4)
dat.mat.all.07 <- read.csv('data/dat.mat.all.07.csv', row.names = 1)
dat.mat.all.18 <- read.csv('data/dat.mat.all.18.csv', row.names = 1)

 ##################################################################################################################
 # will probably move this part to a different script later on... need to filter out which plots are
 # included in the analyses bc there are a number of different "filters" i need to look at
 #            1. the marlin plots --> DONe in this script here 
 #            2. only plots within the east woods
 #            3. only plots with trees in them 
 #            4. plots with greater than 3 species in them 
 
#dat.mat.understory.07 <- dat.mat.understory.07[which(rownames(dat.mat.understory.07) %in% intersect(liz_data$plots, rownames(dat.mat.understory.07))),]
#dat.mat.trees.07 <- dat.mat.trees.07[which(rownames(dat.mat.trees.07) %in% intersect(liz_data$plots, rownames(dat.mat.trees.07))),]
dat.mat.all.07 <- dat.mat.all.07[which(rownames(dat.mat.all.07) %in% intersect(liz_data$plots, rownames(dat.mat.all.07))),]
##################################################################################################################

#pairwise nearest neighbor similarity 

# OKEEEE 11/29 these work but theyre giving me rows with NAs which I dont understand...
# for now im going to try running the mantel test with mean pairwise distance instead of nearest neighbor dissimilarity...

beta_Dpw_all.07 <- comdist(dat.mat.all.07, tree)

beta_Dnn_all.07 <- comdistnt(dat.mat.all.07, tree)

##################################################################################################################

#########
# 2018
########

#dat.mat.understory.18 <- dat.mat.understory.18[which(rownames(dat.mat.understory.18) %in% intersect(liz_data$plots, rownames(dat.mat.understory.18))),]
#dat.mat.trees.18 <- dat.mat.trees.18[which(rownames(dat.mat.trees.18) %in% intersect(liz_data$plots, rownames(dat.mat.trees.18))),]
dat.mat.all.18 <- dat.mat.all.18[which(rownames(dat.mat.all.18) %in% intersect(liz_data$plots, rownames(dat.mat.all.18))),]
##################################################################################################################

#pairwise nearest neighbor similarity 

##################################################################################################################
# Since these metrics dont automatically compute a species richness equivalent, will do so here with 
# Jaccard Distances

#understory_jaccard_07 <- vegdist(dat.mat.understory.07, method = 'jaccard')
#trees_jaccard_07 <- vegdist(dat.mat.trees.07, method = 'jaccard')
all_jaccard_07 <- vegdist(dat.mat.all.07, method = 'jaccard')


#understory_jaccard_18 <- vegdist(dat.mat.understory.18, method = 'jaccard')
#trees_jaccard_18 <- vegdist(dat.mat.trees.18, method = 'jaccard')
all_jaccard_18 <- vegdist(dat.mat.all.18, method = 'jaccard')


##################################################################################################################
# we gotta look at all the environmental variables in terms of dissimilarity between plots

aspect <- dist(liz_data$aspect)
slope <- dist(liz_data$slope)
elevation <- dist(liz_data$elevation)
burn_count <- dist(liz_data$burn_count)
invasives18 <- dist(liz_data$inv_ratio18)
invasives07 <- dist(liz_data$inv_ratio07)
canopy18 <- dist(liz_data$canopy18)
canopy07 <- dist(liz_data$canopy07)
soilindex <- dist(liz_data$soil_index)
drainage <- dist(liz_data$geo_drainage)

row.names(aspect) <- row.names(beta_Dnn_all.07)

# these are all the continuous variables I have so far.. will be looking at expanding this shortly

##################################################################################################################








