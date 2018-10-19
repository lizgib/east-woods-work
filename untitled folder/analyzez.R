# ANALYSIS FALL 2018 

source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/17.envt_data.R')
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/15.falltranslationkey.R')
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/19.1.community_data_matrices.pres_abs.R')
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/20.phylo_dist_matrices.R')
library(picante)
library(ggplot2)
# PHYLOGENETIC DIVERSITY OF EACH PLOT

phylo_understory_07 <- pd(dat.mat.understory.07, tr.ewv4, include.root = F)
phylo_trees_07 <- pd(dat.mat.trees.07, tr.ewv4, include.root = F) 
phylo_all_07 <- pd(dat.mat.all.07, tr.ewv4, include.root = F)

phylo_understory_18 <- pd(dat.mat.understory.18, tr.ewv4, include.root = F)
phylo_trees_18 <- pd(dat.mat.trees.18, tr.ewv4, include.root = F)
phylo_all_18 <- pd(dat.mat.all.18, tr.ewv4, include.root = F)

# PHYLO-BETA-DIVERSITY OF EACH PLOT (using MNTD)

pbd_understory_07 <- mntd(dat.mat.understory.07, cophenetic(tr.ewv4))
pbd_trees_07 <- mntd(dat.mat.trees.07, cophenetic(tr.ewv4))
pbd_all_07 <- mntd(dat.mat.all.07, cophenetic(tr.ewv4))

pbd_understory_18 <- mntd(dat.mat.understory.18, cophenetic(tr.ewv4))
pbd_trees_18 <- mntd(dat.mat.trees.18, cophenetic(tr.ewv4))
pbd_all_18 <- mntd(dat.mat.all.18, cophenetic(tr.ewv4))

# doing burn frequency first because I think all the data is ready for that
liz_data$burn_count <- as.numeric(liz_data$burn_count)
for (c in unique(liz_data$burn_count)){ # go through each of the burn numbers 
  burn_freq <- sum(liz_data$plots[which(liz_data$burn_count == c)])
}
dis_burn <- dist(liz_data$burn_count, method = 'euclidean')

##############
# TREE TYPE
##############
ashelm <- liz_data$plots[which(liz_data$tree_group == "AshElm")]
cherry <- liz_data$plots[which(liz_data$tree_group == "Cherry")]
bwood <- liz_data$plots[which(liz_data$tree_group == "B.wood")]
woak <- liz_data$plots[which(liz_data$tree_group == "WOak")]
buroak <- liz_data$plots[which(liz_data$tree_group == 'BurOak')]
roak <- liz_data$plots[which(liz_data$tree_group == 'ROak')]
maple <-liz_data$plots[which(liz_data$tree_group == 'Maple')]
gash <- liz_data$plots[which(liz_data$tree_group == 'GAsh')]
wnutelm <- liz_data$plots[which(liz_data$tree_group == 'WnutElm')]

ashelm_dat <- sapply(ashelm, function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
ashelm_avg <- mean(ashelm_dat)
cherry_dat <- sapply(cherry, function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
cherry_avg <- mean(cherry_dat)
bwood_dat <- sapply(bwood, function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
bwood_dat <- bwood_dat[which(!is.na(bwood_dat))]
bwood_avg <- mean(bwood_dat)
woak_dat <- sapply(woak, function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
woak_dat <- woak_dat[which(!is.na(woak_dat))]
woak_avg <- mean(woak_dat)
buroak_dat <- sapply(buroak, function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
buroak_dat <- buroak_dat[which(!is.na(buroak_dat))]
buroak_avg <- mean(buroak_dat)
roak_dat <- sapply(roak, function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
roak_dat <- roak_dat[which(!is.na(roak_dat))]
roak_avg <- mean(roak_dat)
maple_dat <- sapply(maple, function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
maple_avg <- mean(maple_dat)
gash_dat <- sapply(gash, function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
gash_avg <- mean(gash_dat)
wnutelm_dat <- sapply(wnutelm, function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
wnutelm_avg <- mean(wnutelm_dat)

average_diversity <- c(ashelm_avg, cherry_avg, bwood_avg, woak_avg, buroak_avg, roak_avg, maple_avg, gash_avg, wnutelm_avg)
average_diversity <- data.frame(average_diversity)
average_diversity$treegrp <- c('ashelm_avg', 'cherry_avg', 'bwood_avg', 'woak_avg', 'buroak_avg', 'roak_avg', 'maple_avg', 'gash_avg', 'wnutelm_avg')
ggplot() +
  aes(average_diversity$treegrp,average_diversity$average_diversity, fill = average_diversity$treegrp) +
  geom_col() +
  ggtitle('Phylogenetic Diversity as Effect of Tree group') +
  xlab('Tree Group') +
  ylab('Average Phylogenetic Diversity')

treegrp_aov <- aov(average_diversity ~ treegrp, data = average_diversity)
tuk_tre <- TukeyHSD(treegrp_aov)

#diagnostic (how many plots are which tree type)
num_group <- as.data.frame(table(liz_data$tree_group))
labels <- num_group$Var1
ggplot() + 
  aes(liz_data$tree_group) +
  geom_histogram(stat = 'count') + 
  xlab('Dominant Tree Group') + 
  ylab('Frequency') +
  ggtitle('Frequency of Plot Dominant Tree Group')

#where/how are they distributed
ggplot() +
  geom_point(aes(liz_data$lon, liz_data$lat, color = liz_data$tree_group)) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  coord_equal()

##################################################################################################################

###########
# CANOPY 
###########

# first... diagnostic stuff
hist(liz_data$canopy_18)
liz_data$canopy_18[which(liz_data$canopy_18 > 200)] <- NA
hist(liz_data$canopy_18)
hist(liz_data$canopy_07)


## how is the canopy distributed around the east woods? 
ggplot() +
  geom_point(aes(liz_data$lon, liz_data$lat, color = liz_data$canopy_07)) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  ggtitle('Canopy 07') +
  coord_equal()

ggplot() +
  geom_point(aes(liz_data$lon, liz_data$lat, color = liz_data$canopy_18)) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  ggtitle('Canopy 18') +
  coord_equal()

# what is canopy's effect on herb density


# what is canopy's effect on herb diversity 
rownames(phylo_understory_07) <- gsub('-', '', rownames(phylo_understory_07))
phylo_understory_07 <- phylo_understory_07[which(rownames(phylo_understory_07) %in% intersect(rownames(phylo_understory_07), liz_data$plots)),]

ggplot() + 
  geom_point(aes(liz_data$canopy_07,phylo_understory_07$PD), color = 'dark blue') + 
  ggtitle('Canopy effect on phylodiversity 07') + 
  xlab('Canopy Cover') + 
  ylab('Plot Phylogenetic Diversity')

ggplot() + 
  geom_point(aes(liz_data$canopy_07,phylo_understory_07$SR), color = 'dark blue') + 
  ggtitle('Canopy effect on Species richness 07') + 
  xlab('Canopy Cover') + 
  ylab('Species richness')

phylo_understory_18 <- phylo_understory_18[which(rownames(phylo_understory_18) %in% intersect(rownames(phylo_understory_18), liz_data$plots)),]

ggplot() + 
  geom_point(aes(liz_data$canopy_18,phylo_understory_18$PD), color = 'dark blue') + 
  ggtitle('Canopy effect on phylodiversity 18') + 
  xlab('Canopy Cover') + 
  ylab('Plot Phylogenetic Diversity')

ggplot() + 
  geom_point(aes(liz_data$canopy_18,phylo_understory_18$SR), color = 'dark blue') + 
  ggtitle('Canopy effect on Species richness 18') + 
  xlab('Canopy Cover') + 
  ylab('Species richness')

liz_data$marlin_canopy[which(liz_data$marlin_canopy > 60)] <- NA
ggplot()+
  geom_point(aes(liz_data$plots, liz_data$canopy_07), color = 'dark blue') +
  geom_point(aes(liz_data$plots, liz_data$canopy_18), color = 'blue') +
  geom_point(aes(liz_data$plots, liz_data$marlin_canopy), color = 'purple') +
  ggtitle('Marlin Canopy vs BA approximation') + 
  xlab('Canopy (2007 and 2018)') + 
  ylab('Marlin Canopy Openness')

ggplot()+
  geom_col(aes(liz_data$plots, liz_data$canopy_18), color = 'blue') +
  geom_col(aes(liz_data$plots, liz_data$canopy_07), color = 'dark blue')+
  ggtitle('Canopy Cover in 2007 and 2018') + 
  xlab('2007') + 
  ylab('2018')





