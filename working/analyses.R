# ANALYSIS FALL 2018 

source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/17.envt_data.R')
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/19.1.community_data_matrices.pres_abs.R')
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/20.phylo_dist_matrices.R')
library(picante)
library(ggplot2)
library(ape)
# PHYLOGENETIC DIVERSITY OF EACH PLOT
#tr.ewv4 <- read.tree('~/Documents/morton arb/east_woods_phylogeny/OUTPUTS/tr.ewv4')
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
for (c in unique(liz_data$plots)){ # go through each of the burn numbers 
  burn_freq <- sum(liz_data$burn_count[which(liz_data$plots== c)])
}
dis_burn <- dist(liz_data$burn_count, method = 'euclidean')

##############
# TREE TYPE
##############

# MARLIN
#########
ashelm <- lapply("AshElm", function(x)  liz_data$plots[which(liz_data$tree_group == x)])
cherry <- lapply("Cherry", function(x) liz_data$plots[which(liz_data$tree_group == x)])
bwood <- lapply("B.wood", function(x) liz_data$plots[which(liz_data$tree_group == x)])
woak <- lapply("WOak", function(x) liz_data$plots[which(liz_data$tree_group == x)])
buroak <- lapply('BurOak', function(x) liz_data$plots[which(liz_data$tree_group == x)])
roak <- lapply('ROak', function(x) liz_data$plots[which(liz_data$tree_group == x)])
maple <-lapply('Maple', function(x) liz_data$plots[which(liz_data$tree_group == x)])
gash <- lapply('GAsh', function(x) liz_data$plots[which(liz_data$tree_group == x)])
wnutelm <- lapply('WnutElm', function(x) liz_data$plots[which(liz_data$tree_group == x)])

### ALRIGHT NOT SURE WHAT TIME IT IS BUT 10/27 NOTES:
# so we are having a problem with the types being returned from lapply and sapply 
# right now I am very close to having what I want, a master dataframe with a column
# for 1. plot ID 2. PD  3. tree group 
# from this I will be able to do an anova of groups based on the plot PD levels

ashelm_dat <- sapply(ashelm[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
ashelm_avg <- mean(ashelm_dat)
marlin_groups <- cbind(ashelm[[1]], ashelm_dat, 'ashelm')

cherry_dat <- sapply(cherry[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
cherry_avg <- mean(cherry_dat)
cherry_cols <- cbind(cherry[[1]], cherry_dat, 'cherry')
marlin_groups <- rbind(marlin_groups, cherry_cols)

bwood_dat <- sapply(bwood[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
bwood_dat <- bwood_dat[which(!is.na(bwood_dat))]
bwood_avg <- mean(bwood_dat)
bwood_cols <- cbind(bwood[[1]], bwood_dat, 'bwood')
marlin_groups <- rbind(marlin_groups, bwood_cols)

woak_dat <- sapply(woak[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
woak_dat <- woak_dat[which(!is.na(woak_dat))]
woak_avg <- mean(woak_dat)
woak_cols <- cbind(woak[[1]], woak_dat, 'woak')
marlin_groups <- rbind(marlin_groups, woak_cols)

buroak_dat <- sapply(buroak[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
buroak_dat <- buroak_dat[which(!is.na(buroak_dat))]
buroak_avg <- mean(buroak_dat)
buroak_cols <- cbind(buroak[[1]], buroak_dat, 'buroak')
marlin_groups <- rbind(marlin_groups, buroak_cols)

roak_dat <- sapply(roak[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
roak_dat <- roak_dat[which(!is.na(roak_dat))]
roak_avg <- mean(roak_dat)
roak_cols <- cbind(roak[[1]], roak_dat, 'roak')
marlin_groups <- rbind(marlin_groups, roak_cols)

maple_dat <- sapply(maple[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
maple_avg <- mean(maple_dat)
maple_cols <-cbind(maple[[1]], maple_dat, 'maple')
marlin_groups <- rbind(marlin_groups, maple_cols)

gash_dat <- sapply(gash[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
gash_avg <- mean(gash_dat)
gash_col <- cbind(gash[[1]], gash_dat, 'gash')
marlin_groups <- rbind(marlin_groups, gash_col)

wnutelm_dat <- sapply(wnutelm[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
wnutelm_avg <- mean(wnutelm_dat)
wnutelm_cols <- cbind(wnutelm[[1]], wnutelm_dat, 'wnutelm')
marlin_groups <- rbind(marlin_groups, wnutelm_cols)

average_diversity <- c(ashelm_avg, cherry_avg, bwood_avg, woak_avg, buroak_avg, roak_avg, maple_avg, gash_avg, wnutelm_avg)
average_diversity <- data.frame(average_diversity)
average_diversity$treegrp <- c('ashelm_avg', 'cherry_avg', 'bwood_avg', 'woak_avg', 'buroak_avg', 'roak_avg', 'maple_avg', 'gash_avg', 'wnutelm_avg')
#########


# 2007 
#########


elm <- lapply("Ulmus", function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
ash <- lapply("Fraxinus", function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
bwood <- lapply("Tilia", function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
oak <- lapply('Quercus', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
maple <-lapply('Acer', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
wnut <- lapply('Juglans', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
hickory <- lapply('Carya', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
cherry <- lapply('Prunus', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
locust <- lapply('Robinia', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
mulberry <- lapply('Morus', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
birch <- lapply('Ostrya', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
willow <- lapply('Salix', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
poplar <- lapply('Populus', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
spruce <- lapply('Picea', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])
pine <- lapply('Pinus', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)]) 
fir <- lapply('Pseudotsuga', function(x)  liz_data$plots[which(liz_data$tree_group_07 == x)])

#### NOTE 10/24 1:23 AM  GO BACK AND SEE THAT YOU ACCOUNTED FOR ALL GENUSES... I KNOW YOU DIDNT BUT WE NEED TO COME UP WITH A WAY OF DOING THIS.
## MAYBE WILL JUST HAVE TO USE ALL OF THE GENUSES ? 

# get all the PD of each plot with given tree genus
#####

elm_dat <- sapply(elm[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
elm_avg <- mean(elm_dat)
groups2007 <- cbind(elm[[1]], elm_dat, 'elm')

ash_dat <- sapply(ash[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
ash_avg <- mean(ash_dat)
ash_cols <- cbind(ash[[1]], ash_dat, 'ash')
groups2007 <- rbind(groups2007, ash_cols)

bwood_dat <- sapply(bwood[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
bwood_dat <- bwood_dat[which(!is.na(bwood_dat))]
bwood_avg <- mean(bwood_dat)
bwood_cols <- cbind(bwood[[1]], bwood_dat, 'bwood')
groups2007 <- rbind(groups2007, bwood_cols)

oak_dat <- sapply(oak[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
oak_dat <- oak_dat[which(!is.na(oak_dat))] # had to add these in for some reason ... I was getting NAs for IDed plots??
oak_avg <- mean(oak_dat)
oak_cols <- cbind(oak[[1]], oak_dat, 'oak')
groups2007 <- rbind(groups2007, oak_cols)

maple_dat <- sapply(maple[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
maple_avg <- mean(maple_dat)
maple_cols <- NULL
maple_cols <- cbind(maple[[1]], maple_dat, 'maple')
groups2007 <- rbind(groups2007, maple_cols)

wnut_dat <- sapply(wnut[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
wnut_avg <- mean(wnut_dat)
wnut_cols <- cbind(wnut[[1]], wnut_dat, 'wnut')
groups2007 <- rbind(groups2007, wnut_cols)

hickory_dat <- sapply(hickory[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
hickory_avg <- mean(hickory_dat)
hickory_cols <- cbind(hickory[[1]], hickory_dat, 'hickory')
groups2007 <- rbind(groups2007, hickory_cols)

cherry_dat <- sapply(cherry[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
cherry_avg <- mean(cherry_dat)
cherry_cols <- NULL
cherry_cols <- cbind(cherry[[1]], cherry_dat, 'cherry')
groups2007 <- rbind(groups2007, cherry_cols)

locust_dat <- sapply(locust[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
locust_avg <- mean(locust_dat)
locust_cols <- cbind(locust[[1]], locust_dat, 'locust')
groups2007 <- rbind(groups2007, locust_cols)

mulberry_dat <- sapply(mulberry[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
mulberry_avg <- mean(mulberry_dat)
mulberry_cols <- cbind(mulberry[[1]], mulberry_dat, 'mulberry')
groups2007 <- rbind(groups2007, mulberry_cols)

birch_dat <- sapply(birch[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
birch_avg <- mean(birch_dat)
birch_cols <- cbind(birch[[1]], birch_dat, 'birch')
groups2007 <- rbind(groups2007, birch_cols)

willow_dat <- sapply(willow[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
willow_avg <- mean(willow_dat)
willow_cols <- cbind(willow[[1]], willow_dat, 'willow')
groups2007 <- rbind(groups2007, willow_cols)

poplar_dat <- sapply(poplar[[1]], function(x) phylo_understory_07$PD[which(rownames(phylo_understory_07) == x)])
poplar_avg <- mean(poplar_dat)
poplar_cols <- cbind(poplar[[1]], poplar_dat, 'poplar')
groups2007 <- rbind(groups2007, poplar_cols)
groups2007 <- data.frame(groups2007)

#conifer_dat <- sapply(c('Picea', 'Pinus', 'Pseudotsuga'), function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
#conifer_avg <- mean(conifer_dat) # THIS IS PROBLEMATIC MAYABE WILL HAVE TO EVALUATE THESE GENUSES ONO THEIR OWN 
######

# ITS LATE SO IM JUST MAKING THIS AS ITS OWN DATAFRAME INSTEAD OF APPENDING TO THE ORIGINAL AVERAGE DIVERSITY DATAFRAME LIKE I WANTED TO
# THEY JUST HAVE DIFFERENT NUMBER OF ROWS/SAMPLES SO I CANT CBIND THEM NICELY HAVE TO ADD NAS TO MARLIN MAYBE??

average_diversity07 <- c(elm_avg, ash_avg, bwood_avg, oak_avg, maple_avg, wnut_avg, hickory_avg, cherry_avg, locust_avg, mulberry_avg, birch_avg, willow_avg, poplar_avg)
average_diversity07 <- data.frame(average_diversity07)
rownames(average_diversity07) <- c('elm_avg', 'ash_avg', 'bwood_avg', 'oak_avg', 'maple_avg', 'wnut_avg', 
                                  'hickory_avg', 'cherry_avg','locust_avg', 'mulberry_avg', 'birch_avg', 'willow_avg', 'poplar_avg')

# 2018 
###########


elm <- lapply("Ulmus", function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
ash <- lapply("Fraxinus", function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
bwood <- lapply("Tilia", function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
oak <- lapply('Quercus', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
maple <-lapply('Acer', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
wnut <- lapply('Juglans', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
hickory <- lapply('Carya', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
cherry <- lapply('Prunus', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
locust <- lapply('Robinia', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
mulberry <- lapply('Morus', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
birch <- lapply('Ostrya', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
willow <- lapply('Salix', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
poplar <- lapply('Populus', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
spruce <- lapply('Picea', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])
pine <- lapply('Pinus', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)]) 
fir <- lapply('Pseudotsuga', function(x)  liz_data$plots[which(liz_data$tree_group_18 == x)])

# get all the PD of each plot with given tree genus
#####

elm_dat <- sapply(elm[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
elm_avg <- mean(elm_dat)
groups2018 <- cbind(elm[[1]], elm_dat, 'elm')

ash_dat <- sapply(ash[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
ash_avg <- mean(ash_dat)
ash_cols <- NULL
ash_cols <- cbind(ash[[1]], ash_dat, 'ash')
groups2018 <- rbind(groups2018, ash_cols)

bwood_dat <- sapply(bwood[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
bwood_dat <- bwood_dat[which(!is.na(bwood_dat))]
bwood_avg <- mean(bwood_dat)
bwood_cols <- NULL
bwood_cols <- cbind(bwood[[1]], bwood_dat, 'bwood')
groups2018 <- rbind(groups2018, bwood_cols)

oak_dat <- sapply(oak[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
oak_dat <- oak_dat[which(!is.na(oak_dat))] # had to add these in for some reason ... I was getting NAs for IDed plots??
oak_avg <- mean(oak_dat)
oak_cols <- NULL
oak_cols <- cbind(oak[[1]], oak_dat, 'oak')
groups2018 <- rbind(groups2018, oak_cols)

maple_dat <- sapply(maple[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
maple_avg <- mean(maple_dat)
maple_cols <- NULL
maple_cols <- cbind(maple[[1]], maple_dat, 'maple')
groups2018 <- rbind(groups2018, maple_cols)

wnut_dat <- sapply(wnut[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
wnut_avg <- mean(wnut_dat)
wnut_cols <- NULL
wnut_cols <- cbind(wnut[[1]], wnut_dat, 'wnut')
groups2018 <- rbind(groups2018, wnut_cols)

hickory_dat <- sapply(hickory[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
hickory_avg <- mean(hickory_dat)
hickory_cols <- NULL
hickory_cols <- cbind(hickory[[1]], hickory_dat, 'hickory')
groups2018 <- rbind(groups2018, hickory_cols)

cherry_dat <- sapply(cherry[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
cherry_avg <- mean(cherry_dat)
cherry_cols <- NULL
cherry_cols <- cbind(cherry[[1]], cherry_dat, 'cherry')
groups2018 <- rbind(groups2018, cherry_cols)

locust_dat <- sapply(locust[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
locust_avg <- mean(locust_dat)
locust_cols <- NULL
locust_cols <- cbind(locust[[1]], locust_dat, 'locust')
groups2018 <- rbind(groups2018, locust_cols)

mulberry_dat <- sapply(mulberry[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
mulberry_avg <- mean(mulberry_dat)
mulberry_cols <- NULL
mulberry_cols <- cbind(mulberry[[1]], mulberry_dat, 'mulberry')
groups2018 <- rbind(groups2018, mulberry_cols)

birch_dat <- sapply(birch[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
birch_avg <- mean(birch_dat)
birch_cols <- NULL
birch_cols <- cbind(birch[[1]], birch_dat, 'birch')
groups2018 <- rbind(groups2018, birch_cols)

willow_dat <- sapply(willow[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
willow_avg <- mean(willow_dat)
willow_cols <- NULL
willow_cols <- cbind(willow[[1]], willow_dat, 'willow')
groups2018 <- rbind(groups2018, willow_cols)

poplar_dat <- sapply(poplar[[1]], function(x) phylo_understory_18$PD[which(rownames(phylo_understory_18) == x)])
poplar_avg <- mean(poplar_dat)
poplar_cols <- NULL
poplar_cols <- cbind(poplar[[1]], poplar_dat, 'poplar')
groups2018 <- rbind(groups2018, poplar_cols)
groups2018 <- data.frame(groups2018)

average_diversity18 <- c(elm_avg, ash_avg, bwood_avg, oak_avg, maple_avg, wnut_avg, hickory_avg, cherry_avg, locust_avg, mulberry_avg, birch_avg, willow_avg, poplar_avg)
average_diversity18 <- data.frame(average_diversity18)
rownames(average_diversity18) <- c('elm_avg', 'ash_avg', 'bwood_avg', 'oak_avg', 'maple_avg', 'wnut_avg', 
                                   'hickory_avg', 'cherry_avg','locust_avg', 'mulberry_avg', 'birch_avg', 'willow_avg', 'poplar_avg')

###########
# ANOVA 
names(groups2007) <- c('plotID', 'PD', 'tree_group')
names(groups2018) <- c('plotID', 'PD', 'tree_group')

fit07 <- aov(PD ~ tree_group, data = groups2007)
sig07 <- TukeyHSD(fit07) # 3:52 PM 10/27 this isnt working for some reason.. havent investigated 
                         # but it looks like something with numeric data?? 

fit18 <- aov(PD ~ tree_group, data = groups2018)
sig18 <- TukeyHSD(fit18)

###########
# PLOTS 
# average phylogenetic diversity as function of plot tree group
ggplot() +
  aes(average_diversity$treegrp,average_diversity$average_diversity, fill = average_diversity$treegrp) +
  geom_col() +
  ggtitle('Phylogenetic Diversity as Effect of Tree group Marlin') +
  xlab('Tree Group') +
  ylab('Average Phylogenetic Diversity')

treegrp_aov <- aov(average_diversity ~ treegrp, data = average_diversity)
tuk_tre <- TukeyHSD(treegrp_aov)

ggplot() +
  aes(average_diversity2$average_diversity2, average_diversity2$average_diversity2, fill = average_diversity2$treegrp07) +
  geom_col() +
  ggtitle('Phylogenetic Diversity as Effect of Tree group 2007') +
  xlab('Tree Group') +
  ylab('Average Phylogenetic Diversity')

#diagnostic (how many plots are which tree type)
num_group <- as.data.frame(table(liz_data$tree_group))
labels <- num_group$Var1
ggplot() + 
  aes(liz_data$tree_group) +
  geom_histogram(stat = 'count') + 
  xlab('Dominant Tree Group') + 
  ylab('Frequency') +
  ggtitle('Frequency of Plot Dominant Tree Group Marlin')

freq_07 <- as.data.frame(table(liz_data$tree_group_07))
labels <-freq_07$Var1
ggplot() + 
  aes(liz_data$tree_group_07) +
  geom_histogram(stat = 'count') + 
  xlab('Dominant Tree Group') + 
  ylab('Frequency') +
  ggtitle('Frequency of Plot Dominant Tree Group 2007')

freq_18 <- as.data.frame(table(liz_data$tree_group_18))
labels <-freq_18$Var1
ggplot() + 
  aes(liz_data$tree_group_18) +
  geom_histogram(stat = 'count') + 
  xlab('Dominant Tree Group') + 
  ylab('Frequency') +
  ggtitle('Frequency of Plot Dominant Tree Group 2018')

#where/how are they distributed
ggplot() +
  geom_point(aes(liz_data$lon, liz_data$lat, color = liz_data$tree_group)) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  coord_equal() + 
  ggtitle('Marlin Dominant Tree Group')
# 2007
ggplot() +
  geom_point(aes(liz_data$lon, liz_data$lat, color = liz_data$tree_group_07)) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  coord_equal() + 
  ggtitle('Dominant Tree Group 2007')
# 2018
ggplot() +
  geom_point(aes(liz_data$lon, liz_data$lat, color = liz_data$tree_group_18)) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  coord_equal() + 
  ggtitle('Dominant Tree Group 2018')

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

##################################################################################################################

#############
# INVASIVES  
#############

# make plot of invasive ratio distribution (where are invasives the thickest in EW)
# make plot of invasive distribution 2007 
ggplot() +
  geom_point(aes(liz_data$lon, liz_data$lat, color = liz_data$invasive_ratio_07)) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  ggtitle('Invasive Species Distribution 2007') +
  coord_equal()

# make plot of invasive distribution in 2018
ggplot() +
  geom_point(aes(liz_data$lon, liz_data$lat, color = liz_data$invasive_ratio_18)) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  ggtitle('Invasive Species Distribution 2018') +
  coord_equal()


##---------------------------------------------------------------------------------------------------------- 

## AS OF 10/28 8:03 PM I HAVE MADE THESE FIGURES BUT I SUSPECT THE RATIO IS INCORRECT... WILL FIX/REDO!!

##---------------------------------------------------------------------------------------------------------- 

# PLOT THE DISTRIBUTION OF SPECIFIC INVASIVES IN THE EW 

# Lonicera (2007) 
lonicera07 <- dat.07[grep('Lonicera', dat.07$accepted_name),]
lonicera_cover <- data.frame()
lonicera07$cover <- as.numeric(lonicera07$cover)
for (plt in unique(lonicera07$plot)){
  temp <- sum(lonicera07$cover[which(lonicera07$plot == plt)])
  lat <- liz_data$lat[which(liz_data$plots == plt)]
  lon <- liz_data$lon[which(liz_data$plots == plt)]
  cols <- cbind(plt, temp, lat, lon)
  lonicera_cover <- rbind(lonicera_cover, cols)
}

ggplot() +
  geom_point(aes(lonicera_cover$lon, lonicera_cover$lat), 
             color = lonicera_cover$temp) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  ggtitle('Lonicera Distribution 2007') +
  coord_equal()

# Lonicera (2018)
lonicera18 <- dat.18[grep('Lonicera', dat.18$accepted_name),]
lonicera_cover <- data.frame()
lonicera18$cover <- as.numeric(lonicera18$cover)
for (plt in unique(lonicera18$plot)){
  temp <- sum(lonicera18$cover[which(lonicera18$plot == plt)])
  cols <- cbind(plt, temp)
  lonicera_cover <- rbind(lonicera_cover, cols)
}

ggplot() +
  geom_point(aes(liz_data$lon[which(liz_data$plots %in% lonicera_cover$plt)], liz_data$lat[which(liz_data$plots %in% lonicera_cover$plt)])
             , color = lonicera_cover$temp) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  ggtitle('Lonicera Distribution 2018') +
  coord_equal()


# Alliara Petiolata (2007)
alliaria07 <- dat.07[grep('Alliaria', dat.07$accepted_name),]
alliaria_cover <- data.frame()
alliaria07$cover <- as.numeric(alliaria07$cover)
for (plt in unique(alliaria07$plot)){
  temp <- sum(alliaria07$cover[which(alliaria07$plot == plt)])
  cols <- cbind(plt, temp)
  alliaria_cover <- rbind(alliaria_cover, cols)
}
alliaria_cover <- as.numeric(alliaria_cover)
ggplot() +
  geom_point(aes(liz_data$lon[which(liz_data$plots %in% alliaria07$plot)], liz_data$lat[which(liz_data$plots %in% alliaria07$plot)]), color = alliaria_cover) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  ggtitle('Alliaria Distribution 2007') +
  coord_equal()

# Alliara Petiolata (2018)
alliaria18 <- dat.18[grep('Alliaria', dat.18$accepted_name),]
alliaria_cover <- data.frame()
alliaria18$cover <- as.numeric(alliaria18$cover)
for (plt in unique(alliaria18$plot)){
  temp <- sum(alliaria18$cover[which(alliaria18$plot == plt)])
  cols <- cbind(plt, temp)
  alliaria_cover <- rbind(alliaria_cover, cols)
}
alliaria18 <- data.frame(unique(alliaria18$plot))
alliaria_cover <- as.numeric(alliaria_cover)
ggplot() +
  geom_point(aes(liz_data$lon[which(liz_data$plots %in% alliaria18$plot)], liz_data$lat[which(liz_data$plots %in% alliaria18$plot)]), color = alliaria_cover) +
  ylab('Latitude') + 
  xlab('Longitude') + 
  scale_size () + 
  ggtitle('Alliaria Distribution 2018') +
  coord_equal()

# Rhamnus Cathartica (2007)


# Rhamuns Cathartica (2018)


# Rosa Multiflora (2007) 


# Rosa Multiflora (2018) 











