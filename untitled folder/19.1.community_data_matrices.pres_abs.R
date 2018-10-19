#COMMUNITY DATA MATRICES PRESENCE ABSENCE
#creates a presense absence based community matrix for herbs, shrubs, and trees for 
# 2007
# 2018 
# 2018 and 2007

source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/15.falltranslationkey.R')
library(ape)
#source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/17.tree_generation.R')
# probably want to just read in the tree instead of sourcing it every time
tr.ewv4 <- read.tree('~/Documents/morton arb/east_woods_phylogeny/OUTPUTS/tr.ewv4')
###################################################################################################

##########
# 2007
##########

#UNDERSTORY

dat <- dat.07[which(dat.07$datset == 'H'),]
dat <- rbind(dat, dat.07[which(dat.07$datset == 'S'),])

vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$species)),
              accname = unique(sort(dat$accepted_name)))
vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                  dimnames = list(vects$plots, vects$accname))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accepted_name'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accepted_name']] <- 1
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv4$tip.label, colnames(dat.mat))
dat.mat.understory.07 <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]

###################################################################################################

# TREES 

dat <- dat.07[which(dat.07$datset == 'T'),]

vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$species)),
              accname = unique(sort(dat$accepted_name)))
vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                  dimnames = list(vects$plots, vects$accname))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accepted_name'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accepted_name']] <- 1
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv4$tip.label, colnames(dat.mat))
dat.mat.trees.07 <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]

###################################################################################################

# ALL SPECIES

dat <- dat.07

vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$species)),
              accname = unique(sort(dat$accepted_name)))
vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                  dimnames = list(vects$plots, vects$accname))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accepted_name'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accepted_name']] <- 1
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv4$tip.label, colnames(dat.mat))
dat.mat.all.07 <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]

######################################################################################################################################################

##########
# 2018
#########

#UNDERSTORY

dat <- dat.18[which(dat.18$datset == 'H'),]
dat <- rbind(dat, dat.18[which(dat.18$datset == 'S'),])

vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$species)),
              accname = unique(sort(dat$accepted_name)))
vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                  dimnames = list(vects$plots, vects$accname))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accepted_name'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accepted_name']] <- 1
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv4$tip.label, colnames(dat.mat))
dat.mat.understory.18 <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]

######################################################################################################################

# TREES 

dat <- dat.18[which(dat.18$datset == 'T'),]

vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$species)),
              accname = unique(sort(dat$accepted_name)))
vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                  dimnames = list(vects$plots, vects$accname))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accepted_name'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accepted_name']] <- 1
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv4$tip.label, colnames(dat.mat))
dat.mat.trees.18 <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]

###################################################################################################

# ALL SPECIES

dat <- dat.18

vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$species)),
              accname = unique(sort(dat$accepted_name)))
vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                  dimnames = list(vects$plots, vects$accname))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accepted_name'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accepted_name']] <- 1
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv4$tip.label, colnames(dat.mat))
dat.mat.all.18 <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]



