#COMMUNITY DATA MATRICES PRESENCE ABSENCE
#creates a presense absence based community matrix for herbs, shrubs, and trees for 
# 2007
# 2018 

setwd('~/Documents/GitHub/east_woods_work/')

# these boys arent working for some reason I think my dat.all file is getting changed as I write it 
# in and out 
# liz_data <- read.csv('data/liz_data.csv')
# dat.all <- read.csv('data/dat.all.csv')

# gonna use source for this script because im having problems :( 
source('~/Documents/GitHub/east_woods_work/scripts/09.envt_data.R')
source('~/Documents/GitHub/east_woods_work/scripts/06.cover.R')


library(ape)
library(reshape2)
library(vegan)
#source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/17.tree_generation.R')
# probably want to just read in the tree instead of sourcing it every time
tr.ewv4 <- read.tree('~/Documents/GitHub/east_woods_work/outputs/tr.ewv4')
###################################################################################################
# this step will prevent problems later on when using the community matrix
# GET RID OF ANY PLOTS THAT AREN'T IN LIZ_DATA

# dat.07 <- dat.07[which(dat.07$plot %in% liz_data$plots),]
# dat.18 <- dat.18[which(dat.18$plot %in% liz_data$plots),]
# liz_data <- liz_data[which(liz_data$plots %in% dat.07$plot),] # this shouldnt get rid of anything but im 
#                                                               # doing it just to be sure 

###################################################################################################

##########
# 2007
##########

dat.mat <- matrix(0, length(unique(dat.07$plot)), length(unique(dat.07$accepted)),
                  dimnames = list(unique(dat.07$plots), unique(dat.07$accepted_name)))
for(sp in unique(dat.07$accepted_name)){
  print(sp)
  for(plt in unique(dat.07$plot)){
    print(plt)
    if(sp %in% dat.07$accepted_name[which(dat.07$plot == plt)]){
           print(dat.mat[plt, sp])
    }
  }
}


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
  dat.mat[dat$plot[i], dat$accepted_name[i]] <- 1
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



