#COMMUNITY DATA MATRICES PRESENCE ABSENCE
#creates a presense absence based community matrix for herbs, shrubs, and trees for 
# 2007
# 2018 

setwd('~/Documents/GitHub/east_woods_work/')
dat.all <- read.csv('data/species/dat.all.csv')
library(ape)
library(reshape2)
library(vegan)
tr.ewv4 <- read.tree('~/Documents/GitHub/east_woods_work/outputs/tr.ew.Spring19')
#---------------------------------------------------------------------------------------------------
# FUNCTIONS 

com_mat <- function(dat, tr.ewv4){
  dat$accepted_name <- trimws(dat$accepted_name)
  vects <- list(plots = unique(sort(dat$plot)),
                sp = unique(sort(dat$species)),
                accname = unique(sort(dat$accepted_name)))
  vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
  dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                    dimnames = list(vects$plots, vects$accname))

  for(p in unique(dat$plot)){
    spp_occur <- as.data.frame(table(dat$accepted_name[which(dat$plot == p)]))
    for (spp in spp_occur$Var1){
      if(!spp %in% dimnames(dat.mat)[[2]]) next
      dat.mat[p, spp] <- 1
    }
  }
  colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
  names_in_tree <- intersect(tr.ewv4$tip.label, colnames(dat.mat))
  dat.mat.out <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
  return(dat.mat.out) #
}


#---------------------------------------------------------------------------------------------------
# FUNCTION CALL
dat.18 <- dat.all[which(dat.all$year == '2018'),]
dat.07 <- dat.all[which(dat.all$year == '2007'),]

# Need to also break up the community matrices into understory and tree layers so we can 
# calculate phylogenetic diversity of tree and understory layers (will be important for invasive
# and burn analyses)
#dat.07.understory <- dat.07[dat.07$datset == 'S',]
#dat.07.understory <-rbind(dat.07.understory, dat.07[dat.07$datset == 'H',])
dat.07.trees <- dat.07[dat.07$datset == 'T',]
  dat.07.understory <-dat.07[which(!dat.07$accepted_name %in% unique(dat.07.trees$accepted_name)),]


#dat.18.understory <- dat.18[dat.18$datset == 'S',]
#dat.18.understory <-rbind(dat.18.understory, dat.18[dat.18$datset == 'H',])
dat.18.trees <- dat.18[dat.18$datset == 'T',]
dat.18.understory <- dat.18[which(!dat.18$accepted_name %in% unique(dat.18.trees$accepted_name)),]

dat.mat.all.07 <- com_mat(dat.07, tr.ewv4)
dat.mat.all.18 <- com_mat(dat.18, tr.ewv4)

dat.mat.understory.18 <- com_mat(dat.18.understory, tr.ewv4)
dat.mat.trees.18 <- com_mat(dat.18.trees, tr.ewv4)

dat.mat.understory.07 <- com_mat(dat.07.understory, tr.ewv4)
dat.mat.trees.07 <- com_mat(dat.07.trees, tr.ewv4)

write.csv(dat.mat.all.07, 'data/Community_Matrix/2007/dat.mat.all.07.csv')
write.csv(dat.mat.all.18, 'data/Community_Matrix/2018/dat.mat.all.18.csv')

write.csv(dat.mat.understory.07, 'data/Community_Matrix/2007/dat.mat.understory.07.csv')
write.csv(dat.mat.understory.18, 'data/Community_Matrix/2018/dat.mat.understory.18.csv')

write.csv(dat.mat.trees.07, 'data/Community_Matrix/2007/dat.mat.trees.07.csv')
write.csv(dat.mat.trees.18, 'data/Community_Matrix/2018/dat.mat.trees.18.csv')


# OK after some testing I've figured it out. The discrepancy between the original data and 
# the community matrix are coming from '' (species names being removed via the tnrs key)
# the community matrix does not include these 
# all the plots that have differing numbers between original data and com_mat should only be off by one
# 12/31







