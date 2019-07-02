#COMMUNITY DATA MATRICES PRESENCE ABSENCE
#creates a presense absence based community matrix for herbs, shrubs, and trees for 
# 2007
# 2018 

setwd('~/Documents/GitHub/east_woods_work/')
dat.all <- read.csv('data/species/dat.all.csv')
library(ape)
library(reshape2)
library(vegan)
data("plots.env")
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


seedlings <- function(tree.mat, herb.mat){
  vec <- c()
  for (sp in herb.mat$accepted_name){
    if(sp %in% tree.mat$accepted_name){
      vec <- c(vec, paste(sp, 'Seedling', sep = ' '))
    }
    else{
      vec <- c(vec, sp)
    }
  }
  return(vec)
}


#---------------------------------------------------------------------------------------------------
# FUNCTION CALL
dat.18 <- dat.all[which(dat.all$year == '2018'),]
dat.07 <- dat.all[which(dat.all$year == '2007'),]
dat.sum.07 <- dat.07[which(dat.07$sample_period == 'SUM'),]

dat.sum.understory.18 <- dat.18[which(dat.18$sample_period == 'SUM'),]

dat.07.trees <- dat.07[dat.07$datset == 'T',]
dat.07.understory <-dat.07[which(dat.07$datset == 'H' | dat.07$datset == 'S'),]
vec <- seedlings(dat.07.trees, dat.07.understory)
dat.07.understory$accepted_name <- vec

dat.sum.understory.07 <- dat.07[which(dat.07.understory$sample_period == 'SUM'),]

dat.18.trees <- dat.18[dat.18$datset == 'T',]
dat.18.understory <- dat.18[which(dat.18$datset == 'H' | dat.18$datset == 'S'),]
vec <- seedlings(dat.18.trees, dat.18.understory)
dat.18.understory$accepted_name <- vec

vec <- seedlings(dat.18.trees, dat.sum.understory.18)
dat.sum.understory.18$accepted_name <- vec
dat.sum.18 <- rbind(dat.sum.understory.18, dat.18.trees)

dat.mat.all.07 <- com_mat(dat.07, tr.ewv4)
dat.mat.all.18 <- com_mat(dat.18, tr.ewv4)

dat.mat.understory.18 <- com_mat(dat.18.understory, tr.ewv4)
dat.mat.trees.18 <- com_mat(dat.18.trees, tr.ewv4)

dat.mat.understory.07 <- com_mat(dat.07.understory, tr.ewv4)
dat.mat.trees.07 <- com_mat(dat.07.trees, tr.ewv4)

com.mat.understory.sum.07 <- com_mat(dat.sum.understory.07, tr.ewv4)
com.mat.understory.sum.18 <- com_mat(dat.sum.understory.18, tr.ewv4)

com.mat.sum.07 <- com_mat(dat.sum.07, tr.ewv4)
com.mat.sum.18 <- com_mat(dat.sum.18, tr.ewv4)

# exclude Hidden Lake, just east woods plots 

ew_all_07 <- dat.mat.all.07[which(rownames(dat.mat.all.07) %in% rownames(plots.env)),]
ew_all_18 <- dat.mat.all.18[which(rownames(dat.mat.all.18) %in% rownames(plots.env)),]

ew_trees_07 <- dat.mat.trees.07[which(rownames(dat.mat.trees.07) %in% rownames(plots.env)),]
ew_trees_18 <- dat.mat.trees.18[which(rownames(dat.mat.trees.18) %in% rownames(plots.env)),]

ew_under_07 <- dat.mat.understory.07[which(rownames(dat.mat.understory.07) %in% rownames(plots.env)),]
ew_under_18 <- dat.mat.understory.18[which(rownames(dat.mat.understory.18) %in% rownames(plots.env)),]


write.csv(com.mat.understory.sum.07, 'data/Community_Matrix/2007/summer.under.07.csv')
write.csv(com.mat.understory.sum.18, 'data/Community_Matrix/2018/summer.under.18.csv')

write.csv(com.mat.sum.07, 'data/Community_Matrix/2007/summer.07.csv')
write.csv(com.mat.sum.18, 'data/Community_Matrix/2018/summer.18.csv')

write.csv(dat.mat.all.07, 'data/Community_Matrix/2007/dat.mat.all.07.csv')
write.csv(dat.mat.all.18, 'data/Community_Matrix/2018/dat.mat.all.18.csv')

write.csv(dat.mat.understory.07, 'data/Community_Matrix/2007/dat.mat.understory.07.csv')
write.csv(dat.mat.understory.18, 'data/Community_Matrix/2018/dat.mat.understory.18.csv')

write.csv(dat.mat.trees.07, 'data/Community_Matrix/2007/dat.mat.trees.07.csv')
write.csv(dat.mat.trees.18, 'data/Community_Matrix/2018/dat.mat.trees.18.csv')

write.csv(ew_all_07, 'data/Community_Matrix/2007/ew.07.csv')
write.csv(ew_all_18, 'data/Community_Matrix/2018/ew.18.csv')

write.csv(ew_trees_07, 'data/Community_Matrix/2007/ew.trees.07.csv')
write.csv(ew_trees_18, 'data/Community_Matrix/2018/ew.trees.18.csv')

write.csv(ew_under_07, 'data/Community_Matrix/2007/ew.understory.07.csv')
write.csv(ew_under_18, 'data/Community_Matrix/2018/ew.understory.18.csv')











