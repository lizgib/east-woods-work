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
setwd('~/Documents/GitHub/east_woods_work/')
understory <- read.csv('data/understory.all.csv')
trees <- read.csv('data/trees.all.csv')
library(ape)
library(reshape2)
library(vegan)
tr.ewv4 <- read.tree('~/Documents/GitHub/east_woods_work/outputs/tr.ewv4')
#---------------------------------------------------------------------------------------------------
# FUNCTIONS 



com_mat <- function(dat, tr.ewv4){

  vects <- list(plots = unique(sort(dat$plot)),
                sp = unique(sort(dat$species)),
                accname = unique(sort(dat$accepted_name)))
  vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
  dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                    dimnames = list(vects$plots, vects$accname))
  print(dim(dat.mat))
  print(dim(dat))
  print(length(vects$plots))
  print(length(vects$accname))
  
  # for(i in 1:dim(dat)[1]){
  #   if(!dat[i, 'accepted_name'] %in% dimnames(dat.mat)[[2]]) next
  #   if(!dat[i, 'plot'] %in% dimnames(dat.mat)[[1]]) next
  #   # print(dat[i, 'plot'])
  #   # print(dat[i, 'accepted_name'])
  #   print(dat.mat[dat[i, 'plot'], dat[i, 'accepted_name']])
  # }
  # colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
  # names_in_tree <- intersect(tr.ewv4$tip.label, colnames(dat.mat))
  # dat.mat.out <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
  # return(dat.mat.spp)
}

#---------------------------------------------------------------------------------------------------
# FUNCTION CALL

understory07 <- understory[which(understory$year == '2007'),]
understory18 <- understory[which(understory$year == '2007'),]

trees07 <- trees[which(trees$year == '2007'),]
trees18 <- trees[which(trees$year == '2018'),]

all07 <- rbind(understory07, trees07)
all18 <- rbind(understory18, trees18)

dat.mat.spp <- com_mat(all07, tr.ewv4)

