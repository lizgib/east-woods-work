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
dat.all <- read.csv('data/dat.all.csv')
library(ape)
library(reshape2)
library(vegan)
tr.ewv4 <- read.tree('~/Documents/GitHub/east_woods_work/outputs/tr.ewv4')
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
    print(spp_occur)
    for (spp in spp_occur$Var1){
      print(spp)
      if(!spp %in% dimnames(dat.mat)[[2]]) next
      dat.mat[p, spp] <- 1
    }
  }
  colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
  return(dat.mat)
  # Commenting this out right now because I think that the dsicrepancy in 
  # numb spp per plot I am seeing is due to me only including 
  # spp in the tree. This is is a test dont keep it you idiot
  # make sure the lines below are included in final run it will fuck everything else up
  
  #names_in_tree <- intersect(tr.ewv4$tip.label, colnames(dat.mat))
  #dat.mat.out <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
  #return(dat.mat.out) #
}

num_spp_per_plot <- function(dat){
  tally <- c()
  for( p in unique(dat$plot)){
    x <- length(unique(dat$accepted_name[which(dat$plot == p)]))
    tally <- c(tally, x)
  }
  return(tally)
}


#---------------------------------------------------------------------------------------------------
# FUNCTION CALL
dat.18 <- dat.all[which(dat.all$year == '2018'),]
dat.07 <- dat.all[which(dat.all$year == '2007'),]

dat.mat.all.07 <- com_mat(dat.07, tr.ewv4)
dat.mat.all.18 <- com_mat(dat.18, tr.ewv4)

write.csv(dat.mat.all.07, 'data/dat.mat.all.07.csv')
write.csv(dat.mat.all.18, 'data/dat.mat.all.18.csv')

tally07 <- num_spp_per_plot(dat.07)
tally07
temp <- data.frame(rowSums(dat.mat.all.07))
temp$rowSums.dat.mat.all.07. == tally07

# magical thing I was trying to get to work (doesnt quite work perfect go back to this!!! 12/9)
# for(p in unique(dat$plot)){
#   spp_occur <- as.data.frame(table(dat$accepted_name[which(dat$plot == p)]))
#   print(spp_occur)
#   for (spp in spp_occur$Var1){
#     if(!spp %in% dimnames(dat.mat)[[2]]) next
#     dat.mat[p, spp] <- 1
#   }
# }






