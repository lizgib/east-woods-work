#MAKE THE WORKING DATASETS 
# combine the species pool from spring and fall surveys for 2007 and 2018 and then 
# combine 2007 and 2018 total species pools
#OUTPUT: 2007_spp_pool.csv, 2018_spp_pool.csv, 2007_2018_spp_pool.csv


setwd('~/Documents/morton arb/east_woods_phylogeny/DATA/')
#git version 
#setwd('~/Documents/GitHub/east-woods-work/data/')
#################
# READ IN DATA
################
#2007 

dat.herb.spring.07.o <- read.csv('spp.herb.2007.csv', as.is = T) # what is with the o's?? i think it was original but why 11/1 10:15
dat.herb.spring.07 <- dat.herb.spring.07.o[1:6]
dat.herb.fall.07.o <- read.csv('sum.spp.herb.2007.csv', as.is = T)
dat.herb.fall.07 <- dat.herb.fall.07.o[1:6]
dat.shrub.spring.07.o <- read.csv('spp.shrub.2007.csv', as.is = T)
dat.shrub.spring.07 <- dat.shrub.spring.07.o[1:6]
dat.shrub.fall.07.o <- read.csv('sum.spp.shrub.2007.csv', as.is = T)
dat.shrub.fall.07 <- dat.shrub.fall.07.o[1:6]
dat.tree.spring.07.o <- read.csv('spp.tree.2007.csv', as.is = T)
dat.tree.spring.07 <- dat.tree.spring.07.o[1:6]
dat.tree.fall.07.o <- read.csv('sum.spp.tree.2007.csv', as.is = T)
dat.tree.fall.07 <- dat.tree.fall.07.o[1:6]
#########################################################################################
#2018

dat.herb.spring.18.o <- read.csv('final.herbs.2018.csv', as.is = T)
dat.herb.spring.18 <- dat.herb.spring.18.o[3:6]
dat.herb.fall.18.o <- read.csv('fall.herbs.2018.csv', as.is = T)
dat.herb.fall.18 <- dat.herb.fall.18.o[3:6]
dat.shrub.spring.18.o <- read.csv('final.shrubs.2018.csv', as.is = T)
dat.shrub.spring.18 <- dat.shrub.spring.18.o[3:8]
dat.shrub.spring.18$Category..Seedling.S...Sapling.U...Shrub.SH. <- NULL
dat.shrub.spring.18$Sapling.Category...5cm.or...5cm <- NULL
# dat.shrub.fall.18.o <- read.csv('final.shrubs.2018.csv', as.is = T)
# dat.shrub.fall.18 <- dat.shrub.fall.18.o[3:8]
# dat.shrub.fall.18$Category..Seedling.S...Sapling.U...Shrub.SH. <- NULL
# dat.shrub.fall.18$Sapling.Category...5cm.or...5cm <- NULL
dat.tree.spring.18.o <- read.csv('final.trees.2018.csv', as.is = T)
dat.tree.spring.18 <- dat.tree.spring.18.o[3:6]
# dat.tree.fall.18.o <- read.csv('final.trees.2018.csv', as.is = T)
# dat.tree.fall.18 <- dat.tree.fall.18.o[3:6]

#########################################################################################
# COMBINE SPP POOLS 
#########################################################################################
#2007 

dat.07 <- dat.herb.spring.07
names(dat.herb.fall.07) <- names(dat.herb.spring.07)
dat.herbs07 <- rbind(dat.07, dat.herb.fall.07)
dat.herbs07$datset <- 'H'
dat.07.1 <- dat.herbs07 # why am I putting them into this .1 object?? i cant remember 11/1 10:15

names(dat.shrub.fall.07) <- names(dat.07[1:6])
names(dat.shrub.spring.07) <- names(dat.07[1:6])
dat.shrubs07 <- rbind(dat.shrub.fall.07, dat.shrub.spring.07)
dat.shrubs07$datset <- 'S'

names(dat.tree.fall.07) <- names(dat.07[1:6])
names(dat.tree.spring.07) <- names(dat.07[1:6])
dat.trees07 <- rbind(dat.tree.spring.07, dat.tree.fall.07)
dat.trees07$datset <- "T"

dat.07.1 <- rbind(dat.07.1, dat.shrubs07, dat.trees07) # Why did I do this?? 11/1 10:11 is wondering? 
######################################################################################################
#2018 

names(dat.herb.fall.18) <- names(dat.07[1:4])
names(dat.herb.spring.18) <- names(dat.07[1:4])
dat.herbs18 <- rbind(dat.herb.fall.18, dat.herb.spring.18)
dat.herbs18$datset <- 'H'

names(dat.shrub.spring.18) <- names(dat.07[1:4])
#names(dat.shrub.fall.18) <- names(dat.07[1:4])
#dat.shrubs18 <- rbind(dat.shrub.spring.18, dat.shrub.fall.18) # why did i have a fall dataset? I dont think they resurveyed?
dat.shrubs18 <- dat.shrub.spring.18
dat.shrubs18$datset <- 'S'
names(dat.tree.spring.18) <- names(dat.07[1:4])
#names(dat.tree.fall.18) <- names(dat.07[1:4])
#dat.trees18 <- rbind(dat.tree.fall.18, dat.tree.spring.18)
dat.trees18 <- dat.tree.spring.18
dat.trees18$datset <- 'T' 
dat.18 <- rbind(dat.herbs18, dat.shrubs18, dat.trees18)

######################################################################################################
#2007 + 2018

#neeeeed to make sure that the species dont have any weird spacing or characters in the name that will
# mess up later analysis
dat.07.1$species <- trimws(dat.07.1$species)
dat.18$species <- trimws(dat.18$species)
dat.07 <- dat.07.1[, c('plot', 'code', 'species', 'cover','datset')]

#just adding a column to make keeping track of the years easier 
dat.07$year <- 2007
dat.18$year <- 2018

dat.all <- rbind(dat.18, dat.07)
dat.all$plot <- gsub('-', '', dat.all$plot)
dat.07$plot <- gsub('-', '', dat.07$plot)
dat.herb.fall.07$plot <- gsub('-', '', dat.herb.fall.07$plot)
dat.tree.fall.07$plot <- gsub('-', '', dat.tree.fall.07$plot)
dat.shrub.fall.07$plot <- gsub('-', '', dat.shrub.fall.07$plot)

# git version 
#write.csv(dat.all, '../outputs/all_survey_data.csv')



