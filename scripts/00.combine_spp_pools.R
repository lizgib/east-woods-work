#MAKE THE WORKING DATASETS 
# combine the species pool from spring and sum surveys for 2007 and 2018 and then 
# combine 2007 and 2018 total species pools
#OUTPUT: dat.07.csv, dat.18.csv, dat.all.csv


#setwd('~/Documents/morton arb/east_woods_phylogeny/DATA/')
#git version 
setwd('~/Documents/GitHub/east_woods_work/data/survey_data/')

# READ IN DATA

#2007 

dat.herb.spring.07 <- read.csv('spp.herb.2007.csv', as.is = T)[1:6] 

dat.herb.sum.07 <- read.csv('sum.spp.herb.2007.csv', as.is = T)[1:6]

dat.shrub.spring.07 <- read.csv('spp.shrub.2007.csv', as.is = T)[1:6]

dat.shrub.sum.07 <- read.csv('sum.spp.shrub.2007.csv', as.is = T)[1:6]

dat.tree.spring.07 <- read.csv('spp.tree.2007.csv', as.is = T)[1:6]

dat.tree.sum.07 <- read.csv('sum.spp.tree.2007.csv', as.is = T)[1:6]

#-----------------------------------------------------------------------------------------------------------
#2018

dat.herb.spring.18 <- read.csv('final.herbs.2018.csv', as.is = T)[3:6]

dat.herb.sum.18 <- read.csv('sum.herbs.2018.csv', as.is = T)[3:6]

dat.shrub.spring.18 <- read.csv('final.shrubs.2018.csv', as.is = T)[3:8]

dat.shrub.spring.18$Category..Seedling.S...Sapling.U...Shrub.SH. <- NULL

dat.shrub.spring.18$Sapling.Category...5cm.or...5cm <- NULL

dat.tree.spring.18 <- read.csv('final.trees.2018.csv', as.is = T)[3:6]


#-----------------------------------------------------------------------------------------------------------
# COMBINE SPP POOLS 
#2007 

dat.herb.sum.07$sample_period <- 'SUM'
dat.herb.spring.07$sample_period <- 'SPR'

names(dat.herb.sum.07) <- names(dat.herb.spring.07)
dat.herbs07 <- rbind(dat.herb.spring.07, dat.herb.sum.07)
dat.herbs07$datset <- 'H'

dat.shrub.sum.07$sample_period <- 'SUM'
dat.shrub.spring.07$sample_period <- 'SPR'
dat.shrub.sum.07$datset <- 'S'
dat.shrub.spring.07$datset <- 'S'

names(dat.shrub.sum.07) <- names(dat.herbs07)
names(dat.shrub.spring.07) <- names(dat.herbs07)
dat.shrubs07 <- rbind(dat.shrub.sum.07, dat.shrub.spring.07)


dat.tree.sum.07$sample_period <- 'SUM'
dat.tree.spring.07$sample_period <- 'SPR'
dat.tree.sum.07$datset <- "T"
dat.tree.spring.07$datset <- 'T'

names(dat.tree.sum.07) <- names(dat.herbs07)
names(dat.tree.spring.07) <- names(dat.herbs07)
dat.trees07 <- rbind(dat.tree.spring.07, dat.tree.sum.07)


dat.07 <- rbind(dat.herbs07, dat.shrubs07, dat.trees07) 
#-----------------------------------------------------------------------------------------------------------
#2018 

names(dat.herb.sum.18) <- names(dat.07[1:4])
names(dat.herb.spring.18) <- names(dat.07[1:4])

dat.herb.sum.18$sample_period <- 'SUM'
dat.herb.spring.18$sample_period <- 'SPR'

dat.herbs18 <- rbind(dat.herb.sum.18, dat.herb.spring.18)
dat.herbs18$datset <- 'H'

names(dat.shrub.spring.18) <- names(dat.07[1:4])

dat.shrub.spring.18$sample_period <- 'SPR'
dat.shrubs18 <- dat.shrub.spring.18
dat.shrubs18$datset <- 'S'

names(dat.tree.spring.18) <- names(dat.07[1:4])

dat.tree.spring.18$sample_period <- 'SPR'
dat.trees18 <- dat.tree.spring.18
dat.trees18$datset <- 'T' 
dat.18 <- rbind(dat.herbs18, dat.shrubs18, dat.trees18)

#-----------------------------------------------------------------------------------------------------------
#2007 + 2018

#neeeeed to make sure that the species dont have any weird spacing or characters in the name that will
# mess up later analysis

# CLEANING SOME FINAL THINGS UP 

dat.07$species <- trimws(dat.07$species)
dat.18$species <- trimws(dat.18$species)
dat.07$freq <- NULL
dat.07$native <- NULL
dat.07$plot <- gsub('-', '', dat.07$plot)

#just adding a column to make keeping track of the years easier 
dat.07$year <- 2007
dat.18$year <- 2018

dat.all <- rbind(dat.18, dat.07)

# Noww... I think this may be a problem with the new survey data. the plots between 2007 and 2018 are not matching up:

plots_both_years <- intersect(dat.07$plot, dat.18$plot)
dat.07 <- dat.07[which(dat.07$plot %in% plots_both_years),]
dat.18 <- dat.18[which(dat.18$plot %in% plots_both_years),]
dat.all <- dat.all[which(dat.all$plot %in% plots_both_years),]

write.csv(dat.all, '../dat.all.csv', row.names = F, quote = F)
rm(list = ls())


