#MAKE THE WORKING DATASETS 
# combine the species pool from spring and sum surveys for 2007 and 2018 and then 
# combine 2007 and 2018 total species pools
#OUTPUT: dat.all.csv

#git version 
library(readxl)
setwd('~/Documents/GitHub/east_woods_work/')

#-----------------------------------------------------------------------------------------------------------
# READ IN DATA
#-----------------------------------------------------------------------------------------------------------
# 2007 
path.2007  = '~/Documents/GitHub/east_woods_work/data/East_Woods/Inventory_2007/'
#path.2007 = '/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2007/'

herb.spring.07 <- read_excel(file.path(path.2007, "Vegetation Sampling-Final_1_17_07.xls"), sheet = "Ground Layer", skip = 1)
herb.spring.07 <- herb.spring.07[,-c(5,6,7,8)]
# herb cover = % of plot covered
names(herb.spring.07) <- c("plot", "spp.code", "species", "cover")

herb.sum.07 <- read_excel(file.path(path.2007, "Summer Vegetation Sampling-RevisedFinal_2_20.CORRECTED PLOTS.xls"), sheet = "Ground Layer", skip = 1)
herb.sum.07 <- herb.sum.07[,-c(5,6,7,8)]
names(herb.sum.07) <- c("plot", "spp.code", "species", "cover")

shrub.spring.07 <- read_excel(file.path(path.2007, "Vegetation Sampling-Final_1_17_07.xls"), sheet = "Shrub Layer", skip = 1)
shrub.spring.07 <- shrub.spring.07[,-c(5,6,7)]
# number of live stems = cover 
names(shrub.spring.07) <- c("plot", "spp.code", "species", "cover")

shrub.sum.07 <- read_excel(file.path(path.2007, "Summer Vegetation Sampling-RevisedFinal_2_20.CORRECTED PLOTS.xls"), sheet = "Shrub Layer", skip = 1)
shrub.sum.07 <- shrub.sum.07[,-c(5,6,7)]
names(shrub.sum.07) <- c("plot", "spp.code", "species", "cover")

tree.spring.07 <- read_excel(file.path(path.2007, "Vegetation Sampling-Final_1_17_07.xls"), sheet = "Tree Plots", skip = 1)
tree.spring.07 <- tree.spring.07[,-c(5,6)]
# DBH = cover
names(tree.spring.07) <- c("plot", "spp.code", "species", "cover")

tree.sum.07 <- read_excel(file.path(path.2007, "Vegetation Sampling-Final_1_17_07.xls"), sheet = "Tree Plots", skip = 1)
tree.sum.07 <- tree.sum.07[,-c(5,6)]
names(tree.sum.07) <- c("plot", "spp.code", "species", "cover")

#-----------------------------------------------------------------------------------------------------------
# 2018
# path.2018 = '/Volumes/GoogleDrive/My Drive/East Woods/Inventory_2018/'
path.2018 = '~/Documents/GitHub/east_woods_work/data/East_Woods/Inventory_2018/'

herb.spring.18 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22.xlsx"), sheet = "Herbaceous")
herb.spring.18 <- herb.spring.18[,-c(1,2,7,8,9)]
# again herb cover = % plot covered
names(herb.spring.18) <- c("plot", "spp.code", "species", "cover")

herb.sum.18 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Summer Herb Data Sheet_AESedits_Oct-22.xlsx"), sheet = "Summer Herbaceous")
herb.sum.18 <- herb.sum.18[,-c(1,2,7,8,9)]
names(herb.sum.18) <- c("plot", "spp.code", "species", "cover")

shrub.spring.18 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22.xlsx"), sheet = "Shrub Layer")
shrub.spring.18 <- shrub.spring.18[,-c(1,2,6,7,9)]
# shrub cover = number live stems 
names(shrub.spring.18) <- c("plot", "spp.code", "species", "cover")

tree.spring.18 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22.xlsx"), sheet = "Tree Layer")
tree.spring.18 <- tree.spring.18[,-c(1,2,7,8,9,10)]
# tree cover = DBH
names(tree.spring.18) <- c("plot", "spp.code", "species", "cover")

#-----------------------------------------------------------------------------------------------------------
# COMBINE SPP POOLS
#-----------------------------------------------------------------------------------------------------------
#2007

herb.sum.07$sample_period <- 'SUM'
herb.spring.07$sample_period <- 'SPR'

herb07 <- rbind(herb.spring.07, herb.sum.07)
herb07$datset <- 'H'

shrub.sum.07$sample_period <- 'SUM'
shrub.spring.07$sample_period <- 'SPR'
shrub.sum.07$datset <- 'S'
shrub.spring.07$datset <- 'S'
shrub07 <- rbind(shrub.sum.07, shrub.spring.07)

tree.sum.07$sample_period <- 'SUM'
tree.spring.07$sample_period <- 'SPR'
tree.sum.07$datset <- 'T'
tree.spring.07$datset <- 'T'

tree07 <- rbind(tree.spring.07, tree.sum.07)

dat.07 <- rbind(herb07, shrub07, tree07)
#-----------------------------------------------------------------------------------------------------------
#2018

herb.sum.18$sample_period <- 'SUM'
herb.spring.18$sample_period <- 'SPR'

herb18 <- rbind(herb.sum.18, herb.spring.18)
herb18$datset <- 'H'

shrub.spring.18$sample_period <- 'SPR'
shrub18 <- shrub.spring.18
shrub18$datset <- 'S'

tree.spring.18$sample_period <- 'SPR'
tree18 <- tree.spring.18
tree18$datset <- 'T'
dat.18 <- rbind(herb18, shrub18, tree18)

#-----------------------------------------------------------------------------------------------------------
#2007 + 2018

# need to make sure that the species dont have any weird spacing or characters in the name that will
# mess up later analysis

# CLEANING SOME FINAL THINGS UP

dat.07$species <- trimws(dat.07$species)
dat.18$species <- trimws(dat.18$species)
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

write.csv(dat.all, 'data/dat.all.csv', row.names = F, quote = F)
# clear out global environment 
# rm(list = ls())


