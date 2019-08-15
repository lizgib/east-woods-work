# MAKE THE WORKING DATASETS 
# combine the species pool from spring and sum surveys for 2007 and 2018 and then 
# combine 2007 and 2018 total species pools

library(readxl)
setwd('~/Documents/GitHub/east_woods_work/')

#-----------------------------------------------------------------------------------------------------------
# READ IN DATA
#-----------------------------------------------------------------------------------------------------------
# 2007 
path.2007 = '/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2007/'

herb.spring.07 <- read_excel(file.path(path.2007, "Vegetation Sampling-Final_1_17_07.xls"), sheet = "Ground Layer", skip = 1)
herb.spring.07 <- herb.spring.07[,-c(5,6,7,8)]
# herb cover = % of plot covered
names(herb.spring.07) <- c("plot", "spp.code", "species", "cover")
# ----
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
#-----


tree.spring.07 <- read_excel(file.path(path.2007, "Vegetation Sampling-Final_1_17_07.xls"), sheet = "Tree Plots", skip = 1)
# code a new column based on the DBH live and DBH dead columns called Status that says whether 'live' or 'dead'
# DBH = cover
names(tree.spring.07) <- c("PlotID", "Spp.Code", "Spp.Name", "DBH.live", "DBH.dead", "Native")

tree.sum.07 <- read_excel(file.path(path.2007, "Vegetation Sampling-Final_1_17_07.xls"), sheet = "Tree Plots", skip = 1)
names(tree.sum.07) <- c("PlotID", "Spp.Code", "Spp.Name", "DBH.live", "DBH.dead", "Native")

tree.sum.07$sample_period <- 'SUM'
tree.spring.07$sample_period <- 'SPR'

tree07 <- rbind(tree.sum.07, tree.spring.07)

tree07$PlotID <- as.factor(tree07$PlotID)
tree07$Spp.Code <- as.factor(tree07$Spp.Code)
tree07$Spp.Name <- as.factor(tree07$Spp.Name)
tree07$DBH.live <- as.numeric(tree07$DBH.live)
tree07$DBH.dead <- as.numeric(tree07$DBH.dead)
tree07$Native <- as.factor(tree07$Native)
tree07$Native[tree07$Native=="-"] <- NA
tree07 <- data.frame(tree07)

tree07 <- tree07[which(!is.na(tree07$DBH.live)),]
# cover in Basal Area 
tree07$cover <- pi*(tree07$DBH.live/2)^2
tree07 <- tree07[,-c(4,5,6)]
tree07 <- tree07[,c(1,2,3,5,4)]
names(tree07) <- c("plot", "spp.code", "species", "cover", "sample_period")  

#-----------------------------------------------------------------------------------------------------------
# 2018
path.2018 = '/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Final Data from AES/Final data (4th round) from AES.10.24.2018/'
#-----
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
#-----
tree18 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22.xlsx"), sheet = "Tree Layer")
tree18 <- tree18[which(tree18$`Canopy Position` != 'S'),]
tree18$`DBH cm` <- as.numeric(tree18$`DBH cm`)
tree18$cover <- pi*(tree18$`DBH cm`/2)^2
# more manual cleaning by dr rollinson
tree18 <- tree18[,-c(1,2,6,7,8,9,10)]
names(tree18) <- c("plot", "spp.code", "species", "cover")

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

tree07$datset <- 'T'

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

tree18$sample_period <- 'SPR'
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

dat.07$cover <- as.numeric(dat.07$cover)
dat.18$cover <- as.numeric(dat.18$cover)

dat.all <- rbind(dat.18, dat.07)

# I think this may be a problem with the new survey data. the plots between 2007 and 2018 are not matching up:

plots_both_years <- intersect(dat.07$plot, dat.18$plot)
dat.07 <- dat.07[which(dat.07$plot %in% plots_both_years),]
dat.18 <- dat.18[which(dat.18$plot %in% plots_both_years),]
dat.all <- dat.all[which(dat.all$plot %in% plots_both_years),]


write.csv(dat.all, 'data/Species/dat.all.csv', row.names = F, quote = F)
#write.csv(dat.all, '/Volumes/GoogleDrive/My Drive/East Woods/URF 2018 Gibbons/Data/Species/dat.all.csv', row.names = F, quote = F)
# clear out global environment 
# rm(list = ls())


