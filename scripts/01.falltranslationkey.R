# MAKE NEW TNRS TRANSLATION KEY TO SEND TO DR. HIPP
# using the same method as in the summer, take the species list from 2007 + 2018 pool 
# and sumbit to TNRS for cleaned names list 
# OUTPUT: species_key.csv, spp.07, spp.18, tree.spp.07, understory.spp.07, tree.spp.18, understory.spp.18 
# 2007_2018_spp_pool.csv (will just append a column with accepted name to spp pool)

#source('~/Documents/GitHub/east_woods_work/scripts/01.combine_spp_pools.R')
setwd('~/Documents/GitHub/east_woods_work/')
dat.all <- read.csv('data/species/dat.all.csv')
library(magrittr)
library(readxl)
library(dbplyr)

all.spp.sorted <- sapply(dat.all$species, function(x) x) %>%
  unlist %>%
  trimws %>%
  unique %>%
  sort

all.spp.sorted <- data.frame(all.spp.sorted)
write.csv(all.spp.sorted, 'data/species/all.spp.sorted.csv', row.names = F, quote = F)
#take these species lists and run through http://tnrs.iplantcollaborative.org/TNRSapp.html
#TNRS OUTPUT: complete_tnrs_list.csv
#---------------------------------------------------------------------------------------------------------
# make the translation key with original species name in one column and 
# accepted species name in another

# LAST UPDATED TRANSLATION KEY: 06/12/19

tnrs_all <- data.frame(read.csv('data/species/tnrs_results.csv'))

#---------------------------------------------------------------------------------------------------------
#append the accepted name to each of the spp pool dataframes

dat.all$accepted_name <- tnrs_all$Accepted_name[match(dat.all$species, tnrs_all$Name_submitted)]

# some of the original names were not found by tnrs and were replaced with NA values. in these cases 
# need to go through and check them manually
# created file with history of these changes that need to be made called removed_spp.xlsx that will be in my folder on the drive

dat.all$species <- as.character(dat.all$species)
dat.all$accepted_name <-as.character(dat.all$accepted_name)

removed_spp <- read_excel(file.path('/Volumes/GoogleDrive/My Drive/East Woods/URF 2018 Gibbons/Data/removed_spp.xlsx'), sheet = "Sheet1")
# look through the species names (not the accepted ones) and modify the accepted name category based on the value in removed/resolved column 
# gonna do this the ugly way firs because it is quicker but will make it look nicer later 
removed_spp$`Species Name` <- as.character(removed_spp$`Species Name`)
removed_spp$Resolve <- as.character(removed_spp$Resolve)
accepted_name2 <- c()
for (spp in dat.all$species){
    accepted_name2 <- c(accepted_name2, ifelse(spp %in% removed_spp$`Species Name`, 
                         removed_spp$Resolve[which(removed_spp$`Species Name` == spp)],
                         dat.all$accepted_name[which(dat.all$species == spp)]))}
dat.all$accepted_name <- accepted_name2


write.csv(dat.all, 'data/species/dat.all.csv', row.names = F, quote = F)


# rm(list = ls())


