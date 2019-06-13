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

removed_spp <- read_excel(file.path('/Volumes/GoogleDrive/My Drive/East Woods/URF 2018 Gibbons/Data/removed_spp.xlsx'), sheet = "Sheet1")
# look through the species names (not the accepted ones) and modify the accepted name category based on the value in removed/resolved column 
# gonna do this the ugly way firs because it is quicker but will make it look nicer later 
removed_spp$`Species Name` <- as.factor(removed_spp$`Species Name`)
remove_these <- removed_spp$`Species Name`[which(removed_spp$Remove == 'removed')]
#fix_these <- removed_spp$`Species Name`[which(!is.na(removed_spp$Resolve))] # not working so here comes ugly 
# ----------------

# dat.all$new_accepted_name <- ifelse(is.na(removed_spp$Resolve[match(dat.all$species, removed_spp$`Species Name`)]), 
#                                     dat.all$accepted_name, 
#                                     removed_spp$Resolve[match(dat.all$species, removed_spp$`Species Name`)])

dat.all$accepted_name[grep('Hepatica acutiloba', dat.all$species)] <- 'Hepatica acutiloba'
dat.all$accepted_name[grep('Impatiens', dat.all$species)] <- 'Impatiens capensis'
dat.all$accepted_name[grep('Lactuca', dat.all$species)] <- 'Lactuca'
dat.all$accepted_name[grep('Lonicera prolifera', dat.all$species)] <- 'Lonicera reticulata'
dat.all$accepted_name[grep('Malus sieboldii', dat.all$species)] <- 'Malus toringo'
dat.all$accepted_name[grep('Melilotus officinalis', dat.all$species)] <- 'Melilotus officinalis'
dat.all$accepted_name[grep('Osmorhiza claytoniana', dat.all$species)] <- 'Osmorhiza claytonii'
dat.all$accepted_name[grep('Osmorhiza claytonii', dat.all$species)] <- 'Osmorhiza claytonii'
dat.all$accepted_name[grep('Parthenocissus sp.', dat.all$species)] <- 'Parthenocissus quinquefolia'
dat.all$accepted_name[grep('Prenanthes alba', dat.all$species)] <- 'Prenanthes alba'
dat.all$accepted_name[grep('Quercus coccinea', dat.all$species)] <- 'Quercus ellipsoidalis'


# after doing this it looks like a lot of these oddball species may have alreadu been removed...

dat.all$accepted_name[which(dat.all$species %in% remove_these)] <- NA
# dat.all$newcol <- removed_spp$Resolve[match(dat.all$species, removed_spp$`Species Name`)]
# dat.all$newcol <- as.factor(dat.all$newcol)
# 
# for (i in 1:length(dat.all$newcol)){
#   print(dat.all[i, 'newcol'])
#   # if(is.na(dat.all$newcol[i])){
#   #   dat.all$newcol[i] <- dat.all$accepted_name[i]
#   # }
#   # else{
#   #   dat.all$newcol[i] <- dat.all$newcol[i]
#   # }
# }
# # dat.all$new_accepted_name <- ifelse(!is.na(dat.all$newcol), dat.all$newcol, dat.all$accepted_name)



write.csv(dat.all, 'data/species/dat.all.csv', row.names = F, quote = F)


# rm(list = ls())


# there are some species in here that need to be manually cleaned out still 6/3

