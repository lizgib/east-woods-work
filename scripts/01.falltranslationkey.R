# MAKE NEW TNRS TRANSLATION KEY TO SEND TO DR. HIPP
# using the same method as in the summer, take the species list from 2007 + 2018 pool 
# and sumbit to TNRS for cleaned names list 
# OUTPUT: fall_translation_key.csv, 2007_spp_pool.csv, 2018_spp_pool.csv, 
# 2007_2018_spp_pool.csv (will just append a column with accepted name to spp pool)

#source('~/Documents/GitHub/east_woods_work/scripts/01.combine_spp_pools.R')
setwd('~/Documents/GitHub/east_woods_work/')
dat.all <- read.csv('data/species/dat.all.csv')
library(magrittr)

dat.all.sorted <- sapply(dat.all$species, function(x) x) %>%
  unlist %>%
  trimws %>%
  unique %>%
  sort

dat.all.sorted <- data.frame(dat.all.sorted)
write.csv(dat.all.sorted, 'data/species/complete_spp_list.csv', row.names = F)
#take these species lists and run through http://tnrs.iplantcollaborative.org/TNRSapp.html
#TNRS OUTPUT: complete_tnrs_list.csv
#---------------------------------------------------------------------------------------------------------
# make the translation key with original species name in one column and 
# accepted species name in another

# LAST UPDATED TRANSLATION KEY: 06/03/19

tnrs_all <- data.frame(read.csv('data/species/tnrs_results.csv'))

#---------------------------------------------------------------------------------------------------------
#append the accepted name to each of the spp pool dataframes

dat.all$accepted_name <- tnrs_all$Accepted_name[match(dat.all$species, tnrs_all$Name_submitted)]
dat.all$accepted_name <- gsub(dat.all$accepted_name)
# some of the original names were not found by tnrs and were replaced with NA values. in these cases 
# will keep the original species name
# for(i in which(dat.all$accepted_name == '')){
#   dat.all$accepted_name[i] <- dat.all$species[i]
# }
write.csv(dat.all, 'data/species/dat.all.csv', row.names = F, quote = F)
# rm(list = ls())


# there are some species in here that need to be manually cleaned out still 6/3

