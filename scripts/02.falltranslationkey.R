#MAKE NEW TNRS TRANSLATION KEY TO SEND TO DR. HIPP
# using the same method as in the summer, take the species list from 2007 + 2018 pool 
# and sumbit to TNRS for cleaned names list 
# OUTPUT: fall_translation_key.csv, 2007_spp_pool.csv, 2018_spp_pool.csv, 
# 2007_2018_spp_pool.csv (will just append a column with accepted name to spp pool)

source('~/Documents/GitHub/east_woods_work/scripts/01.combine_spp_pools.R')
library(magrittr)
dat.all.sorted <- sapply(dat.all$species, function(x) x) %>%
  unlist %>%
  trimws %>%
  unique %>%
  sort

dat.all.sorted <- data.frame(dat.all.sorted)
write.csv(dat.all.sorted, '../../outputs/complete_spp_list.csv')
#take these species lists and run through http://tnrs.iplantcollaborative.org/TNRSapp.html
#TNRS OUTPUT: complete_tnrs_list.csv
###################################################################################################
# make the translation key with original species name in one column and 
# accepted species name in another

# LAST UPDATED TRANSLATION KEY: 11/3

tnrs_all <- data.frame(read.csv('../../outputs/complete_tnrs_list.csv', as.is = T))

###################################################################################################
#append the accepted name to each of the spp pool dataframes

dat.all$accepted_name <- tnrs_all$Accepted_name[match(dat.all$species, tnrs_all$Name_submitted)] 

dat.07$accepted_name <- tnrs_all$Accepted_name[match(dat.07$species, tnrs_all$Name_submitted)]

dat.18$accepted_name <- tnrs_all$Accepted_name[match(dat.18$species, tnrs_all$Name_submitted)]

