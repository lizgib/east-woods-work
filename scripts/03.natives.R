
################
# NATIVE STATUS
#################


#dat.all <- read.csv('data/dat.all.csv')
source('~/Documents/GitHub/east_woods_work/scripts/02.falltranslationkey.R')
setwd('~/Documents/GitHub/east_woods_work/')
usda_spp_dat <- read.csv('data/species_data/usda_spp_info.csv')


native_id <- data.frame(usda_spp_dat[,c('Scientific.Name', 'Native.Status')])
names(native_id) <- c('Accepted_name', 'native')
native_id$native <- 'n'
invasive_tnrs <- read.csv('data/species_data/invasives_tnrs.csv')
invasive_tnrs$native <- 'i'
native_id <- rbind(native_id, invasive_tnrs[,c('Accepted_name', 'native')])

dat.all$nativestatus <- native_id$native[match(dat.all$accepted_name, native_id$Accepted_name)] # broooo need to re run the translation key with the new data from summer!! 12/6

missing_natives <- unique(dat.all$accepted_name[which(is.na(dat.all$nativestatus))]) # I have this number trimmed down to 86!!

#############################################################################################
# manually go through list of 86 spp and get IDs for them --> Dr. Hipp did this part 

# read back in the missing natives file
revised_missing_invasives <- read.csv('data/species_data/missing_natives - missing_natives.csv') # key from Dr. Hipp with his comments 
revised_missing_invasives <- revised_missing_invasives[,c('x', 'native.exotic.invasive')]
names(revised_missing_invasives) <- c('Accepted_name', 'native')
# append the missing names/statuses to native_id
native_id <- rbind(native_id, revised_missing_invasives)

dat.all$nativestatus <- native_id$native[match(dat.all$accepted_name, native_id$Accepted_name)]
 # run this again we should have all our species covered 

write.csv(dat.all, 'data/dat.all.csv', row.names = F, quote = F)
#rm(list = ls())


