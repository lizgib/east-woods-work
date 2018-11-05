#SPECIES INFORMATION DATASET
# using the 2007 + 2018 CLEANED spp pool, create a key for species with data on  
#    1. is spp a spring ephemeral? 
#    2. questionable ID --> species may be misidentified in survey
#    3. flowering time 
#    4. perennial/annual
#OUTPUT: spp_info.csv
#also creates a vector which checks (T/F) if a spp appears in 3 or less plots


################
# NATIVE STATUS
#################

source('~/Documents/GitHub/east_woods_work/scripts/04.natives.R')
spp_info <- unique(native_id)

###################
# PERENNIAL/ANNUAL
###################

spp_info$perennial_annual <- usda_spp_dat$Duration[match(spp_info$Accepted_name, usda_spp_dat$Scientific.Name)]




