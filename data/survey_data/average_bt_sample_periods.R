'''
12/5/18 
Handling the multiple survey periods 
 some of the species reported in plots are redundant due to being recorded in both spring 
 and summer surveys
 this script will take the average cover between any species found in both spring and summer in a plot
 and only list that species once 
'''
source('~/Documents/GitHub/east_woods_work/scripts/01.combine_spp_pools.R')
#dat.all <- read.csv('data/dat.all.csv')

temp <- dat.07[1:40,]

#-----------------------------------------------------------------------------------------------------------
# FUNCTIONS 

duplicates <- function(dat, plt){
  spp_counts <- data.frame(table(dat$species[which(dat$plt == plt)]))
  duplicate_spp <- spp_counts$Var1[which(spp_counts$Freq > 2)]
  return(duplicate_spp)
}


avg_bt_dup <- function(dat1, dat2, plt, dup){
  for(i in dup){
    cov <- dat$cover[which(dat$species == i)] 
  }
  return(cov)
}

#-----------------------------------------------------------------------------------------------------------

for(p in unique(dat.07$plot)){
  dup <- duplicates(dat.07, p) # list of the duplicate spp for that plot
  covers <- avg_bt_dup(dat.07, p, dup)
  print(covers)
}

