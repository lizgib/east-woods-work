#analyses on 2007  
library(ggplot2)
library(vegan)
library(phytools)
##################################################################################################################

source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/mantelMultiple.R')

# all_mantel.07 <- mantelMultiple(beta_Dnn_all.07, X = list(aspect = aspect, slope = slope, 
#                                                           elevation = elevation, canopy07 = canopy07, 
#                                                           invasives07 = invasives07 ))

# MY mantel test isnt working with nearest neighbor!! Cuz some rows have NAs for some reason??
# trying mpd instead...

# so 12/3 what I think is happening is that something is wrong with canopy and invasive ratio!! 
# the test runs when I take care of those 

# ok but it also only works with mean pairwise not mean nearest neighbor 

all_mantel.07 <- mantelMultiple(beta_Dnn_all.07, X = list(aspect = aspect, slope = slope,
                                                          elevation = elevation))


all_mantel.18 <- mantelMultiple(beta_Dnn_all.18, X = list(aspect = aspect, slope = slope, 
                                                          elevation = elevation, canopy18 = canopy18, 
                                                          invasives18 = invasives18))

# use phytools or vegan for multiple mantel 


##################################################################################################################

# vegan
all.mantel.07 <- mantel(beta_Dnn_all.07, aspect, slope, elevation, burn_count, 
                        canopy07, invasives07, soilindex, drainage)

all.mantel.18 <- mante(beta_Dnn_all.18, aspect, slope, elevation, burn_count, 
                       canopy18, invasives18, soilindex, drainage)

# phytools 
temp <- beta_Dnn_all.07[which(as.matrix(rownames(beta_Dnn_all.07) != 'ZZ142'))]
all.mantel.07 <- multi.mantel(temp, X = list(aspect, slope, elevation, burn_count, 
                                                        canopy18, invasives18, soilindex, drainage))

