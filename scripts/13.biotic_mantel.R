#analyses on 2007  
library(ggplot2)
source('~/Documents/GitHub/east_woods_work/scripts/10.phylo_diss_matrices.R')
source('~/Documents/GitHub/east_woods_work/scripts/09.envt_data.R')
source('~/Documents/GitHub/east_woods_work/scripts/11.analyzing_data.R')
source('~/Documents/GitHub/east_woods_work/scripts/phylo_metrics.R')

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

# plot by plot

fit07 <- lm(phylo_all_07$pd.obs ~ liz_data$elevation + liz_data$slope + liz_data$aspect + liz_data$burn_count + liz_data$canopy_07
           + liz_data$plot_invasive_cover_07)


fit18 <- lm(phylo_all_18$pd.obs ~ liz_data$elevation + liz_data$slope + liz_data$aspect + liz_data$burn_count + liz_data$canopy_18
            + liz_data$plot_invasive_cover_18)





