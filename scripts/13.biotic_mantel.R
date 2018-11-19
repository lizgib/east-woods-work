#analyses on 2007  
library(ggplot2)
source('~/Documents/GitHub/east_woods_work/scripts/10.phylo_dist_matrices.R')
source('~/Documents/GitHub/east_woods_work/scripts/09.envt_data.R')
source('~/Documents/GitHub/east_woods_work/scripts/11.analyses.R')

# all continous data I have so far 
dist_aspect <- dist(liz_data$aspect)
slope <- dist(liz_data$slope)
elevation <- dist(liz_data$elevation)
burn_count <- dist(liz_data$burn_count)
marlin_canopy <- dist(liz_data$marlin_canopy)
invasives_18 <- dist(liz_data$invasive_ratio_18)
invasives_07 <- dist(liz_data$invasive_ratio_07)
canopy18 <- dist(liz_data$canopy_18)
canopy07 <- dist(liz_data$canopy_07)




# pdiversity_07 <- dist(phylo_all) <- this one shold use the phylogenetic distance metric not euclidean
# percent_ACM <-dist(liz_data$percent_ACM)
# percent_ECM <- dist(liz_data$percent_ECM)  # NEDD TO CALCULATE THESE STILL 
# tree phylogenetic diversity

##################################################################################################################

source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/mantelMultiple.R')

all_mantel <- mantelMultiple(beta_Dpw_all.pa.07, X = list(aspect = dist_aspect, slope = slope, 
                                                          elevation = elevation, canopy07 = canopy07, canopy18 = canopy18, 
                                                          invasives07 = invasives_07, invasives18  = invasives_18 ))

#if DPw is nonsignificant....maybe try a terminal metric (see p 150 in Cadotte)
#terminal metrics sensitive to turnover at tips of tree
#basal metrics (such as Dpw) are sensitive to turnover deeper in phylogeny
#in the book, with his sample dataset he found that the terminal metrics were more significantly 
#related to environmental distances
##################################################################################################################



