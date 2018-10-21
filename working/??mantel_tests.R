# MANTEL TESTS 
# last ran : 10/15 8:30 AM

source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/analyzez.R')

# MAKE THE DISTANCE MATRICES FOR THE ENVIRONMENTAL DATA 

canopy_18_dist <- dist(liz_data$canopy_18, method = 'euclidean')
canopy_07_dist <- dist(liz_data$canopy_07, method = 'euclidean')
burn_dist <- dist(liz_data$burn_count, method = 'euclidean')

# RUN THE MULTIPLE MANTEL TEST 

source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/mantelMultiple.R')

mantel <- mantelMultiple(beta_Dnn_all.pa.18, X = list(canopy_07 = canopy_07_dist, canopy_18 = canopy_18_dist))




