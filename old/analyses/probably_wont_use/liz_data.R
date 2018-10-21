#Liz's envt data 
#setting up a big excel sheet of all my environmental data for plots
setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
source('SCRIPTS/newstuff/00.read_data.R')
source('SCRIPTS/newstuff/05.invasives.R')
source('../data_processing/SCRIPTS/02.cover.R')
setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')

liz_data <- data.frame(marlin_data[,2:8])
liz_data$Group <- NULL
liz_data$Type <- NULL

##########################################################################################
#getting a canopy cover estimate for 2018 
#will do the basal area * number of trees 
####################################################
# change of plans on o8/2 just use the CanopyOpenness from marlin dat
####################################################
# canopy_cover <- data.frame(rownames(liz_data))
# idkwattocallthis <- data.frame(table(cover.dat.tree$plot))
# 
# number_trees <- c()
# basal_area <- c()
# for (plt in canopy_cover$rownames.liz_data.){
#   number_trees <- c(number_trees, ifelse(plt %in% idkwattocallthis$Var1,
#          idkwattocallthis$Freq[which(idkwattocallthis$Var1 == plt)], 0))
#   basal_area <- c(basal_area, ifelse(plt %in% cover.dat.tree$plot,
#          sum(cover.dat.tree$cover[which(cover.dat.tree$plot == plt)]), 0))
# }
# canopy_cover$number_trees <- number_trees
# canopy_cover$basal_area <- basal_area
# canopy_cover$canopy_proxy <- canopy_cover$basal_area * canopy_cover$number_trees

##########################################################################################
#Invasives 
#for each plot do the change in invasives from 2007 to 2018 
#sum of invasive cover in 2007 - sum of invasive cover in 2018 


#invasive cover
#invasive cover : the sum of all native plants in plot


##########################################################################################
#Drought Index 

#plotdryness <- ln(elevation + (slope * aspect/180))
liz_data$Dryness <- log(liz_data$Elevation + (liz_data$Slope * abs(180-liz_data$Aspect)))
#evaluate these separately 

                        
                      




