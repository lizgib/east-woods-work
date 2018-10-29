#make my dataframe w plot, spp, accname, and cover
# ok so the gameplan here is that i will keep the cover of each veg type as a separate unit 
# each veg type will amount to 100% for each plot (so the sum of all plant %cover should = 100%, 
# the sum of all basal areas of trees in a plot = 100%, and the total number of stems for shrubs
# = 100%)
# each plot will have a total cover of 3 
# I am not going to be comparing the relative quantities of trees to shrubs to herbs 
# I am comparing the composition of shrubs, herbs, trees, between plots
##########################################################################################################
source('../SCRIPTS/14.combine_spp_pools.R')
# REPLACE DAT07 W DAT.ALL

herb_plots <- data.frame(unique(dat.07[which(dat.07$datset == 'H'),'plot']))
total_cover <- c()
dat.herbs.07$cover <- as.numeric(dat.herbs.07$cover)
for (plt in herb_plots$unique.dat.07.which.dat.07.datset.....H.....plot...){ #for each herb plot I find the sum of all the percent covers
    temp <- sum(dat.herbs.07$cover[which(dat.herbs.07$plot == plt)]) #this will be over 100% for some plots
    total_cover <- c(total_cover, temp) 
}
herb_plots$total_cover <- total_cover #I make a new dataframe that has the total cover for each plot 

dat.herbs.07 <- dat.herbs.07[which(!dat.herbs.07$cover == 0),] #I get rid of any rows that I won't be able to divide on
percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(cover in dat.herbs.07$cover){ #...each spp in the plot takes up
  ptc <- cover/herb_plots$total_cover[which(herb_plots$unique.dat.07.which.dat.07.datset.....H.....plot...
                                            == dat.herbs.07$plot[cover])] 
  percent_of_total_cover <- c(percent_of_total_cover, ptc)
} ####>>>>?>?>>?>?>? this is almost perfect but i have total covers over 100% which i dont want......<<<<<??
dat.herbs.07$percent_total_cover <- percent_of_total_cover #add this percent of total area to the original

##########################################################################################################
#adjust the trees to be in basal area 
dat.tree.07$cover <- as.double(dat.tree.07$cover) #for some reason my cover is a factor so convert so I can do math
dat.tree.07 <- na.omit(dat.tree.07) #get rid of the na's because they mess up the number of rows later
dat.tree.07 <- dat.tree.07[which(!dat.tree.07$cover == 0),] #get rid of rows that have no trees reported
#plot area = 33.82 sq ft
BA <- c() #calculate the basal area (just the area of the trunk)
for (c in dat.tree.07$cover){
  c <- c * 0.0328084 #converting the cm DBH into feet  (so the final area is in sq feet)
  basal_area <- pi * (c/2)**2
  BA <- c(BA, basal_area)
}
dat.tree.07$cover <- BA #replace the DBH values with the basal area. we don't care about dbh anymore

tree_plots <- data.frame(unique(dat.tree.07$plot)) #get all the tree plots
total_cover <- c() #doing the same thing i did for herbs, take all the basal areas in a plot and 
for (plt in tree_plots$unique.dat.tree.07.plot.){ #... add them together so you have the total area 
  temp <- sum(dat.tree.07$cover[which(dat.tree.07$plot == plt)]) # .. that is occupied by trees
  total_cover <- c(total_cover, temp)
}
tree_plots$total_cover <- total_cover 

for(x in 1:length(dat.tree.07$plot)){ #for each spp in the original data
  dat.tree.07$percent_total_cover <- 
    dat.tree.07$cover[x]/tree_plots$total_cover[which(tree_plots$unique.dat.tree.07.plot. == dat.tree.07$plot[x])]
  #divide the cover that species occupies by the total tree cover value for that plot 
}


########################################################################################################################
#adjust shrubs to be in percent cover 
shrub_plots <- data.frame(unique(dat.shrub.07$plot)) #get all the shrub plots
total_cover <- c()
for (plt in shrub_plots$unique.dat.shrub.07.plot.){ #for each shrub plot I find the sum of all the percent covers
  temp <- sum(dat.shrub.07$cover[which(dat.shrub.07$plot == plt)]) #this will be over 100% for some plots
  total_cover <- c(total_cover, temp) 
}
shrub_plots$total_cover <- total_cover #I make a new dataframe that has the total cover for each plot 

dat.shrub.07 <- dat.shrub.07[which(!dat.shrub.07$cover == 0),] #I get rid of any rows that I won't be able to divide on
percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(cover in dat.shrub.07$cover){ #...each spp in the plot takes up
  ptc <- cover/shrub_plots$total_cover[which(shrub_plots$unique.dat.shrub.07.plot. == dat.shrub.07$plot[cover])] 
  percent_of_total_cover <- c(percent_of_total_cover, ptc)
}
dat.shrub.07$percent_total_cover <- percent_of_total_cover #add this percent of total area to the original

########################################################################################################################