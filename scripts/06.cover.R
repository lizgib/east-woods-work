#make my dataframe w plot, spp, accname, and cover
# ok so the gameplan here is that i will keep the cover of each veg type as a separate unit 
# each veg type will amount to 100% for each plot (so the sum of all plant %cover should = 100%, 
# the sum of all basal areas of trees in a plot = 100%, and the total number of stems for shrubs
# = 100%)
# each plot will have a total cover of 3 
# I am not going to be comparing the relative quantities of trees to shrubs to herbs 
# I am comparing the composition of shrubs, herbs, trees, between plots

source('~/Documents/GitHub/east_woods_work/scripts/02.falltranslationkey.R')
dat.all$cover <- as.numeric(dat.all$cover)
dat.07$cover <- as.numeric(dat.07$cover)
dat.18$cover <- as.numeric(dat.18$cover)

dat.07$plot <- gsub('-', '', dat.07$plot)
dat.all$plot <- gsub('-', '', dat.all$plot)


####################################################################################################################
# ok looking at cover for 2007 herb layer
herbs07 <- dat.07[which(dat.07$datset == 'H'),]
herb_plots <- data.frame(unique(herbs07$plot))

total_cover <- c()
for (i in herb_plots$unique.herbs07.plot.){
  temp <- sum(herbs07$cover[which(herbs07$plot == i)])
  total_cover <- c(total_cover, temp/2) #### OMG THIS IS A MeSS SOME HOW ITS DOUBLING MY HERB COVER!!!!!  10/27
}

herb_plots$total_cover <- total_cover

plot_herb_cover <- c()
for (plt in herbs07$plot){
  plot_herb_cover <- c(plot_herb_cover, herb_plots$total_cover[which(plt == herb_plots$unique.herbs07.plot.)])
}
herbs07$plot_herb_cover <- plot_herb_cover

herbs07 <- herbs07[which(!herbs07$cover == 0),]
percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(cover in herbs07$cover){ #...each spp in the plot takes up
  ptc <- cover/herb_plots$total_cover[which(herb_plots$unique.herbs07.plot. == herbs07$plot[cover])] 
  percent_of_total_cover <- c(percent_of_total_cover, ptc)
} ####>>>>?>?>>?>?>? this is almost perfect but i have total covers over 100% which i dont want......<<<<<??
herbs07$percent_total_cover <- percent_of_total_cover #add this percent of total area to the original

#########################################################
# THIS WORKS !!! DONT TOUCH IT 10:20 PM 10-6-18
#########################################################

####################################################################################################################
#adjust the trees to be in basal area
trees07 <- dat.07[which(dat.07$datset == 'T'),]
trees07 <- na.omit(trees07) #get rid of the na's because they mess up the number of rows later
trees07 <- trees07[which(!trees07$cover == 0),] #get rid of rows that have no trees07 reported
#plot area = 33.82 sq ft
BA <- c() #calculate the basal area (just the area of the trunk)
for (c in trees07$cover){
  c <- c * 0.0328084 #converting the cm DBH into feet  (so the final area is in sq feet)
  basal_area <- pi * (c/2)**2
  BA <- c(BA, basal_area)
}
trees07$BA <- BA #add a column for BA

tree_plots <- data.frame(unique(trees07$plot)) #get all the tree plots
total_cover <- c() #doing the same thing i did for herbs, take all the basal areas in a plot and
for (plt in tree_plots$unique.trees07.plot.){ #... add them together so you have the total area
  temp <- sum(trees07$cover[which(trees07$plot == plt)]) # .. that is occupied by trees07
  total_cover <- c(total_cover, temp)
}
tree_plots$total_cover <- total_cover
plot_tree_cover <- c()
for (plt in trees07$plot){
  plot_tree_cover <- c(plot_tree_cover, tree_plots$total_cover[which(plt == tree_plots$unique.trees07.plot.)])
}
trees07$plot_tree_cover <- plot_tree_cover

percent_of_total_cover <- c()
for(x in 1:length(trees07$plot)){ #for each spp in the original data
  temp <-
    trees07$cover[x]/tree_plots$total_cover[which(tree_plots$unique.trees07.plot. == trees07$plot[x])]
  #divide the cover that species occupies by the total tree cover value for that plot
  percent_of_total_cover <- c(percent_of_total_cover, temp)
}
trees07$percent_total_cover <- percent_of_total_cover

#####################################################################################################################
#adjust shrubs to be in percent cover
shrubs07 <- dat.07[which(dat.07$datset == 'S'),]
shrub_plots <- data.frame(unique(shrubs07$plot)) #get all the shrub plots
total_cover <- c()
for (plt in shrub_plots$unique.shrubs07.plot.){ #for each shrub plot I find the sum of all the percent covers
  temp <- sum(shrubs07$cover[which(shrubs07 == plt)]) #this will be over 100% for some plots
  total_cover <- c(total_cover, temp)
}
shrub_plots$total_cover <- total_cover #I make a new dataframe that has the total cover for each plot

shrubs07 <- shrubs07[which(!shrubs07$cover == 0),] #I get rid of any rows that I won't be able to divide on
percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(cover in shrubs07$cover){ #...each spp in the plot takes up
  ptc <- cover/shrub_plots$total_cover[which(shrub_plots$unique.shrubs07.plot. == shrubs07$plot[cover])]
  percent_of_total_cover <- c(percent_of_total_cover, ptc)
}

shrubs07$percent_total_cover <- percent_of_total_cover #add this percent of total area to the original

# DO FOR 2018
#####################################################################################################
# herbs 2018
herbs18 <- dat.18[which(dat.18$datset == 'H'),]
herb_plots <- data.frame(unique(herbs18$plot))

total_cover <- c()
for (i in herb_plots$unique.herbs18.plot.){
  temp <- sum(herbs18[which(herbs18$plot == i),'cover'])
  total_cover <- c(total_cover, temp/2)
}
herb_plots$total_cover <- total_cover

plot_herb_cover <- c()
for (plt in herbs18$plot){
  plot_herb_cover <- c(plot_herb_cover, herb_plots$total_cover[which(plt == herb_plots$unique.herbs18.plot.)])
}
herbs18$plot_herb_cover <- plot_herb_cover

herbs18 <- herbs18[which(!herbs18$cover == 0),]
percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(cover in herbs18$cover){ #...each spp in the plot takes up
  ptc <- cover/herb_plots$total_cover[which(herb_plots$unique.herbs18.plot. == herbs18$plot[cover])] 
  percent_of_total_cover <- c(percent_of_total_cover, ptc)
} 
herbs18$percent_total_cover <- percent_of_total_cover #add this percent of total area to the original

##########################################################################################################
#adjust the trees to be in basal area
trees18 <- dat.18[which(dat.18$datset == 'T'),]
trees18 <- na.omit(trees18) #get rid of the na's because they mess up the number of rows later
trees18 <- trees18[which(!trees18$cover == 0),] #get rid of rows that have no trees18 reported
#plot area = 33.82 sq ft
BA <- c() #calculate the basal area (just the area of the trunk)
for (c in trees18$cover){
  c <- c * 0.0328084 #converting the cm DBH into feet  (so the final area is in sq feet)
  basal_area <- pi * (c/2)**2
  BA <- c(BA, basal_area)
}
trees18$cover <- BA #replace the DBH values with the basal area. we don't care about dbh anymore

tree_plots <- data.frame(unique(trees18$plot)) #get all the tree plots
total_cover <- c() #doing the same thing i did for herbs, take all the basal areas in a plot and
for (plt in tree_plots$unique.trees18.plot.){ #... add them together so you have the total area
  temp <- sum(trees18$cover[which(trees18$plot == plt)]) # .. that is occupied by trees18
  total_cover <- c(total_cover, temp)
}
tree_plots$total_cover <- total_cover
plot_tree_cover <- c()
for (plt in trees18$plot){
  plot_tree_cover <- c(plot_tree_cover, tree_plots$total_cover[which(plt == tree_plots$unique.trees18.plot.)])
}
trees18$plot_tree_cover <- plot_tree_cover

percent_of_total_cover <- c()
for(x in 1:length(trees18$plot)){ #for each spp in the original data
  temp <-
    trees18$cover[x]/tree_plots$total_cover[which(tree_plots$unique.trees18.plot. == trees18$plot[x])]
  # divide the cover that species occupies by the total tree cover value for that plot
  percent_of_total_cover <- c(percent_of_total_cover, temp)
}
trees18$percent_total_cover <- percent_of_total_cover

##########################################################################################################
#adjust shrubs to be in percent cover

shrubs18 <- dat.18[which(dat.18$datset == 'S'),]
shrub_plots <- data.frame(unique(shrubs18$plot)) #get all the shrub plots
total_cover <- c()
for (plt in shrub_plots$unique.shrubs18.plot.){ #for each shrub plot I find the sum of all the percent covers
  temp <- sum(shrubs18$cover[which(shrubs18$plot == plt)]) #this will be over 100% for some plots
  total_cover <- c(total_cover, temp)
}
shrub_plots$total_cover <- total_cover #I make a new dataframe that has the total cover for each plot

shrubs18 <- shrubs18[which(!shrubs18$cover == 0),] #I get rid of any rows that I won't be able to divide on
percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(i in 1:length(shrubs18$plot)){ #...each spp in the plot takes up
  percent_of_total_cover <-
    c(percent_of_total_cover, ((shrubs18[i,'cover'])/(shrub_plots[which(shrub_plots$unique.shrubs18.plot. == shrubs18[i,'plot']), 'total_cover'])))
}

shrubs18$percent_total_cover <- percent_of_total_cover #add this percent of total area to the original

