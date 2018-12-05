#make my dataframe w plot, spp, accname, and cover
# ok so the gameplan here is that i will keep the cover of each veg type as a separate unit 
# each veg type will amount to 100% for each plot (so the sum of all plant %cover should = 100%, 
# the sum of all basal areas of trees in a plot = 100%, and the total number of stems for shrubs
# = 100%)
# each plot will have a total cover of 3 
# I am not going to be comparing the relative quantities of trees to shrubs to herbs 
# I am comparing the composition of shrubs, herbs, trees, between plots

source('~/Documents/GitHub/east_woods_work/scripts/02.falltranslationkey.R')
source('~/Documents/GitHub/east_woods_work/scripts/04.natives.R')
dat.all$cover <- as.numeric(dat.all$cover)

dat.07 <- dat.all[which(dat.all$year == '2007'),]
dat.18 <- dat.all[which(dat.all$year == '2018'),]

####################################################################################################################
# ok looking at cover for 2007 herb layer
herbs07 <- dat.07[which(dat.07$datset == 'H'),]
herb_plots <- data.frame(unique(herbs07$plot))

total_cover <- 
  data.frame(
    cov = sapply(unique(herbs07$plot), function(x){
      sum(herbs07$cover[herbs07$plot == x], na.rm = T)
    }
  )
)
total_cover$cov <- as.numeric(as.character(total_cover$cov))

herbs07$plot_herb_cover <- total_cover$cov[match(herbs07$plot, row.names(total_cover))]

herbs07$spp_percent_total_herb_cover <- herbs07$cover/herbs07$plot_herb_cover

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

total_cover <- 
  data.frame(
    cov = sapply(unique(trees07$plot), function(x){
      sum(trees07$BA[trees07$plot == x], na.rm = T)
    }
    #row.names = unique(trees07$plot)
    )
  )
total_cover$cov <- as.numeric(as.character(total_cover$cov))

trees07$plot_tree_cover <- total_cover$cov[match(trees07$plot, row.names(total_cover))]

trees07$percent_total_cover <- trees07$BA/trees07$plot_tree_cover

#####################################################################################################################
#adjust shrubs to be in percent cover
shrubs07 <- dat.07[which(dat.07$datset == 'S'),]
shrubs07 <- shrubs07[which(!shrubs07$cover == 0),] #I get rid of any rows that I won't be able to divide on
shrubs07 <- shrubs07[which(!is.na(shrubs07$cover)),]

total_cover <- 
  data.frame(
    cov = sapply(unique(shrubs07$plot), function(x){
      sum(shrubs07$cover[shrubs07$plot == x], na.rm = T)
    }
    )
  )
total_cover$cov <- as.numeric(as.character(total_cover$cov))

shrubs07$plot_shrub_cover <- total_cover$cov[match(shrubs07$plot, row.names(total_cover))]
shrubs07$percent_total_cover <- shrubs07$cover/shrubs07$plot_shrub_cover #add this percent of total area to the original

# DO FOR 2018
#####################################################################################################
# herbs 2018

herbs18 <- dat.18[which(dat.18$datset == 'H'),]
herb_plots <- data.frame(unique(herbs18$plot))

total_cover <- 
  data.frame(
    cov = sapply(unique(herbs18$plot), function(x){
      sum(herbs18$cover[herbs18$plot == x], na.rm = T)
    }
    #row.names = unique(herbs18$plot)
    )
  )
total_cover$cov <- as.numeric(as.character(total_cover$cov))

herbs18$plot_herb_cover <- total_cover$cov[match(herbs18$plot, row.names(total_cover))]

herbs18$spp_percent_total_herb_cover <- herbs18$cover/herbs18$plot_herb_cover

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
trees18$BA <- BA #add a column for BA

tree_plots <- data.frame(unique(trees18$plot)) #get all the tree plots
total_cover <- c() #doing the same thing i did for herbs, take all the basal areas in a plot and
for (plt in tree_plots$unique.trees18.plot.){ #... add them together so you have the total area
  temp <- trees18$BA[which(trees18$plot == plt)] # .. that is occupied by trees18
  temp2 <- sum(temp)
  total_cover <- c(total_cover, temp2)
}

tree_plots$total_cover <- total_cover
plot_tree_cover <- c()
for (plt in trees18$plot){
  plot_tree_cover <- c(plot_tree_cover, tree_plots$total_cover[which(plt == tree_plots$unique.trees18.plot.)])
}
trees18$plot_tree_cover <- plot_tree_cover

trees18$percent_total_cover <- trees18$BA/trees18$plot_tree_cover

##########################################################################################################
#adjust shrubs to be in percent cover

shrubs18 <- dat.18[which(dat.18$datset == 'S'),]
shrubs18 <- shrubs18[which(!shrubs18$cover == 0),] #I get rid of any rows that I won't be able to divide on
shrubs18 <- shrubs18[which(!is.na(shrubs18$cover)),]

total_cover <- 
  data.frame(
    cov = sapply(unique(shrubs18$plot), function(x){
      sum(shrubs18$cover[shrubs18$plot == x], na.rm = T)
    }
    #row.names = unique(herbs18$plot)
    )
  )
total_cover$cov <- as.numeric(as.character(total_cover$cov))

shrubs18$plot_shrub_cover <- total_cover$cov[match(shrubs18$plot, row.names(total_cover))]
shrubs18$spp_percent_total_shrub_cover <- shrubs18$cover/shrubs18$plot_shrub_cover #add this percent of total area to the original

##########################################################################################################

# invasive cover 

# 2007 

invasives <- dat.all[which(dat.all$nativestatus == 'i'),]
invasives07 <- invasives[which(invasives$year == '2007'),]
invasives18 <- invasives[which(invasives$year == '2018'),]

invasives07 <- invasives07[which(!is.na(invasives07$cover)),]
invasives07 <- invasives07[which(!invasives07$cover == 0),]

total_cover <- data.frame(
  cov = sapply(unique(invasives07$plot), function(x){
    sum(invasives07$cover[invasives07$plot == x], na.rm = T)
  }
  )
)
total_cover$cov <- as.numeric(total_cover$cov)
invasives$invasives07 <- total_cover$cov[match(invasives$plot, row.names(total_cover))]

# 2018

invasives18 <- invasives18[which(!is.na(invasives18$cover)),]
invasives18 <- invasives18[which(!invasives18$cover == 0),]

total_cover <- data.frame(
  cov = sapply(unique(invasives18$plot), function(x){
    sum(invasives18$cover[invasives18$plot == x], na.rm = T)
  }
  )
)
total_cover$cov <- as.numeric(total_cover$cov)
invasives$invasives18 <- total_cover$cov[match(invasives$plot, row.names(total_cover))]


