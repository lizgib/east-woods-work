#DATA WRANGLING 
# read in data from various sources to create plot by plot dataframe for envt variables
#OUTPUT: liz_data.csv
library(dict)
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/18.cover.R')
point_info_GIS <- read.csv('~/Documents/morton arb/east_woods_phylogeny/DATA/Analyses_Rollinson/point_info_GIS.csv')

plots <- point_info_GIS$PlotID
plots <- gsub('-', '', plots)
liz_data <- data.frame(plots)

######################
# LATITUDE/LONGITUDE 
######################
liz_data$lat <- point_info_GIS$lat
liz_data$lon <- point_info_GIS$lon

###########################
# SLOPE, ASPECT, ELEVATION 
###########################
liz_data$elevation <- point_info_GIS$elev
liz_data$slope <- point_info_GIS$slope
liz_data$aspect <- point_info_GIS$aspect
liz_data$area_name <- point_info_GIS$AreaName
liz_data$com_class <- point_info_GIS$ComClass

##################
# BURN FREQUENCY 
##################
burn_data <- read.csv('~/Documents/morton arb/east_woods_phylogeny/DATA/Analyses_Rollinson/point_info_GIS_burnhistory_2017-12.csv')
burned_plots <- burn_data$PlotID[which(!is.na(burn_data$Burn_Date))]
burned_plots <- gsub('-', '', burned_plots)
burned_plots <- as.data.frame(sort(table(burned_plots)))
liz_data$burn_count <- burned_plots$Freq[match(liz_data$plots, burned_plots$burned_plots)]

##################
# SOIL DATA
##################
soil_data <- read.csv('~/Documents/morton arb/east_woods_phylogeny/DATA/Analyses_Rollinson/point_info_GIS_soils.csv')
soil_data$PlotID <- gsub('-', '', soil_data$PlotID) 
soil_data <- soil_data[match(liz_data$plots, soil_data$PlotID),]
liz_data$soil_texture <- soil_data$texture
liz_data$drainage <- soil_data$Drainage

######################
# DOMINANT TREE GROUP
######################
marlin_data <- read.csv('~/Documents/morton arb/east_woods_phylogeny/DATA/marlins_data.csv')
marlin_data$Plot <- gsub('-', '', marlin_data$Plot)
marlin_data <- marlin_data[match(liz_data$plots, marlin_data$Plot),]
liz_data$tree_group <- marlin_data$Grp

trees07$genus <- gsub(' .*', '', trees07$accepted_name)

# go through each plot in liz_data
# for all the trees in each plot, which has the max BA 

tree_type_plots_07 <- data.frame()
for (plt in unique(trees07$plot)){
  dom <- max(trees07$percent_total_cover[which(trees07$plot == plt)])
  tree_type <- unique(trees07$genus[which(trees07$percent_total_cover == dom)])
  temp <- data.frame(plt, tree_type)
  tree_type_plots_07 <- rbind(tree_type_plots_07, temp)
}

liz_data$tree_group_07 <- tree_type_plots_07$tree_type[match(liz_data$plots, tree_type_plots_07$plt)]

trees18$genus <- gsub(' .*', '', trees18$accepted_name)

tree_type_plots_18 <- data.frame()
for (plt in unique(trees18$plot)){
  dom <- max(trees18$percent_total_cover[which(trees18$plot == plt)])
  tree_type <- unique(trees18$genus[which(trees18$percent_total_cover == dom)])
  temp <- data.frame(plt, tree_type)
  tree_type_plots_18 <- rbind(tree_type_plots_18, temp)
}

liz_data$tree_group_18 <- tree_type_plots_18$tree_type[match(liz_data$plots, tree_type_plots_18$plt)]

##################
# CANOPY COVER 
##################

# get the marlin canopy values in for comparison
liz_data$marlin_canopy <- marlin_data$CanopyOpenness[match(marlin_data$Plot, liz_data$plots)]

# get the plot_tree_cover for 2007 plots 
trees07$plot <- gsub('-','',trees07$plot)
plot_tree_cover_07 <- data.frame(trees07$plot)
plot_tree_cover_07$plot_tree_cover <- trees07$plot_tree_cover
plot_tree_cover_07 <- unique(plot_tree_cover_07)

liz_data$canopy_07 <- plot_tree_cover_07$plot_tree_cover[match(liz_data$plots, plot_tree_cover_07$trees07.plot)]

# get the plot_tree_cover for 2018 plots 
plot_tree_cover_18 <- data.frame(trees18$plot)
plot_tree_cover_18$plot_tree_cover <- trees18$plot_tree_cover
plot_tree_cover_18 <- unique(plot_tree_cover_18)

liz_data$canopy_18 <- plot_tree_cover_18$plot_tree_cover[match(liz_data$plots, plot_tree_cover_18$trees18.plot)]


##################################################################################################################

#############
# INVASIVES  
#############
source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/16.speciesinfo.R')
# make invasive species vect from the species info dataframe
invasives <- spp_info$Accepted_name[which(spp_info$native == 'i')]

## first find total plot invasive cover 
# make vector total invasive cover (throughout east woods) for comparison between years 

### NOTE: I AM ONLY EVALUATING HERBS RIGHT NOW 

###################
# RUNNING INTO PROBLEMS ALREADY... MY TOTAL COVER IS 2X its supposed to be????
#### I TEMPORARILY FIXED THIS TODAY 10/28.... just divided the plot total cover value by 2...
#### I NEED TO GO BACK TO THE COVER SCRIPT AND SEE WHY THIS IS HAPPENING!!!!! 
#### ALSO need to check the other covers (shrubs and trees) to make sure theyre not double as well... 
########### COULD SCREW UP CANOPY TOOOOO!!!!!! 
###################

# 2007 

# for each plot in liz_data
invasive_cover <- data.frame()
for(p in sort(liz_data$plots)){
  slice <- herbs07[which(herbs07$plot == p),]
  slice$cover <- as.numeric(slice$cover)
  temp <- sum(slice$cover[which(slice$accepted_name %in% invasives)])
  plotlist <- cbind(p, temp)
  invasive_cover <- rbind(invasive_cover, plotlist)
}
liz_data$plot_invasive_cover_07 <- invasive_cover$temp[match(liz_data$plots, invasive_cover$p)]

## get the total plot cover 
liz_data$total_herb_cover_07 <- herbs07$plot_herb_cover[match(liz_data$plots, herbs07$plot)]
liz_data$invasive_ratio_07 <- as.numeric(liz_data$plot_invasive_cover_07)/as.numeric(liz_data$total_herb_cover_07)
# THIS CALUCLATION DOESNT LOOK RIGHT..... ^^^ 4:31 PM 10/28 

# 2018 

invasive_cover <- data.frame()
for(p in sort(liz_data$plots)){
  slice <- herbs18[which(herbs18$plot == p),]
  slice$cover <- as.numeric(slice$cover)
  temp <- sum(slice$cover[which(slice$accepted_name %in% invasives)])
  plotlist <- cbind(p, temp)
  invasive_cover <- rbind(invasive_cover, plotlist)
}
liz_data$plot_invasive_cover_18 <- invasive_cover$temp[match(liz_data$plots, invasive_cover$p)]

## get the total plot cover 
liz_data$total_herb_cover_18 <- herbs18$plot_herb_cover[match(liz_data$plots, herbs18$plot)]
liz_data$invasive_ratio_18 <- as.numeric(liz_data$plot_invasive_cover_18)/as.numeric(liz_data$total_herb_cover_18)



