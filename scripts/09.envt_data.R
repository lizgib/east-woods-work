#DATA WRANGLING 
# read in data from various sources to create plot by plot dataframe for envt variables
#OUTPUT: liz_data.csv
understory.all <- read.csv('data/understory.all.csv')
trees.all <- read.csv('data/trees.all.csv')
point_info_GIS <- read.csv('~/Documents/GitHub/east_woods_work/data/plot_data/Analyses_Rollinson/point_info_GIS.csv', as.is = T)

point_info_GIS$PlotID <- gsub('-', '', point_info_GIS$PlotID)
plots <- point_info_GIS$PlotID
liz_data <- data.frame(sort(plots))
names(liz_data) <- c('plots')

######################
# LATITUDE/LONGITUDE 
######################
liz_data$lat <- point_info_GIS$lat[match(liz_data$plots,point_info_GIS$PlotID)]
liz_data$lon <- point_info_GIS$lon[match(liz_data$plots,point_info_GIS$PlotID)]

###########################
# SLOPE, ASPECT, ELEVATION 
###########################
liz_data$elevation <- point_info_GIS$elev[match(liz_data$plots,point_info_GIS$PlotID)]
liz_data$slope <- point_info_GIS$slope[match(liz_data$plots,point_info_GIS$PlotID)]
liz_data$aspect <- point_info_GIS$aspect[match(liz_data$plots,point_info_GIS$PlotID)]


##################
# BURN FREQUENCY 
##################
burn_data <- read.csv('~/Documents/GitHub/east_woods_work/data/plot_data/Analyses_Rollinson/point_info_GIS_burnhistory_2017-12.csv', as.is = T)
burned_plots <- burn_data$PlotID[which(!is.na(burn_data$Burn_Date))]
burned_plots <- gsub('-', '', burned_plots)
burned_plots <- as.data.frame(sort(table(burned_plots)))
liz_data$burn_count <- burned_plots$Freq[match(liz_data$plots, burned_plots$burned_plots)]

##################
# CANOPY COVER 
##################
trees07 <- trees.all[which(trees.all$year == '2007'),]
trees18 <- trees.all[which(trees.all$year == '2018'),]

# get the plot_tree_cover for 2007 plots 
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

liz_data$invasives07 <- invasives07$cover[match(liz_data$plots, invasives07$plot)]
liz_data$invasives18 <- invasives18$cover[match(liz_data$plots, invasives18$plot)]

##################
# SOIL DATA
##################
soil_data <- read.csv('~/Documents/GitHub/east_woods_work/data/plot_data/Analyses_Rollinson/point_info_GIS_soils.csv', as.is = T)
soil_data$PlotID <- gsub('-', '', soil_data$PlotID) 
soil_data <- soil_data[match(liz_data$plots, soil_data$PlotID),]
liz_data$soil_texture <- soil_data$texture
liz_data$drainage <- soil_data$Drainage

######################
# DOMINANT TREE GROUP
######################
marlin_data <- read.csv('~/Documents/GitHub/east_woods_work/data/plot_data/marlins_data.csv', as.is = T)
marlin_data$Plot <- gsub('-', '', marlin_data$Plot)
marlin_data <- marlin_data[match(liz_data$plots, marlin_data$Plot),]
liz_data$tree_group <- marlin_data$Grp

trees07 <- dat.07[which(dat.07$datset == 'T'),]
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

trees18 <- dat.18[which(dat.18$datset == 'T'),]
trees18$genus <- gsub(' .*', '', trees18$accepted_name)

tree_type_plots_18 <- data.frame()
for (plt in unique(trees18$plot)){
  dom <- max(trees18$percent_total_cover[which(trees18$plot == plt)])
  tree_type <- unique(trees18$genus[which(trees18$percent_total_cover == dom)])
  temp <- data.frame(plt, tree_type)
  tree_type_plots_18 <- rbind(tree_type_plots_18, temp)
}

liz_data$tree_group_18 <- tree_type_plots_18$tree_type[match(liz_data$plots, tree_type_plots_18$plt)]

#############
# SOIL INDEX 
#############

# only two different soil textures (silt loam and silt clay loam)-- giving them IDs 1 and 2 respectively

liz_data$soil_texture <- gsub('silt loam', 1, liz_data$soil_texture)
liz_data$soil_texture <- gsub('silty clay loam', 2, liz_data$soil_texture)

liz_data$drainage <- gsub('very poorly drained', 1, liz_data$drainage)
liz_data$drainage <- gsub('poorly drained', 2, liz_data$drainage)
liz_data$drainage <- gsub('moderately well-drained', 3, liz_data$drainage)
liz_data$drainage <- gsub('well-drained', 4, liz_data$drainage)
liz_data$drainage <- gsub('excellently drained', 5, liz_data$drainage)

liz_data$drainage <- as.numeric(liz_data$drainage)
liz_data$soil_texture <- as.numeric(liz_data$soil_texture)

# ^^^ not working rn 11/30 but I dont have time to fix it right now 
liz_data$soil_index <- liz_data$soil_texture * liz_data$drainage

####################
# PERCENT ACM ECM  
###################
percent_acm <- c()

# for each tree in the plot

# if the genus in ACM trees

# sum the total cover 

# divide the sum of ACM trees by the total plot tree cover 

# add to percent_acm 


####################
# SOME OTHER STUFF 
###################
liz_data$area_name <- point_info_GIS$AreaName
liz_data$com_class <- point_info_GIS$ComClass
liz_data$soil_index <- 0 # blank for now. this will be continuous translation of the soil variables 
liz_data$geo_drainage <- 0 # also will be continous coding of some soil variables and combination of slope aspect and elevation 
liz_data$ECM_ACM <- 0 # continuous, percent of trees which are ACM or ECM 


# there is an extra plot in here that the species dataframes

liz_data <- liz_data[which(liz_data$plots %in% unique(dat.all$plot)),]
liz_data$plots == sort(unique(dat.all$plot))
