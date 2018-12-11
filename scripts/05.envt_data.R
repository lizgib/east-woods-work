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
liz_data$canopy07 <- trees07$plot_cover[match(liz_data$plots, trees07$plot)]
# get the plot_tree_cover for 2018 plots 
liz_data$canopy18 <- trees18$plot_cover[match(liz_data$plots, trees18$plot)]

#------------------------------------------------------------------------------------------------------------------
#############
# INVASIVES  
#############
understory07 <- understory.all[which(understory.all$year == '2007'),]
understory18 <- understory.all[which(understory.all$year == '2018'),]

invasives07 <- understory07[which(understory07$nativestatus == 'i'),]
invasives07 <- rbind(invasives07, understory07[which(understory07$nativestatus == 'x'),])
invasives18 <- understory18[which(understory18$nativestatus == 'i'),]
invasives18 <- rbind(invasives18, understory18[which(understory18$nativestatus == 'x'),])

get_inv_cover <- function(dat){ # this function will take in the invasive dataframe just made
  total_cover <- 
    data.frame(
      cov = sapply(unique(dat$plot), function(x){
        sum(dat$cover[which(dat$plot == x)], na.rm = T)
      }
      ),
      row.names = unique(dat$plot)
    )
  total_cover$cov <- as.numeric(as.character(total_cover$cov)) # gives me the total cover of invasives in the plot
  dat$plot_cover <- total_cover$cov[match(dat$plot, row.names(total_cover))]
  return(dat)
}


inv_cover07 <- get_inv_cover(invasives07)
inv_cover18 <- get_inv_cover(invasives18)

understory07$invasive_cover <-inv_cover07$plot_cover[match(understory07$plot, inv_cover07$plot)]
understory18$invasive_cover <- inv_cover18$plot_cover[match(understory18$plot, inv_cover18$plot)]

understory07$inv_ratio07 <- understory07$invasive_cover/understory07$plot_cover
understory18$inv_ratio18 <- understory18$invasive_cover/understory18$plot_cover

liz_data$inv_ratio07 <- understory07$inv_ratio07[match(liz_data$plots, understory07$plot)]
liz_data$inv_ratio18 <- understory18$inv_ratio18[match(liz_data$plots, understory18$plot)]

#############
# SOIL DATA
############
soil_data <- read.csv('~/Documents/GitHub/east_woods_work/data/plot_data/Analyses_Rollinson/point_info_GIS_soils.csv', as.is = T)
soil_data$PlotID <- gsub('-', '', soil_data$PlotID) 
soil_data <- soil_data[match(liz_data$plots, soil_data$PlotID),]
liz_data$soil_texture <- soil_data$texture
liz_data$drainage <- soil_data$Drainage

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

######################
# DOMINANT TREE GROUP
######################
marlin_data <- read.csv('~/Documents/GitHub/east_woods_work/data/plot_data/marlins_data.csv', as.is = T)
marlin_data$Plot <- gsub('-', '', marlin_data$Plot)
marlin_data <- marlin_data[match(liz_data$plots, marlin_data$Plot),]
liz_data$tree_group <- marlin_data$Grp

trees07$genus <- gsub(' .*', '', trees07$accepted_name)

# go through each plot in liz_data
# for all the trees in each plot, which has the max BA 

tree_type_plots_07 <- data.frame()
for (plt in unique(trees07$plot)){
  dom <- max(trees07$spp_percent_total_cover[which(trees07$plot == plt)])
  tree_type <- unique(trees07$genus[which(trees07$spp_percent_total_cover == dom)])
  temp <- data.frame(plt, tree_type)
  tree_type_plots_07 <- rbind(tree_type_plots_07, temp)
}

liz_data$tree_group_07 <- tree_type_plots_07$tree_type[match(liz_data$plots, tree_type_plots_07$plt)]

trees18$genus <- gsub(' .*', '', trees18$accepted_name)

tree_type_plots_18 <- data.frame()
for (plt in unique(trees18$plot)){
  dom <- max(trees18$spp_percent_total_cover[which(trees18$plot == plt)])
  tree_type <- unique(trees18$genus[which(trees18$spp_percent_total_cover == dom)])
  temp <- data.frame(plt, tree_type)
  tree_type_plots_18 <- rbind(tree_type_plots_18, temp)
}

liz_data$tree_group_18 <- tree_type_plots_18$tree_type[match(liz_data$plots, tree_type_plots_18$plt)]

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
liz_data$geo_drainage <- liz_data$elevation + (liz_data$slope * liz_data$aspect) # also will be continous coding of some soil variables and combination of slope aspect and elevation 
liz_data$ECM_ACM <- 0 # continuous, percent of trees which are ACM or ECM 


# there is an extra plot in here that the species dataframes
dat.all <- rbind(trees.all, understory.all)
liz_data <- liz_data[which(liz_data$plots %in% unique(dat.all$plot)),]
#liz_data$plots == sort(unique(dat.all$plot))

write.csv(liz_data, 'data/liz_data.csv')

