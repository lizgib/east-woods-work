#DATA WRANGLING 
# read in data from various sources to create plot by plot dataframe for envt variables
#OUTPUT: liz_data.csv

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
marlin_data <- marlin_data[match(liz_data$plots, liz_data$plots),]
liz_data$tree_group <- marlin_data$Grp

for( plot in unique(trees07$plot)){
  dat.tmp <- dat.tree.all[dat.tree.all$PlotID==PLT & dat.tree.all$Status==STAT,]
  # Finding dominant species
  dat.tmp2 <- aggregate(dat.tmp$BA, by=dat.tmp[,c("Year", "Spp.Name")], FUN=sum)
  names(dat.tmp2)[which(names(dat.tmp2)=="x")] <- "BA"
  dat.tmp2$dens <- aggregate(dat.tmp$BA, by=dat.tmp[,c("Year", "Spp.Name")], FUN=length)[,"x"]
  
  dat.tmp2[dat.tmp2$Year==2007, "IV.BA"] <- dat.tmp2[dat.tmp2$Year==2007, "BA"]/sum(dat.tmp2[dat.tmp2$Year==2007, "BA"])
  dat.tmp2[dat.tmp2$Year==2007, "IV.dens"] <- dat.tmp2[dat.tmp2$Year==2007, "dens"]/sum(dat.tmp2[dat.tmp2$Year==2007, "dens"])
  dat.tmp2[dat.tmp2$Year==2018, "IV.BA"] <- dat.tmp2[dat.tmp2$Year==2018, "BA"]/sum(dat.tmp2[dat.tmp2$Year==2018, "BA"])
  dat.tmp2[dat.tmp2$Year==2018, "IV.dens"] <- dat.tmp2[dat.tmp2$Year==2018, "dens"]/sum(dat.tmp2[dat.tmp2$Year==2018, "dens"])
  
  dat.tmp2$IV.mean <- apply(dat.tmp2[,c("IV.BA", "IV.dens")], 1, mean)
  
  if(nrow(dat.tmp2[dat.tmp2$Year==2007,])>0){
    dat.plot[ind.2007,"Spp.Dom"] <- dat.tmp2[dat.tmp2$Year==2007 & dat.tmp2$IV.mean==max(dat.tmp2$IV.mean[which(dat.tmp2$Year==2007)]),"Spp.Name"]
  }
  if(nrow(dat.tmp2[dat.tmp2$Year==2018,])>0){
    dat.plot[ind.2018,"Spp.Dom"] <- dat.tmp2[dat.tmp2$Year==2018 & dat.tmp2$IV.mean==max(dat.tmp2$IV.mean[which(dat.tmp2$Year==2018)]),"Spp.Name"]
  }
  
  if(nrow(dat.plot[c(ind.2007,ind.2018),])<2) {
    dat.plot[ind.2018, "Spp.Change"] <- "CHANGE"
  } else {
    dat.plot[ind.2018, "Spp.Change"] <- ifelse(dat.plot[ind.2018,"Spp.Dom"]!=dat.plot[ind.2007,"Spp.Dom"], "CHANGE", "NO")
  }
} # End Status loop



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

###############
# HERB DENSITY 
###############






