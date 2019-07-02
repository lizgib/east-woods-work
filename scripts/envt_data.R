library(ggplot2)
library(plyr)
library(magrittr)
library(readxl)
library(dbplyr)
# Get all the environmental data/plot metadata processed in one place

plots.env <- read.csv('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Rollinson/data_processed/point_info_GIS.csv', row.names = 7)
row.names(plots.env) <- gsub('-', '', row.names(plots.env))

plots.env <- plots.env[which(rownames(plots.env)!= 'BB115'),] # this plot is not anywhere in survey data. removing it

#---------------------------------------
# Burning/Management 

plots.env$MgmtUnit <- ifelse(is.na(plots.env$wooded), "Non-Wooded",
                             ifelse(plots.env$wooded=="Hidden Lake", "Hidden Lake",
                                    ifelse(plots.env$unit=="South 40 South", "Annual Burn",
                                           ifelse(!is.na(plots.env$unit), "Mixed Management", "No Management"))))


#-----------------------------------------
# Dominant tree genus

dat.all <- read.csv('data/species/dat.all.csv', as.is = T)
tree07 <- dat.all[which(dat.all$datset == 'T' & dat.all$year == '2007' & dat.all$sample_period == 'SUM'),]
tree18 <- dat.all[which(dat.all$datset == 'T' & dat.all$year == '2018'),]


source('scripts/cover.R')   

tree_type_plots_18 <- get_dominant_tree_group(tree18)
plots.env$DomGenus18 <- tree_type_plots_18$tree_type[match(row.names(plots.env), tree_type_plots_18$unique.tree.dat.plot.)]
tree_type_plots_07 <- get_dominant_tree_group(tree07)
plots.env$DomGenus07 <- tree_type_plots_07$tree_type[match(row.names(plots.env), tree_type_plots_07$unique.tree.dat.plot.)]


#------------------------------------------
# Oak or Acer plot? 
# since there are a lot of different genuses look only at oak and maple for dominant classes and less common ones all grouped as 'other'


oak_hickory <- c('Quercus', 'Carya', 'Cornus', 'Crataegus', 'Liriodendron', 'Juglans')
beech_maple <- c('Acer', 'Ulmus', 'Tilia', 'Prunus')
#beech_maple <- c('Acer', 'Quercus rubra')
plots.env$ForType18 <- ifelse(plots.env$DomGenus18 %in% oak_hickory, "Oak-Hickory",
                              ifelse(plots.env$DomGenus18 %in% beech_maple, "Beech-Maple",
                                     (ifelse(is.na(plots.env$DomGenus18), 'Other', 'Other'))))

plots.env$ForType07 <- ifelse(plots.env$DomGenus07 %in% oak_hickory, "Oak-Hickory",
                             ifelse(plots.env$DomGenus07 %in% beech_maple, "Beech-Maple",
                                    (ifelse(is.na(plots.env$DomGenus07), 'Other', 'Other'))))

plots.env$DomGenus18 <- as.character(plots.env$DomGenus18)
plots.env$DomGenus18[is.na(plots.env$DomGenus18)] <- 'No trees'
plots.env$DomGenus07 <- as.character(plots.env$DomGenus07)
plots.env$DomGenus07[is.na(plots.env$DomGenus07)] <- 'No trees'

#------------------------------------------
# Tree Cover 

tree07 <- get_plot_cover(tree07)
tree18 <- get_plot_cover(tree18)

tree07cover <- tree07[,c(1,9)]
tree07cover <- unique(tree07cover)
tree18cover <- tree18[,c(1,9)]
tree18cover <- unique(tree18cover)

missing_plots <- as.data.frame(rownames(plots.env)[which(!rownames(plots.env)%in% tree07cover$plot)])
names(missing_plots) <- c('plot')
missing_plots$plot_cover <- 0
test <- rbind(tree07cover, missing_plots)

plots.env$TreeCover07 <- test$plot_cover[match(rownames(plots.env), test$plot)]


missing_plots <- as.data.frame(rownames(plots.env)[which(!rownames(plots.env)%in% tree18cover$plot)])
names(missing_plots) <- c('plot')
missing_plots$plot_cover <- 0
test <- rbind(tree18cover, missing_plots)

plots.env$TreeCover18 <- test$plot_cover[match(rownames(plots.env), test$plot)]


#-----------------------------------------
# Invasive removal? 
#-----------------------------------------
# Invasive presence 
# this is set up to go but I really dont think I can use this abundance data will add it to the dataframe anyways and try it out maybe? 
# 
# 
# InvCover18 <- get_invasive_cover(plots.env, understory18)
# InvCover07 <- get_invasive_cover(plots.env, understory07)
#--------------------------------------
# Soil 

plots.soil <- read.csv('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Rollinson/data_processed/point_info_GIS_soils.csv')
plots.soil$PlotID <- gsub('-', '', plots.soil$PlotID)
plots.env$Drainage <- plots.soil$Drainage[match(rownames(plots.env), plots.soil$PlotID)]
plots.env$texture <- plots.soil$texture[match(rownames(plots.env), plots.soil$PlotID)]
plots.env$consistenc <- plots.soil$consistenc[match(rownames(plots.env), plots.soil$PlotID)]
plots.env$Permeabili <- plots.soil$Permeabili[match(rownames(plots.env), plots.soil$PlotID)]
plots.env$wtrhldgcap <- plots.soil$wtrhldgcap[match(rownames(plots.env), plots.soil$PlotID)]
plots.env$watertbl <- plots.soil$watertbl[match(rownames(plots.env), plots.soil$PlotID)]
plots.env$orgmatter <- plots.soil$orgmatter[match(rownames(plots.env), plots.soil$PlotID)]
plots.env$rootdevdep <- plots.soil$rootdevdep[match(rownames(plots.env), plots.soil$PlotID)]

#--------------------------------------
# Burn frequency

# this way of doing it won't work... all the plots are accounted for even if they were burned or not

plots.burn <- read.csv('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Rollinson/data_processed/point_info_GIS_burnhistory.csv')
plots.burn$PlotID <- gsub('-', '', plots.burn$PlotID)
burn_counts <- as.data.frame(table(plots.burn$PlotID[which(!is.na(plots.burn$Burn_Date))]))
plots.env$BurnCount <- burn_counts$Freq[match(rownames(plots.env), burn_counts$Var1)]
plots.env$BurnCount[is.na(plots.env$BurnCount)] <- 0


#--------------
# checking out the new soils data!! 

sheet_A <- read_excel('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Midgley/2018EWSurvey_BDGSMLOI_20190208_DRAFT.xlsx', sheet = 'data entry sheet_A')
sheet_B <- read_excel('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Midgley/2018EWSurvey_BDGSMLOI_20190208_DRAFT.xlsx', sheet = 'data entry sheet_B')
sheet_2 <- read_excel('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Midgley/2018EWSurvey_BDGSMLOI_20190208_DRAFT.xlsx', sheet = 'Sheet2')


# appending two different columns for each variable since there is a top soil and a lower soil measure for each

# extract the dry weight of each sample and the % organic matter

# using sheet B because it looked more standardized for each sample

topsoil <- sheet_B[which(sheet_B$`layer (cm)` == '0-10'),]
deepsoil <- sheet_B[which(sheet_B$`layer (cm)` == '10-20'),]


# there are a lot of NAs in these columns due to missing values in the spreadsheet 
# leaving this code as is, but will have to change it later if there is no value after 
# Dr. Midgely is done processing it
plots.env$TopOrgMat <- topsoil$`% OM`[match(rownames(plots.env), topsoil$`Plot ID`)]
plots.env$LowOrgMat <- deepsoil$`% OM`[match(rownames(plots.env), deepsoil$`Plot ID`)]

plots.env$TopDryWgt <- topsoil$DWE[match(rownames(plots.env), topsoil$`Plot ID`)]
plots.env$LowDryWgt <- deepsoil$DWE[match(rownames(plots.env), deepsoil$`Plot ID`)]

# also throwing in the % water content even though she said it might not be the best data (collected
# over a month so not as reliable)

plots.env$TopWtrContent <- topsoil$`residual water content (%)`[match(rownames(plots.env), topsoil$`Plot ID`)]
plots.env$LowWtfContent <- deepsoil$`residual water content (%)`[match(rownames(plots.env), deepsoil$`Plot ID`)]

#------
# adding in distance to edge variables (computed in QGIS)

# dist to path (in meters)

path_dist <- read.csv('~/Desktop/dist_to_path.csv')
path_dist <- path_dist[,c('field_1','HubDist')]
names(path_dist) <- c('PlotID', 'PathDist')
plots.env$PathDist <- path_dist$PathDist[match(rownames(plots.env), path_dist$PlotID)]


road_dist <- read.csv('~/Desktop/dist_to_road.csv')
road_dist <- road_dist[,c('field_1','HubDist')]
names(road_dist) <-  c('PlotID', 'RoadDist')
plots.env$RoadDist <- road_dist$RoadDist[match(rownames(plots.env), road_dist$PlotID)]

road_or_path <- read.csv('~/Documents/GIS/dist_to_road_or_path.csv')
road_or_path <- road_or_path[,c('field_1', 'HubDist')]
plots.env$RoadPathDist <- road_or_path$HubDist[match(rownames(plots.env), road_or_path$field_1)]

#--------
# Solar Radiation (from ArcGIS Points Solar Radiation function)

# in Wh/m2, measurements estimated for dates from 4/30/2018 (first day of spring survey) to 8/24/2018 (last day of summmer survey)

sol_rad <- read.csv('~/Documents/GIS/pt_sol_rad_coord.csv')
plots.env$SolRad <- sol_rad$T0[match(plots.env$x.utm16, sol_rad$xcoord)]

ggplot(data = plots.env, aes(x = lon, y = lat, size = SolRad, col = SolRad))+
  geom_point() + 
  coord_equal()


save(plots.env, file = 'data/plots.env.RData')
write.csv(plots.env, '~/Documents/GitHub/east_woods_work/data/plots.env.csv')
