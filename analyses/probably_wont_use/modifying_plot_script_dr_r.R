
# Libraries
library(raster); library(rgdal); library(rgeos)
library(lubridate)

# File paths
path.local <- "~/Documents/morton arb/east_woods_phylogeny/analyses/"
path.gis <- "~/Documents/morton arb/east_woods_phylogeny/analyses/GIS_files/"

path.maps <- "maps/"
setwd(path.local)


# Useful layers:
# # IMLS plots found in Bob Fahey's files on Shared Drive; now copied locally
# NOTE: 2 sets -- all plots and then some that look like they've been filtered out b/c of roads etc
# plot(candidates)
# summary(candidates)
# #candidates <- readOGR('GIS_files/mygeodata/cleaneduplatlon.shp')
# # Extracting the sites we're using
# candidates$PlotID <- as.factor(paste0(candidates$stand, candidates$order))
# selected <- data.frame(stand=c("A", "B", "C", "D"),
#                         order=c( 1 ,  5 ,  6 ,  1))
# selected$PlotID <- as.factor(paste0(selected$stand, selected$order))
# selected.pts <- candidates[candidates$PlotID %in% selected$PlotID,]
#candidates <- mdf
# DEM 
#dem <- raster("/Volumes/GIS/Collections/DEMs/ewoods/") # Elevation looks like its in feet
candidates <- readOGR('../../../../Documents/mygeodata/lat_lon_for_marlin_plts.shp')
#candidates <- readOGR('../../../../Documents/CandidatePlots.shp')
dem <- raster('GIS_files/DEMs/ewoods/')
# Water, streams
#water <- readOGR("/Volumes/GIS/Collections/hydrology/water.shp")
#water <- readOGR('GIS_files/')
#water <- readOGR("/Volumes/GIS/Collections/hydrology/wat_co_arboretum.shp")
#pools <- readOGR("/Volumes/GIS/Collections/hydrology/wat_pools_seasonal.shp")
#streams.perm <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_permanent.shp")
#streams.int <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_seasonal.shp")

# Roads, Trails
#roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_2011-2020_ctrln.shp")
roads <- readOGR('GIS_files/roads/circ_veh_rd_2011-2020_ctrln.shp')
#paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
paths <- readOGR('GIS_files/trails/paths.shp')
#parking <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/parking_lots.shp")
summary(roads)
summary(paths)

# Property & Woodland boundarys
#property <- readOGR("/Volumes/GIS/Collections/boundaries/Morton_Arboretum.shp")
property <- readOGR('GIS_files/MortonArb_boundary/Morton_Arboretum.shp')
#woods <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/2008 vegetative cover type/Woodland.shp")
woods <- readOGR('GIS_files/east_woods/Woodland.shp')
woods <- woods[2,] # We only want to worry about the main block; row 1 = King's Grove, row 2= main tract; row 3 = weird patch
# mgmt  <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Boundaries/New Management Units.shp")
# harvest <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Canopy Thinning/Canopy Thinning.shp")
# burn <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Burn/Burned_Area.shp")
# summary(harvest)

# Transforming everythign to the DEM crs just to make life easier
candidates   <- spTransform(candidates, projection(dem))
#selected.pts <- spTransform(selected.pts, projection(dem))
#water        <- spTransform(water, projection(dem))
#pools        <- spTransform(pools, projection(dem))
#streams.perm <- spTransform(streams.perm, projection(dem))
#streams.int  <- spTransform(streams.int, projection(dem))
roads        <- spTransform(roads, projection(dem))
paths        <- spTransform(paths, projection(dem))
woods        <- spTransform(woods, projection(dem))
property     <- spTransform(property, projection(dem))
#parking      <- spTransform(parking, projection(dem))
# ---------------------------------------


# ---------------------------------------
# Making an updated, pretty map for files
# ---------------------------------------
library(ggplot2)
library(scales)
library(ggspatial)

dem.df <- data.frame(rasterToPoints(dem))
summary(dem.df)

ewoods <- fortify(woods)
holes <- which(!duplicated(ewoods$group))

# Stolen from http://apps.fishandwhistle.net/archives/1126
fixfeature <- function(df) {
  ringstarts <- which(!duplicated(df$group))
  if(length(ringstarts) < 2) {
    return(df)
  } else {
    ringstarts <- c(ringstarts, nrow(df))
    indicies <- c(1:(ringstarts[2]-1), do.call(c, lapply(2:(length(ringstarts)-1), function(x) {
      c(1, ringstarts[x]:(ringstarts[x+1]-1))
    })), nrow(df))
    return(df[indicies,])
  }
}

ewoods <- fixfeature(ewoods)
summary(ewoods)

# Adding the x-y coordinates for our selected plots
# selected.pts <- selected.pts[order(selected.pts$PlotID),]
# data.frame(selected.pts)
# selected.pts$PlotID
# selected[,c("x", "y")] <- coordinates(selected.pts)
# summary(selected)

candidates$lat2 <- candidates$lat * 10000
candidates$long2 <- candidates$long * 10000

png("LizPlots_Locations.png", height=6, width=8, units="in", res=320)
p <- ggplot() +
  geom_raster(data=dem.df, aes(x=dem.df$x, y=dem.df$y, fill=ewoods)) +
  geom_polygon(data=ewoods, aes(x=long, y=lat, group=id), fill="green", alpha=0.3) +
  geom_path(data=property, aes(x=long, y=lat, group=group), color="green4", size=2) +
  geom_path(data=roads, aes(x=long, y=lat, group=group), color="ivory3", size=1) +
  #geom_path(data=roads, aes(x=long+5, y=lat+5, group=group), color="ivory", size=0.2) +
  geom_path(data=paths, aes(x=long, y=lat, group=group), color="tan4", size=0.7, linetype="dashed") +
  geom_point(data = candidates, aes(x=candidates$long2, y=candidates$lat2), color = 'blue', size = 5)+
  scale_fill_gradient(low="black", high="white", name="Elevation") +
  coord_equal(xlim=range(dem.df$x), ylim=c(4631000, min(dem.df$y)), expand=F) +
  theme_bw() +
  theme(legend.position="top",
        axis.title=element_blank())
print(p)
dev.off()

#### the problem im having is in the dimensions of the dem object. when I convert it to a dataframe, 
### alll the y valeus are this big 4631326 number and I can scale up my lat coordinates to match the 
### x values in dem.df but i have no idea where this y number is coming from and how i'm supposed to fit my  
### y coordinates to it....

#------------------------------------------------------------------------------------------------------
library(png)
eastwoods <- readPNG('LizPlots_Locations.png')
# my_image <-  readPNG(getURLContent('http://path.to/image.png'))
# p1 + annotation_raster(my_image, ymin = 4,ymax= 5,xmin = 30,xmax = 40)

p1 <- ggplot() +
  geom_point(data = candidates, aes(x=candidates$long2, y=candidates$lat2), color = 'red', size = 5) +
  coord_equal()


p2 <-p1 + annotation_raster(eastwoods, ymin = min(candidates$lat2), xmin = min(candidates$long2), xmax = max(candidates$long2), ymax = max(candidates$lat2))
  
  
  
  