#removing outliers from the marlin data to see if this improves mantel results 

library(vegan)
source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/newstuff/00.read_data.R')
source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/oldstuff/marlin_plots.R')

# marlin_data <- na.omit(marlin_data)
# canopy_dist <- dist(marlin_data$CanopyOpenness, method = 'euclidean')
# canopy_ord <- metaMDS(canopy_dist)
# 
# aspect_dist <- dist(marlin_data$Aspect, method = 'euclidean')
# aspect_ord <- metaMDS(aspect_dist) 
# #when I plot out the aspect ordination it looks pretty clean/no clear outliers 
# 
# slope_dist <- dist(marlin_data$Slope, method = 'euclidean')
# slope_ord <- metaMDS(slope_dist)
# #again...pretty clean I think? everything basically in line along mDS2 but MDS1 got some patchy stuff < -4
# #still I think its pretty good
# 
# elevation_dist <- dist(marlin_data$Elevation, method = 'euclidean')
# elevation_ord <- metaMDS(elevation_dist)
# #pretty clean 
# 
# ivansives_dist <- dist(marlin_data$AlienNativeRatio, method = 'euclidean')
# invasives_ord <- metaMDS(ivansives_dist)
# #definitelysome outliers here 
# #REMOVE points > 2 in dim MDS1


##########################################################
#Spatial distances 
coord$CORNER <- gsub('-', '', coord$CORNER)
coordinates_df <- coord[which(coord$CORNER %in% intersect(coord$CORNER, rownames(marlin_data))),]
coordinates_df <- coordinates_df[, c(-1,-2,-4,-5,-6,-7,-8,-9, -10, -11)]
rownames(coordinates_df) <- coordinates_df$CORNER
worsk <- dist(coordinates_df, diag = T, upper = T)
dist_ord <- metaMDS(worsk)
#dist_ord$points[which(dist_ord$points$MDS1 > 5),]
