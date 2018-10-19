#make map  

library(vegan)
source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/oldstuff/messedup_marlin_plots.R')
diversity <- read.csv('OUTPUTS/diversity_metrics.csv')

marlinplots_c <- marlinplots[which(marlinplots$CanopyOpenness < 50),]
diversity1 <- diversity[which(diversity$X %in% intersect(diversity$X, marlinplots_c$Plot)),]
rownames(diversity1) <- diversity1$X
diversity1$lat <- marlinplots_c$marlinlat[which(marlinplots_c$Plot %in% intersect(marlinplots_c$Plot, rownames(diversity1)))]
diversity1$lon <- marlinplots_c$marlinlon[which(marlinplots_c$Plot %in% intersect(marlinplots_c$Plot, rownames(diversity1)))]
diversity1$smallerPD <- diversity1$PD/1000
diversity1$canopy <- as.numeric(marlinplots_c$CanopyOpenness[which(marlinplots_c$Plot %in% intersect(marlinplots_c$Plot, rownames(diversity1)))])

canopy_ord <- metaMDS(dist(diversity1$canopy, method = 'euclidean'))
MDS1 <- canopy_ord$points[1:nrow(diversity1),1]

# ggplot() +
#   aes(diversity$lat, diversity$lon, color = abs(MDS1), size = diversity$smallerPD) + 
#   geom_point() +
#   xlab('Latitude') + 
#   ylab('Longitude') + 
#   scale_size () + 
#   coord_equal() + 
#   scale_color_gradient(low = 'darkgreen', high = 'orange')

plt <- ggplot() +
  aes(diversity1$lon, diversity1$lat, color = abs(MDS1), size = diversity1$smallerPD) + 
  geom_point() +
  xlab('Longitude') + 
  ylab('Latitude') + 
  scale_size () + 
  coord_equal() + 
  scale_color_gradient(low = 'orange', high = 'yellow')
print(plt)
#############################################################################################################

marlinplots_in <- marlinplots[which(!is.na(marlinplots$AlienNativeRatio)),]
marlinplots_in <- marlinplots_in[which(marlinplots_in$AlienNativeRatio < 4),]
diversity2 <- diversity[which(diversity$X %in% intersect(diversity$X, marlinplots_in$Plot)),]
rownames(diversity2) <- diversity2$X
diversity2$lat <- marlinplots_in$marlinlat[which(marlinplots_in$Plot %in% intersect(marlinplots_in$Plot, rownames(diversity2)))]
diversity2$lon <- marlinplots_in$marlinlon[which(marlinplots_in$Plot %in% intersect(marlinplots_in$Plot, rownames(diversity2)))]
diversity2$smallerPD <- diversity2$PD/1000
diversity2$invasives <- as.numeric(marlinplots_in$AlienNativeRatio[which(marlinplots_in$Plot %in% intersect(marlinplots_in$Plot, rownames(diversity2)))])

invasives_ord <- metaMDS(dist(diversity2$invasives, method = 'euclidean'))
MDS1 <- invasives_ord$points[1:nrow(diversity2),1]


# ggplot() +
#   aes(diversity2$lon, diversity2$lat, color = abs(MDS1), size = diversity2$smallerPD) + 
#   geom_point() +
#   xlab('Longitude') + 
#   ylab('Latitude') + 
#   scale_size () + 
#   coord_equal() + 
#   scale_color_gradient(low = 'darkgreen', high = 'yellow')

diversity3 <- diversity2[which(diversity2$invasives < 1.2),]
ggplot() +
  aes(diversity3$lon, diversity3$lat, color = diversity3$invasives, size = diversity3$smallerPD) +
  geom_point() +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  scale_color_gradient(low = 'darkgreen', high = 'yellow')

#############################################################################################################

marlinplots_as <- marlinplots[which(!is.na(marlinplots$Aspect)),]
diversity2 <- diversity[which(diversity$X %in% intersect(diversity$X, marlinplots_as$Plot)),]
rownames(diversity2) <- diversity2$X
diversity2$lat <- marlinplots_as$marlinlat[which(marlinplots_as$Plot %in% intersect(marlinplots_as$Plot, rownames(diversity2)))]
diversity2$lon <- marlinplots_as$marlinlon[which(marlinplots_as$Plot %in% intersect(marlinplots_as$Plot, rownames(diversity2)))]
diversity2$smallerPD <- diversity2$PD/1000
diversity2$aspect <- as.numeric(marlinplots_as$Aspect[which(marlinplots_as$Plot %in% intersect(marlinplots_as$Plot, rownames(diversity2)))])

aspect_ord <- metaMDS(dist(diversity2$aspect, method = 'euclidean'))
MDS1 <- aspect_ord$points[1:nrow(diversity2),1]


ggplot() +
  aes(diversity2$lon, diversity2$lat, color = abs(MDS1), size = diversity2$smallerPD) + 
  geom_point() +
  xlab('Longitude') + 
  ylab('Latitude') + 
  scale_size () + 
  coord_equal() + 
  scale_color_gradient(low = 'darkgreen', high = 'yellow')


