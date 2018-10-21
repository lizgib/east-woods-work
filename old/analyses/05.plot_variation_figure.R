#make map of plots with diversity level as point size and variation
# of environmental variables is color of points

library(ggplot2)
source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/oldstuff/marlin_plots.R')
diversity <- read.csv('OUTPUTS/diversity_metrics.csv')

marlinplots <- marlinplots[which(marlinplots$CanopyOpenness < 50),]
diversity <- diversity[which(diversity$X %in% intersect(diversity$X, marlinplots$Plot)),]
rownames(diversity) <- diversity$X
diversity$lat <- marlinplots$marlinlat[which(marlinplots$Plot %in% intersect(marlinplots$Plot, rownames(diversity)))]
diversity$lon <- marlinplots$marlinlon[which(marlinplots$Plot %in% intersect(marlinplots$Plot, rownames(diversity)))]
diversity$smallerPD <- diversity$PD/1000
diversity$canopy <- as.numeric(marlinplots$CanopyOpenness[which(marlinplots$Plot %in% intersect(marlinplots$Plot, rownames(diversity)))])

canopy_ord <- metaMDS(dist(diversity$canopy))
diversity <- cbind(diversity, canopy_ord$points)

ggplot() +
  geom_point(aes(diversity$lat, diversity$lon), color = abs(diversity$MDS1$MDS1), size = (diversity$smallerPD)) + 
  xlab('Latitude') + 
  ylab('Longitude') + 
  scale_size () + 
  coord_equal()

ggplot(diversity(aes(x=lon, y = lat))) +
  geom_point(aes(color = abs(MDS2), size = (smallerPD))) + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  scale_size () + 
  coord_equal()
