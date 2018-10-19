#get plots

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses')

trees <- data.frame(read.csv('../data_processing/DATA/spp.trees.2018.csv', as.is = T))
herbs <- data.frame(read.csv('../data_processing/DATA/spp.herb.2018.csv', as.is =T))
shrubs <- data.frame(read.csv('../data_processing/DATA/spp.shrub.2018.csv', as.is = T))

plots <- unique(trees$Plot.ID.Number)
plots <- c(unique(herbs$Plot.ID.Number), plots)
plots <- c(unique(shrubs$Plot.ID.Number), plots)
plots <- sort(unique(plots))

coord <- data.frame(read.csv('DATA/plot.coordinates.2018-07-06.csv', as.is = T))
plts <- gsub('-', '', coord$CORNER, fixed = T)

lat <- coord$lat[match(plots, plts)]
lon <- coord$lon[match(plots, plts)]
df <-data.frame(lat)
df$lon <- lon
newdf<-na.omit(df[df$lat>40,])

marlinplots <- data.frame(read.csv('DATA/marlins_data.csv', as.is = T))
marlinplots <- marlinplots$Plot
mplts <- gsub('-', '', marlinplots, fixed = T)
marlinlat <- coord$lat[match(mplts, plts)]
marlinlon <- coord$lon[match(mplts, plts)]
mdf <-data.frame(marlinlat)
mdf$marlinlon <- marlinlon
mdf <- na.omit(mdf[mdf$marlinlat>40,])
rownames(mdf) <- mplts[match(mplts, plts)]

library(ggplot2)
ggplot() + 
  geom_point(aes(newdf$lon, newdf$lat), color = 'orange') +
 # geom_point(aes(mdf$marlinlat, mdf$marlinlon), color = 'blue') + 
  xlab('Longitude') + 
  ylab('Latitude') +
  coord_equal()


# ggplot(data=mdf2[mdf2$MDS1<10,]) + 
#   # geom_point(aes(newdf$lat, newdf$lon), color = 'red') +
#   geom_point(aes(marlinlat, marlinlon, color=MDS1)) + 
#   xlab('latitude') + 
#   ylab('longitude') +
#   coord_equal()

