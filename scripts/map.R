# easy visulaization of where plots are
library(ggplot2)
library(RColorBrewer)
theme_set(theme_minimal())
source('~/Documents/GitHub/east_woods_work/scripts/ordination.R')

dat.plots <- read.csv('~/Documents/GitHub/east_woods_work/data/East_Woods/Inventory_2018/Analyses_Rollinson/point_info_GIS.csv')
dat.plots$PlotID <- gsub('-', '', dat.plots$PlotID)
ord_outliers <- dat.plots[which(dat.plots$PlotID %in% c('R79', 'LL123')),]
east_woods <- dat.plots[which(dat.plots$PlotID %in% ew_plots),]
hidden_lake <- dat.plots[which(dat.plots$AreaName == 'Hidden Lake'),]
burned_plots_map <- east_woods[which(east_woods$PlotID %in% burned),]
unburned_plots_map <- east_woods[which(east_woods$PlotID %in% unburned),]

ggplot()+ 
  geom_point(aes(east_woods$lon, east_woods$lat),  cex = 0.8, pch = 21, col = 'red', bg = 'yellow') + 
  #geom_point(aes(ord_outliers$lon, ord_outliers$lat), color = 'blue') + 
  #geom_point(aes(hidden_lake$lon, hidden_lake$lat), color = 'yellow') + 
  #geom_point(aes(burned_plots_map$lon, burned_plots_map$lat), color = 'black', pch = 21, bg = 'darkslategray') + 
  #geom_point(aes(unburned_plots_map$lon, unburned_plots_map$lat), color = 'blue3', pch = 21, bg = 'blue') + 
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('East Woods Plots with > 3 Species')

ggplot()+
  geom_point(aes(east_woods$lon, east_woods$lat),  cex = 0.8, pch = 21, col = 'red', bg = 'yellow') +
  geom_point(aes(ord_ew_map$lon, ord_ew_map$lat)) + 
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() 
# qhy are these not the same plots!!!

