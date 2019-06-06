library(picante)
library(vegan)
library(RColorBrewer)
theme_set(theme_minimal())

dat <- read.csv('~/Documents/GitHub/east_woods_work/data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)

# looking at the groupings of plots based on different units (management and area)
plots.env <- read.csv('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Rollinson/data_processed/point_info_GIS.csv')
#plots.env <- read.csv('~/Documents/GitHub/east_woods_work/data/East_Woods/Inventory_2018/Analyses_Rollinson/point_info_GIS.csv')
plots.env$PlotID <- gsub('-', '', plots.env$PlotID)
plots.burn <- read.csv('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Rollinson/data_processed/point_info_GIS_burnhistory.csv')
#plots.burn <- read.csv('~/Documents/GitHub/east_woods_work/data/East_Woods/Inventory_2018/Analyses_Rollinson/point_info_GIS_burnhistory.csv')
plots.burn$PlotID <- gsub('-', '', plots.burn$PlotID)

# first clean up spp pool 
# this shouldnt even really be in this script needs to move somewhere else before com mat are made
# not sure which spp name the impatiens were supposed to go with so gonna hold off on running this till I do 
# dat$Impatiens_ <- dat$Impatiens_capensis + dat$Impatiens_pallida
# dat$Impatiens_capensis <- NULL 
# dat$Impatiens_pallida <- NULL

# first grab only plots in East Woods 
ew_plots <- plots.env[which(plots.env$wooded== 'East Woods'),]
dat_ew <- dat[ew_plots$PlotID,]
# then only look at which plots have more than 3 species present in them
dat_ew <- dat_ew[which(rowSums(dat_ew) > 3),]

# these are likely wetland plots.. were showing up as outliers on the ordination
dat_ew <- dat_ew[which(!row.names(dat_ew) %in% c('R79', 'LL123')),]
ord_outliers <- ew_plots[which(ew_plots$PlotID %in% c('R79', 'LL123')),]

ord_ew <- metaMDS(dat_ew)
plot(ord_ew, type = 'n')
points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
#text(ord_ew, display = 'sites', cex = 0.6)
title('East Woods plots with > 3 species')

ggplot()+ 
  geom_point(aes(ew_plots$lon, ew_plots$lat),  cex = 0.8, pch = 21, col = 'red', bg = 'yellow') + 
  geom_point(aes(ord_outliers$lon, ord_outliers$lat), color = 'blue') + 
  #geom_point(aes(hidden_lake$lon, hidden_lake$lat), color = 'yellow') + 
  #geom_point(aes(burned_plots_map$lon, burned_plots_map$lat), color = 'black', pch = 21, bg = 'darkslategray') + 
  #geom_point(aes(unburned_plots_map$lon, unburned_plots_map$lat), color = 'blue3', pch = 21, bg = 'blue') + 
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('East Woods Plots with > 3 Species')


# Burned Plots / Managed Areas

plots.burn <- plots.burn[which(plots.burn$PlotID %in% ew_plots$PlotID),]
burned <- plots.burn[which(!is.na(plots.burn$)),]
unburned <- plots.burn[which(is.na(plots.burn$Burn_Date)),]

hulls <- data.frame(ord_ew$points)
hulls$burn_stat <- ifelse(row.names(hulls) %in% burned$PlotID, 'Burned', 'Unburned')

plot(ord_ew, type = 'n')
points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
#text(ord_ew, display = 'sites', cex = 0.6)
title('(TREES) EW plots Burned and Unburned')
ordiellipse(ord_ew, hulls$burn_stat, col = c('black', 'blue'), lwd = 2, label= FALSE)
ordispider(ord_ew, hulls$burn_stat, col = c('black', 'blue'), label = T)
legend('topleft', legend = unique(hulls$burn_stat), col = c('black', 'blue'), pch = 21, pt.bg = c('black', 'blue'), cex = 0.8)

ggplot()+ 
  geom_point(aes(ew_plots$lon, ew_plots$lat),  cex = 0.8, pch = 21, col = 'red', bg = 'yellow') + 
  geom_point(aes(burned$lon, burned$lat), color = 'black', pch = 21, bg = 'darkslategray') + 
  geom_point(aes(unburned$lon, unburned$lat), color = 'blue3', pch = 21, bg = 'blue') + 
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Burned vs Unburned East Woods Plots')


# Community class

hulls$ComClass <- plots.env$ComClass[which(plots.env$PlotID %in% row.names(hulls))]

colvec = brewer.pal(11, 'Spectral')

plot(ord_ew, type = 'n')
points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
#text(ord_ew, display = 'sites', cex = 0.6)
title('East Woods Plots with > 3 species ComClasses')
ordiellipse(ord_ew, hulls$ComClass, col = colvec[unique(hulls$ComClass)], label = T, cex = 0.6)
ordispider(ord_ew, hulls$ComClass, label = F, cex = 0.6,  col = colvec[unique(hulls$ComClass)]) 
legend("topleft", legend = unique(hulls$ComClass), col = colvec[unique(hulls$ComClass)], 
      pt.bg = colvec[unique(hulls$ComClass)], bty = 'n', pch = 21, cex = 0.8)


# Woodland plots

woodland_types <- c('Mesic woodland', 'Dry mesic woodland, Mesic-wet mesic forest, Mesic woodland' , 'Dry mesic woodland, Mesic-wet mesic forest, Savanna' ,
                    'Mesic-wet mesic forest, Mesic-wet mesic shrubland' ,  'Mesic-wet mesic shrubland, Mesic savanna, Mesic woodland' ,  'Mesic-wet mesic woodland, Mesic woodland' ,
                    'Savanna')

grassland_types <- c('Mesic savanna, Mesic woodland' ,  'Mesic-wet mesic shrubland' , 'Wet mesic prairie and savanna')


plots.env$WoodGrass <- ifelse(plots.env$ComClass %in% woodland_types, 'Woodland', 'Grassland')
hulls$WoodGrass <- plots.env$WoodGrass[which(plots.env$PlotID %in% row.names(hulls))]

plot(ord_ew, type = 'n')
points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
#text(ord_ew, display = 'sites', cex = 0.6)
title('(TREES) EW plots Woodland and Grassland')
ordiellipse(ord_ew, hulls$WoodGrass, col = c('chartreuse4', 'brown4'), lwd = 2, label= FALSE)
ordispider(ord_ew, hulls$WoodGrass, col = c('chartreuse4', 'brown4'), label = T)
legend('topleft', legend = unique(hulls$WoodGrass), col = c('chartreuse4', 'brown4'), pch = 21, pt.bg = c('chartreuse4', 'brown4'), cex = 0.8)


w_plots <- row.names(hulls)[which(hulls$WoodGrass == 'Woodland')]
woodland_plots <- plots.env[which(plots.env$PlotID %in% w_plots),]
g_plots <- row.names(hulls)[which(hulls$WoodGrass == 'Grassland')]
grassland_plots <- plots.env[which(plots.env$PlotID %in% g_plots),]

ggplot()+ 
  geom_point(aes(ew_plots$lon, ew_plots$lat),  cex = 0.8, pch = 21, col = 'red', bg = 'yellow') + 
  geom_point(aes(woodland_plots$lon, woodland_plots$lat), color = 'chartreuse4', pch = 21, bg = 'chartreuse') + 
  geom_point(aes(grassland_plots$lon, grassland_plots$lat), color = 'brown4', pch = 21, bg = 'chocolate') + 
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Woodland and Grassland Plots EW')


# Management Area 






