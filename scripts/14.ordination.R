
library(picante)
library(vegan)
library(RColorBrewer)
library(readxl)
library(ggplot2)
theme_set(theme_minimal()) 

dat <- read.csv('~/Documents/GitHub/east_woods_work/data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)

# looking at the groupings of plots based on different units (management and area)
plots.env <- read.csv('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Rollinson/data_processed/point_info_GIS.csv')
#plots.env <- read.csv('~/Documents/GitHub/east_woods_work/data/East_Woods/Inventory_2018/Analyses_Rollinson/point_info_GIS.csv')
plots.env$PlotID <- gsub('-', '', plots.env$PlotID)
plots.env$MgmtUnit <- ifelse(is.na(plots.env$wooded), "Non-Wooded",
                             ifelse(plots.env$wooded=="Hidden Lake", "Hidden Lake",
                                    ifelse(plots.env$unit=="South 40 South", "Annual Burn",
                                           ifelse(!is.na(plots.env$unit), "Mixed Management", "No Management"))))
#---------------
# script to replace how I calculated the dominant tree group in the fall

dat.all <- read.csv('data/species/dat.all.csv')
tree07 <- dat.all[which(dat.all$datset == 'T' & dat.all$year == '2007'),]
tree18 <- dat.all[which(dat.all$datset == 'T' & dat.all$year == '2018'),]

tree18$genus <- gsub(' .*', '', tree18$species)
tree07$genus <- gsub(' .*', '', tree07$species)
  
source('~/Documents/GitHub/east_woods_work/scripts/cover.R')

tree18$cover <- as.numeric(tree18$cover)
tree07$cover <- as.numeric(tree07$cover)
tree18 <- get_plot_cover(tree18)
tree07 <- get_plot_cover(tree07)

tree_type_plots_18 <- data.frame()
for (plt in unique(tree18$plot)){
 dom <- max(tree18$spp_percent_total_cover[which(tree18$plot == plt)])
 tree_type <- unique(tree18$genus[which(tree18$spp_percent_total_cover == dom)])
 temp <- data.frame(plt, tree_type)
 tree_type_plots_18 <- rbind(tree_type_plots_18, temp)
}

plots.env$DomGenus18 <- tree_type_plots_18$tree_type[match(plots.env$PlotID, tree_type_plots_18$plt)]

tree_type_plots_07 <- data.frame()
for (plt in unique(tree07$plot)){
  dom <- max(tree07$spp_percent_total_cover[which(tree07$plot == plt)])
  tree_type <- unique(tree07$genus[which(tree07$spp_percent_total_cover == dom)])
  temp <- data.frame(plt, tree_type)
  tree_type_plots_07 <- rbind(tree_type_plots_07, temp)
}

plots.env$DomGenus07 <- tree_type_plots_07$tree_type[match(plots.env$PlotID, tree_type_plots_07$plt)]


# first grab only plots in East Woods
ew_plots <- plots.env[which(plots.env$wooded== 'East Woods'),]
dat_ew <- dat[ew_plots$PlotID,]
# then only look at which plots have more than 3 species present in them
dat_ew <- dat_ew[which(rowSums(dat_ew) > 3),]

# these are likely wetland plots.. were showing up as outliers on the ordination
# 2018 
# dat_ew <- dat_ew[which(!row.names(dat_ew) %in% c('R79', 'LL123')),]
# ord_outliers <- ew_plots[which(ew_plots$PlotID %in% c('R79', 'LL123')),]
# 2007 
dat_ew <- dat_ew[which(!row.names(dat_ew) %in% c('Y94', 'FF94', 'R79')),]
ord_outliers <- ew_plots[which(ew_plots$PlotID %in% c('Y94', 'FF94', 'R79')),]

ew_plots <- ew_plots[which(ew_plots$PlotID %in% rownames(dat_ew)),]


ord_ew <- metaMDS(dat_ew)

# Burned Plots / Managed Areas

ew_plots$oakacer18 <- ifelse(ew_plots$DomGenus18 == 'Quercus', "Quercus",
                         ifelse(ew_plots$DomGenus18 =="Acer", "Acer",
                                (ifelse(is.na(ew_plots$DomGenus18), 'Other', 'Other'))))

ew_plots$oakacer07 <- ifelse(ew_plots$DomGenus07 == 'Quercus', "Quercus",
                             ifelse(ew_plots$DomGenus07 =="Acer", "Acer",
                                    (ifelse(is.na(ew_plots$DomGenus07), 'Other', 'Other'))))

hulls <- data.frame(ord_ew$points)
hulls$MgmtUnit <- ew_plots$MgmtUnit[which(ew_plots$PlotID %in% row.names(hulls))]

plot(ord_ew, type = 'n')
points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
title('EW Plots Management 2007')
text(ord_ew, 'sites')
ordiellipse(ord_ew, hulls$MgmtUnit, col = c('cyan4', 'coral1', 'darkgray'), lwd = 2, label= F)
ordispider(ord_ew, hulls$MgmtUnit, col = c('cyan3', 'coral3', 'darkgray'), label = T)
legend('topleft', legend = unique(hulls$MgmtUnit), col = c('cyan4', 'coral1', 'darkgray'), pch = 21, pt.bg = c('cyan4', 'coral1', 'darkgray'), cex = 0.8)


ggplot(data = ew_plots, aes(x = lon, y = lat, color = factor(MgmtUnit)))+
  geom_point() +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('East Woods Plots Management 2018')


# Dominant tree type
hulls$oakacer18 <- ew_plots$oakacer18[which(ew_plots$PlotID %in% row.names(hulls))]
hulls$oakacer07 <- ew_plots$oakacer07[which(ew_plots$PlotID %in% row.names(hulls))]

colvec <- c('cornflowerblue', 'cornsilk4', 'aquamarine2', 'blueviolet', 'deeppink3', 'darkgreen',
            'chocolate4', 'deepskyblue3', 'deeppink4', 'darkblue', 'chartreuse2', 'darkseagreen3', 'darkgoldenrod3')

# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
# #text(ord_ew, display = 'sites', cex = 0.6)
# #ordiellipse(ord_ew, hulls$DomGenus18, col = colvec[hulls$DomGenus18], lwd = 2, label= T)
# ordispider(ord_ew, hulls$oakacer18, col = c('chocolate4', 'blueviolet', 'cornflowerblue'), label = F)
# title('Oak/Acer EW plots 2018')
# legend('topleft', legend = unique(hulls$oakacer18), col = c('chocolate4', 'blueviolet', 'cornflowerblue'), pch = 21, cex = 0.8)

ggplot(data = hulls, aes(x = hulls$MDS1, y = hulls$MDS2, col = hulls$oakacer07)) + 
  geom_point(pch = 21, cex = 0.8) + 
  geom_polygon(data = hulls, aes(MDS1, MDS2, fill = oakacer07, group = oakacer07, alpha = 0.30)) + 
  coord_equal() + 
  ggtitle('Oak Acer Plots 2007')
  theme(
      axis.text.x = element_blank(),  # remove x-axis text
      axis.text.y = element_blank(), # remove y-axis text
      axis.ticks = element_blank(),  # remove axis tickspanel.background = element_blank(), 
      panel.grid.major = element_blank(),  #remove major-grid labels
      panel.grid.minor = element_blank())  #remove minor-grid labels)
  

ew_plots$DomGenus18 <- as.factor(ew_plots$DomGenus18)
ew_plots$DomGenus07 <-  as.factor(ew_plots$DomGenus07) 


n <- 20
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colvec = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

tregen <- unique(c(as.vector(ew_plots$DomGenus07), as.vector(ew_plots$DomGenus18)))

myColors <- colvec[1:17]
names(myColors) <- levels(tregen)
colScale <- scale_colour_manual(name = "tregen", values = myColors)

ggplot(data = ew_plots, aes(x = lon, y = lat, col = ew_plots$DomGenus18, fill = ew_plots$DomGenus07)) +
   geom_point(shape = 21, size = 3, stroke = 2) +
   #geom_point(shape = 21, col = ew_plots$DomGenus18, fill = ew_plots$DomGenus07)+
   xlab('Longitude') +
   ylab('Latitude') +
   scale_size () +
   coord_equal() +
   ggtitle('Dominant Tree Genus Changes between 2018 + 2007')



# ggplot(data = ew_plots, aes(x = lon, y = lat, size = SR)) + 
#   geom_point(col = 'darkblue')+
#   xlab('Longitude') +
#   ylab('Latitude') +
#   scale_size () +
#   coord_equal() +
#   ggtitle('Species Richness')


# dat = 'data/ord_mat/ord_07_all.csv' # NAs need to be removed from this before passing in 
# 
# setwd('~/Documents/GitHub/east_woods_work/')
# dat = read.csv(dat, row.names = 1)


# do_kmeans <- function(dat, n){
#   dat = as.matrix(dat)
#   n = as.numeric(n) # number of ks
#   c1 <- kmeans(dat, n, iter.max = 100, nstart = 50)
#   return(as.data.frame(c1$cluster))
# }
# 
# n2 <- do_kmeans(ord_pts, 2) # like woodland grassland?
# n3 <- do_kmeans(ord_pts, 3) # like management units
# n5 <- do_kmeans(ord_pts, 5)
# n10 <- do_kmeans(ord_pts, 10) # closest one to the number of com classes
# n20 <- do_kmeans(ord_pts, 20)
# 
# oup <- as.data.frame(ord_pts)
# oup$k2 <- n2$`c1$cluster`
# oup$k3 <- n3$`c1$cluster`
# oup$k5 <- n5$`c1$cluster`
# oup$k10 <- n10$`c1$cluster`
# oup$k20 <- n20$`c1$cluster`

# n <- 20
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# colvec = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[oup$k2])
# title('K2 2007')
# #ordiellipse(ord_ew, oup$k2, col = 'black', lwd = 2, label= F)
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[oup$k3])
# title('K3 2007')
# #ordiellipse(ord_ew, oup$k3, col = 'black', lwd = 2, label= F)
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[oup$k5])
# title('K5 2007')
# #ordiellipse(ord_ew, oup$k5, col = 'black', lwd = 2, label= F)
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[oup$k10])
# title('K10 2007')
# #ordiellipse(ord_ew, oup$k10, col = 'black', lwd = 2, label= F)
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[oup$k20])
# title('K20 2007')
# #ordiellipse(ord_ew, oup$k20, col = 'darkgray', lwd = 2, label= F)

# write.csv(oup, 'outputs/clust_07.csv', quote = F) # save the cluster IDs to file

#-------------------
dat.ord <- as.data.frame(ord_ew$points)
#temp <- ew_plots[,c('MgmtUnit', 'lat', 'lon')]
temp <- ew_plots[,c('lat', 'lon', 'oakacer07')]

(fit <- envfit(ord_ew, temp, perm = 999, na.rm = T))
scores(fit, "vectors")
plot(ord_ew, type = 'n')
plot(ord_ew, 'sites')
plot(fit, p.max = 0.05, col = "red")
title('Dominant tree group Features Fit 2007')

ord <- cca(dat_ew ~ MgmtUnit + unit, ew_plots, na.action = na.omit)
plot(ord, 'sites')
fit <- envfit(ord, temp, perm = 999, display = "lc", na.rm = T)
plot(fit, p.max = 0.05, col = "red")

# ggplot(data = ew_plots, aes(x = lon, y = lat, size = dat.ord$MDS2, col = dat.ord$MDS2)) +
#   geom_point()+
#   xlab('Longitude') +
#   ylab('Latitude') +
#   scale_size () +
#   coord_equal() +
#   ggtitle('Ordination all plots 2018 (MDS2)')

