
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

#path.2018 = '/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Final Data from AES/Final data (4th round) from AES.10.24.2018/'
#tree.spring.18 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22.xlsx"), sheet = "Tree Layer")
#tree.spring.18 <- tree.spring.18[,-c(1,2,7,8,9,10)]
# tree cover = DBH
#names(tree.spring.18) <- c("plot", "spp.code", "species", "cover")

#tree.spring.18 <- tree.spring.18[which(tree.spring.18$plot %in% plots.env$PlotID),]

# go through each plot in point_info_GIS
# for all the trees in each plot, which has the max BA

#tree.spring.18$genus <- gsub(' .*', '', tree.spring.18$species)

#source('~/Documents/GitHub/east_woods_work/scripts/04.cover.R')

#tree.spring.18 <- get_BA(tree.spring.18)
#tree.spring.18 <- get_plot_cover(tree.spring.18)


# tree_type_plots_18 <- data.frame()
# for (plt in unique(tree.spring.18$plot)){
#   dom <- max(tree.spring.18$spp_percent_total_cover[which(tree.spring.18$plot == plt)])
#   tree_type <- unique(tree.spring.18$genus[which(tree.spring.18$spp_percent_total_cover == dom)])
#   temp <- data.frame(plt, tree_type)
#   tree_type_plots_18 <- rbind(tree_type_plots_18, temp)
# }

#plots.env$DomGenus18 <- tree_type_plots_18$tree_type[match(plots.env$PlotID, tree_type_plots_18$plt)]

# ggplot(data = plots.env, aes(x = lon, y = lat, color = factor(wooded)))+
#   geom_point() +
#   #geom_point(aes(ew_plots$lon, ew_plots$lat),  cex = 0.8, pch = 21, col = 'red', bg = 'yellow') +
#   #geom_point(aes(MixMgmt$lon, MixMgmt$lat), color = 'black', pch = 21, bg = 'darkslategray') +
#   # geom_point(aes(AnnualBurn$lon, AnnualBurn$lat), color = 'blue3', pch = 21, bg = 'blue') +
#   xlab('Longitude') +
#   ylab('Latitude') +
#   scale_size () +
#   coord_equal() +
#   ggtitle('Arboretum Plots')
# -------------

# first grab only plots in East Woods
ew_plots <- plots.env[which(plots.env$wooded== 'East Woods'),]
dat_ew <- dat[ew_plots$PlotID,]
# then only look at which plots have more than 3 species present in them
dat_ew <- dat_ew[which(rowSums(dat_ew) > 3),]

# these are likely wetland plots.. were showing up as outliers on the ordination
# 2018 
dat_ew <- dat_ew[which(!row.names(dat_ew) %in% c('R79', 'LL123')),]
ord_outliers <- ew_plots[which(ew_plots$PlotID %in% c('R79', 'LL123')),]
ew_plots <- ew_plots[which(ew_plots$PlotID %in% rownames(dat_ew)),]
# 2007 
# dat_ew <- dat_ew[which(!row.names(dat_ew) %in% c('Y94', 'FF94')),]
# ord_outliers <- ew_plots[which(ew_plots$PlotID %in% c('Y94', 'FF94')),]

ord_ew <- metaMDS(dat_ew)

# ------------------
# save the points to file so we can cluster using kmeans
#write.csv(ord_ew$points, 'data/ord_mat/ord_18_all.csv')
#write.csv(ord_ew$points, 'data/ord_mat/ord_18_all.csv')
#ord_pts <- ord_ew$points
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
# #text(ord_ew, display = 'sites', cex = 0.6)
# title('EW Plots > 3 spp 2007')
# 
# ggplot()+
#   geom_point(aes(ew_plots$lon, ew_plots$lat),  cex = 0.8, pch = 21, col = 'red', bg = 'yellow') +
#   geom_point(aes(ord_outliers$lon, ord_outliers$lat), color = 'blue') +
#   xlab('Longitude') +
#   ylab('Latitude') +
#   scale_size () +
#   coord_equal() +
#   ggtitle('East Woods Plots with > 3 Species 2007')


# Burned Plots / Managed Areas

# hulls <- data.frame(ord_ew$points)
# hulls$MgmtUnit <- ew_plots$MgmtUnit[which(ew_plots$PlotID %in% row.names(hulls))]
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
# title('EW Plots Management 2007')
# ordiellipse(ord_ew, hulls$MgmtUnit, col = c('cyan4', 'coral1', 'darkgray'), lwd = 2, label= F)
# ordispider(ord_ew, hulls$MgmtUnit, col = c('cyan3', 'coral3', 'darkgray'), label = T)
# legend('topleft', legend = unique(hulls$MgmtUnit), col = c('cyan4', 'coral1', 'darkgray'), pch = 21, pt.bg = c('cyan4', 'coral1', 'darkgray'), cex = 0.8)
# 
# 
# ggplot(data = ew_plots, aes(x = lon, y = lat, color = factor(MgmtUnit)))+
#   geom_point() +
#   xlab('Longitude') +
#   ylab('Latitude') +
#   scale_size () +
#   coord_equal() +
#   ggtitle('East Woods Plots Management 2007')


# Dominant tree type
# hulls$DomGenus18 <- ew_plots$DomGenus18[which(ew_plots$PlotID %in% row.names(hulls))]
# 
# colvec <- c('cornflowerblue', 'cornsilk4', 'aquamarine2', 'blueviolet', 'deeppink3', 'darkgreen', 
#             'chocolate4', 'deepskyblue3', 'deeppink4', 'darkblue', 'chartreuse2', 'darkseagreen3', 'darkgoldenrod3')
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[hulls$DomGenus18])
# #text(ord_ew, display = 'sites', cex = 0.6)
# #ordiellipse(ord_ew, hulls$DomGenus18, col = colvec[hulls$DomGenus18], lwd = 2, label= T)
# #ordispider(ord_ew, hulls$DomGenus18, col = colvec, label = F)
# title('Dominant Tree Genus EW plots 2007')
# legend('topleft', legend = unique(hulls$DomGenus18), col = as.numeric(unique(factor(colvec[hulls$DomGenus18]))), pch = 21, cex = 0.8)
# 
# 
# ggplot(data = ew_plots, aes(x = lon, y = lat, color = factor(DomGenus18)))+
#   geom_point() +
#   #geom_point(aes(ew_plots$lon, ew_plots$lat),  cex = 0.8, pch = 21, col = 'red', bg = 'yellow') +
#   #geom_point(aes(MixMgmt$lon, MixMgmt$lat), color = 'black', pch = 21, bg = 'darkslategray') +
#   # geom_point(aes(AnnualBurn$lon, AnnualBurn$lat), color = 'blue3', pch = 21, bg = 'blue') +
#   xlab('Longitude') +
#   ylab('Latitude') +
#   scale_size () +
#   coord_equal() +
#   ggtitle('Dominant Tree Genus 2007')

# this is not gonna be the way I calculated things in the fall
# for now, just gonna count up which was the dominant

# ew_plots$PBD <- diversity_metrics$PBD[match(ew_plots$PlotID, diversity_metrics$X)]
# ew_plots$SR <- diversity_metrics$SR[match(ew_plots$PlotID, diversity_metrics$X)]
# 
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
temp <- ew_plots[,c('aspect', 'elev', 'slope')]

(fit <- envfit(ord_ew, temp, perm = 999, na.rm = T))
scores(fit, "vectors")
plot(ord_ew, 'sites')
plot(ord_ew, type = 'n')
plot(fit, p.max = 0.05, col = "red")

ord <- cca(dat_ew ~ MgmtUnit + unit, ew_plots, na.action = na.omit)
plot(ord, type="p")
plot(ord, type="s")
fit <- envfit(ord, temp, perm = 999, display = "lc", na.rm = T)
plot(fit, p.max = 0.05, col = "red")

ggplot(data = ew_plots, aes(x = lon, y = lat, size = dat.ord$MDS2, col = dat.ord$MDS2)) +
  geom_point()+
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Ordination all plots 2018 (MDS2)')

