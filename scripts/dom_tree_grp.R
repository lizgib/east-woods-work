# script to replace how I calculated the dominant tree group in the fall 
library(readxl)

path.2018 = '/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Final Data from AES/Final data (4th round) from AES.10.24.2018/'
tree.spring.18 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22.xlsx"), sheet = "Tree Layer")
tree.spring.18 <- tree.spring.18[,-c(1,2,7,8,9,10)]
# tree cover = DBH
names(tree.spring.18) <- c("plot", "spp.code", "species", "cover")

tree.spring.18 <- tree.spring.18[which(tree.spring.18$plot %in% plots.env$PlotID),]

# go through each plot in point_info_GIS
# for all the trees in each plot, which has the max BA 

tree.spring.18$genus <- gsub(' .*', '', tree.spring.18$species)

source('~/Documents/GitHub/east_woods_work/scripts/04.cover.R')

tree.spring.18 <- get_BA(tree.spring.18)
tree.spring.18 <- get_plot_cover(tree.spring.18)


tree_type_plots_18 <- data.frame()
for (plt in unique(tree.spring.18$plot)){
  dom <- max(tree.spring.18$spp_percent_total_cover[which(tree.spring.18$plot == plt)])
  tree_type <- unique(tree.spring.18$genus[which(tree.spring.18$spp_percent_total_cover == dom)])
  temp <- data.frame(plt, tree_type)
  tree_type_plots_18 <- rbind(tree_type_plots_18, temp)
}

plots.env$DomGenus18 <- tree_type_plots_18$tree_type[match(plots.env$PlotID, tree_type_plots_18$plt)]

ggplot(data = plots.env, aes(x = lon, y = lat, color = factor(DomGenus18)))+ 
  geom_point() + 
  #geom_point(aes(ew_plots$lon, ew_plots$lat),  cex = 0.8, pch = 21, col = 'red', bg = 'yellow') + 
  #geom_point(aes(MixMgmt$lon, MixMgmt$lat), color = 'black', pch = 21, bg = 'darkslategray') + 
  # geom_point(aes(AnnualBurn$lon, AnnualBurn$lat), color = 'blue3', pch = 21, bg = 'blue') + 
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Arboretum Plots')


