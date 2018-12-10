# plotting the plot by plot and pllot dissimilarity regressions 
library(ggplot2)
library(colorspace)
liz_data <- read.table('~/Documents/GitHub/east_woods_work/data/liz_data.csv', sep = ',')
theme_set(theme_minimal())

#plt_format <- theme()

# INDIVIDUAL PLOT 07

make_plot_07 <- function(liz_data, xvar, xlabel){
p1 <- ggplot()+
      geom_point(aes(xvar, liz_data$PD07), color = 'mediumseagreen') +
      #ggtitle((paste(n, 'effect on MNTD', sep = ' '))) +
      xlab(xlabel) +
      ylab(' ') # just gonna make one axis in ppt
p1 + stat_smooth(method = 'lm', formula = y ~ x)
pr2 = summary(lm(xvar ~ liz_data$PD07))$r.squared
print(paste('PD', xlabel, 'R2 = ', pr2))
ggsave(filename = paste(xlabel, 'effect on MNTD07.png', sep = ''), plot = p1,
        device = 'png', width = 4, height = 4, path = '~/Desktop/plots/2007')
p2 <- ggplot()+
      geom_point(aes(xvar, liz_data$SR07), color = 'mediumblue') +
      #ggtitle((paste(n, 'effect on SR', sep = ' '))) +
      xlab(xlabel) +
      ylab(' ')
p2 + stat_smooth(method = 'lm', formula = y ~ x)
pr2 = summary(lm(xvar ~ liz_data$SR07))$r.squared
print(paste('SR', xlabel, 'R2 = ', pr2))
ggsave(filename = paste(xlabel, 'effect on SR07.png', sep = ''), plot = p2,
        device = 'png', width = 4, height = 4, path = '~/Desktop/plots/2007')
}

make_plot_07(liz_data, liz_data$burn_count, 'burn_count')
make_plot_07(liz_data, liz_data$canopy07, 'canopy07')
make_plot_07(liz_data, liz_data$inv_ratio07, 'inv_ratio07')
make_plot_07(liz_data, liz_data$soil_index, 'soil_index')
make_plot_07(liz_data, liz_data$geo_drainage, 'drainage')


# INDIVIDUAL PLOT 18

make_plot_18 <- function(liz_data, xvar, xlabel){
  p1 <- ggplot()+
    geom_point(aes(xvar, liz_data$PD18), color = 'mediumseagreen') +
    #ggtitle((paste(n, 'effect on MNTD', sep = ' '))) +
    xlab(xlabel) +
    ylab(' ') # just gonna make one axis in ppt
  p1 <- p1 + stat_smooth(method = 'lm', formula = y ~ x)
  pr2 = summary(lm(xvar ~ liz_data$PD18))$r.squared
  print(paste('PD', xlabel, 'R2 = ', pr2))
  ggsave(filename = paste(xlabel, 'effect on MNTD18.png', sep = ''), plot = p1,
         device = 'png', width = 4, height = 4, path = '~/Desktop/plots/2018')
  p2 <- ggplot()+
    geom_point(aes(xvar, liz_data$SR18), color = 'mediumblue') +
    #ggtitle((paste(n, 'effect on SR', sep = ' '))) +
    xlab(xlabel) +
    ylab(' ')
  p2 <- p2 + stat_smooth(method = 'lm', formula = y ~ x)
  pr2 = summary(lm(xvar ~ liz_data$SR18))$r.squared
  print(paste('SR', xlabel, 'R2 = ', pr2))
  ggsave(filename = paste(xlabel, 'effect on SR18.png', sep = ''), plot = p2,
         device = 'png', width = 4, height = 4, path = '~/Desktop/plots/2018')
}

make_plot_18(liz_data, liz_data$burn_count, 'burn_count')
make_plot_18(liz_data, liz_data$canopy18, 'canopy18')
make_plot_18(liz_data, liz_data$inv_ratio18, 'inv_ratio18')
make_plot_18(liz_data, liz_data$soil_index, 'soil_index')
make_plot_18(liz_data, liz_data$geo_drainage, 'drainage')




##################################################################################################################################

# DISSIMILARITY 

png('dissimilarity_invasives_07.png', width = 500, height = 500)
ggplot()+
  geom_point(aes(invasives07, beta_Dnn_all.07), color = 'blue') +
  geom_point(aes(invasives07, all_jaccard_07), color = 'purple') +
  ggtitle('INVASIVE RATIO 07') + 
  xlab('Invasive Ratio') + 
  ylab('Diversity')
dev.off()

png('dissimilarity_canopy_07.png', width = 500, height = 500)
ggplot()+
  geom_point(aes(canopy07, beta_Dnn_all.07), color = 'blue') +
  geom_point(aes(canopy07, all_jaccard_07), color = 'purple') +
  ggtitle('CANOPY COVER 07') + 
  xlab('Canopy Cover') + 
  ylab('Diversity')
dev.off()

# png('soil_index_07.png', width = 500, height = 500)
# ggplot()+
#   geom_point(aes(liz_data$soil_index, phylo_all_07$PD), color = 'blue') +
#   geom_point(aes(liz_data$soil_index, phylo_all_07$SR), color = 'purple') +
#   ggtitle('SOIL INDEX 07') + 
#   xlab('Soil') + 
#   ylab('Diversity')
# dev.off()

png('dissimilarity_elevation_07.png', width = 500, height = 500)
ggplot()+
  geom_point(aes(elevation, beta_Dnn_all.07), color = 'blue') +
  geom_point(aes(elevation, all_jaccard_07), color = 'purple') +
  ggtitle('ELEVATION 07') + 
  xlab('Elevation') + 
  ylab('Diversity')
dev.off()

png('dissimilarity_slope_07.png', width = 500, height = 500)
ggplot()+
  geom_point(aes(slope, beta_Dnn_all.07), color = 'blue') +
  geom_point(aes(slope, all_jaccard_07), color = 'purple') +
  ggtitle('SLOPE 07') + 
  xlab('Slope') + 
  ylab('Diversity')
dev.off()

png('dissimilarity_aspect_07.png', width = 500, height = 500)
ggplot()+
  geom_point(aes(aspect, beta_Dnn_all.07), color = 'blue') +
  geom_point(aes(aspect, all_jaccard_07), color = 'purple') +
  ggtitle('ASPECT 07') + 
  xlab('Aspect') + 
  ylab('Diversity')
dev.off()

# 2018

png('dissimilarity_invasives_18.png', width = 500, height = 500)
ggplot()+
  geom_point(aes(invasives18, beta_Dnn_all.18), color = 'blue') +
  geom_point(aes(invasives18, all_jaccard_18), color = 'purple') +
  ggtitle('INVASIVE RATIO 18') + 
  xlab('Invasive Ratio') + 
  ylab('Diversity')
dev.off()

png('dissimilarity_canopy_18.png', width = 500, height = 500)
ggplot()+
  geom_point(aes(canopy18, beta_Dnn_all.18), color = 'blue') +
  geom_point(aes(canopy18, all_jaccard_18), color = 'purple') +
  ggtitle('CANOPY COVER 18') + 
  xlab('Canopy Cover') + 
  ylab('Diversity')
dev.off()

# png('soil_index_18.png', width = 500, height = 500)
# ggplot()+
#   geom_point(aes(liz_data$soil_index, phylo_all_18$PD), color = 'blue') +
#   geom_point(aes(liz_data$soil_index, phylo_all_18$SR), color = 'purple') +
#   ggtitle('SOIL INDEX 18') + 
#   xlab('Soil') + 
#   ylab('Diversity')
# dev.off()

png('dissimilarity_elevation_18.png', width = 500, height = 500)
ggplot()+
  geom_point(aes(elevation, beta_Dnn_all.18), color = 'blue') +
  geom_point(aes(elevation, all_jaccard_18), color = 'purple') +
  ggtitle('ELEVATION 18') + 
  xlab('Elevation') + 
  ylab('Diversity')
dev.off()

png('dissimilarity_slope_18.png', width = 500, height = 500)
ggplot()+
  geom_point(aes(slope, beta_Dnn_all.18), color = 'blue') +
  geom_point(aes(slope, all_jaccard_18), color = 'purple') +
  ggtitle('SLOPE 18') + 
  xlab('Slope') + 
  ylab('Diversity')
dev.off()

png('dissimilarity_aspect_18.png', width = 500, height = 500)
ggplot()+
  stat_density_2d() +
  geom_point(aes(aspect, beta_Dnn_all.18), color = 'blue') + 
  #geom_point(aes(aspect, all_jaccard_18), color = 'purple') +
  ggtitle('ASPECT 18') + 
  xlab('Aspect') + 
  ylab('Diversity')
dev.off()






