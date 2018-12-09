# plotting the plot by plot and pllot dissimilarity regressions 
library(ggplot2)
library(colorspace)
source('~/Documents/GitHub/east_woods_work/scripts/phylo_metrics.R')
liz_data <- read.table('~/Documents/GitHub/east_woods_work/data/liz_data.csv', sep = ',')
theme_set(theme_minimal())

#plt_format <- theme()

# INDIVIDUAL PLOT 07
plotting_data_07 <- NULL
InvasiveRatio07 <- as.factor(liz_data$plot_invasive_cover_07)
Canopy07 <-as.factor(liz_data$canopy_07)
Elevation <- as.factor(liz_data$elevation)
Slope <- as.factor(liz_data$slope)
Aspect <- as.factor(liz_data$aspect)
plotting_data_07 <- data.frame(cbind(plotting_data_07, InvasiveRatio07, Canopy07, Elevation, Slope, Aspect, phylo_all_07$pd.obs, phylo_all_07$ntaxa))

titles <- c('InvasiveRatio07', 'Canopy07', 'Elevation', 'Slope', 'Aspect')

for(i in plotting_data_07[,1:5]){
  for (n in titles){
    p1 <- ggplot()+
      geom_point(data = plotting_data_07, aes(i, V6), color = 'mediumseagreen') +
      #ggtitle((paste(n, 'effect on MNTD', sep = ' '))) +
      xlab(n) +
      ylab(' ') # just gonna make one axis in ppt
    ggsave(filename = paste(n, 'effect on MNTD07.png', sep = ''), plot = p1,
           device = 'png', width = 4, height = 4, path = '~/Documents/morton arb/east_woods_phylogeny/OUTPUTS/Fall Figures/2007/')
    p2 <- ggplot()+
      geom_point(data = plotting_data_07, aes(i, V7), color = 'mediumblue') +
      #ggtitle((paste(n, 'effect on SR', sep = ' '))) +
      xlab(n) +
      ylab(' ')
    ggsave(filename = paste(n, 'effect on SR07.png', sep = ''), plot = p2,
           device = 'png', width = 4, height = 4, path = '~/Documents/morton arb/east_woods_phylogeny/OUTPUTS/Fall Figures/2007/')
  }
}


# INDIVIDUAL PLOT 18 
plotting_data_18 <- NULL
InvasiveRatio18 <- as.factor(liz_data$plot_invasive_cover_18)
Canopy18 <-as.factor(liz_data$canopy_18)
Elevation <- as.factor(liz_data$elevation)
Slope <- as.factor(liz_data$slope)
Aspect <- as.factor(liz_data$aspect)
plotting_data_18 <- data.frame(cbind(plotting_data_18, InvasiveRatio18, Canopy18, Elevation, Slope, Aspect, phylo_all_18$pd.obs, phylo_all_18$ntaxa))

titles <- c('InvasiveRatio18', 'Canopy18', 'Elevation', 'Slope', 'Aspect')

for(i in plotting_data_18[,1:5]){
  for (n in titles){
    p1 <- ggplot()+
      geom_point(data = plotting_data_18, aes(i, V6), color = 'mediumseagreen') +
      #ggtitle((paste(n, 'effect on MNTD', sep = ' '))) +
      xlab(n) + 
      ylab(' ') # just gonna make one axis in ppt 
    ggsave(filename = paste(n, 'effect on MNTD18.png', sep = ''), plot = p1,
           device = 'png', width = 4, height = 4, path = '~/Documents/morton arb/east_woods_phylogeny/OUTPUTS/Fall Figures/2018/')
    p2 <- ggplot()+
      geom_point(data = plotting_data_18, aes(i, V7), color = 'mediumblue') +
      #ggtitle((paste(n, 'effect on SR', sep = ' '))) +
      xlab(n) + 
      ylab(' ')
    ggsave(filename = paste(n, 'effect on SR18.png', sep = ''), plot = p2,
           device = 'png', width = 4, height = 4, path = '~/Documents/morton arb/east_woods_phylogeny/OUTPUTS/Fall Figures/2018/')
  }
}


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






