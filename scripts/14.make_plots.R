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
plts <- list(p1, p2)
return(plts)
}

burnplts07 <- make_plot_07(liz_data, liz_data$burn_count, 'burn_count')
canopyplts07 <- make_plot_07(liz_data, liz_data$canopy07, 'canopy07')
invplts07 <- make_plot_07(liz_data, liz_data$inv_ratio07, 'inv_ratio07')
soilplts07 <- make_plot_07(liz_data, liz_data$soil_index, 'soil_index')
drainplts07 <- make_plot_07(liz_data, liz_data$geo_drainage, 'drainage')

pdplts07 <- multiplot(burnplts07[1], canopyplts07[1], invplts07[1], soilplts07[1], drainplts07[1], ncol = 2)
pdplts07

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
  plts <- list(p1, p2)
  return(plts)
}

burnplts18 <- make_plot_18(liz_data, liz_data$burn_count, 'burn_count')
canopyplts18 <- make_plot_18(liz_data, liz_data$canopy18, 'canopy18')
invplts18 <- make_plot_18(liz_data, liz_data$inv_ratio18, 'inv_ratio18')
soilplts18 <- make_plot_18(liz_data, liz_data$soil_index, 'soil_index')
drainplts18 <- make_plot_18(liz_data, liz_data$geo_drainage, 'drainage')




#----------------------------------------------------------------------------------------------------------

# DISSIMILARITY 

