# just jumping into this one because I want results!!!

#plot diversity levels and various factors 

source('~/Documents/GitHub/east_woods_work/scripts/11.analyses.R')
library(ggplot2)
#   TREE GROUP

# INVASIVE SPECIES RATIO

ggplot()+
  aes(gibbons_data$plot_invasive_cover_07, gibbons_data$plot_invasive_cover_18) +
  geom_point() +
  stat_smooth(method = 'lm', formula = x ~ y)
  
ggplot()+
  aes(phylo_all_18$PD, gibbons_data$plot_invasive_cover_18) +
  geom_point() +  
  stat_smooth(method = 'lm', formula = x ~ y)

ggplot()+
  aes(phylo_all_18$SR, gibbons_data$plot_invasive_cover_18) +
  geom_point()

ggplot()+
  aes(phylo_all_07$PD, gibbons_data$plot_invasive_cover_07) + 
  geom_point()


