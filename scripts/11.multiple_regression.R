library(ggplot2)
# just get the R2 between each var and diversity 

data('plots.env')
# 2007 
#inv.07.pd <- lm(phylo_all_07$PD ~ gibbons_data$plot_invasive_cover_07)
#inv.07.sr <- lm(phylo_all_07$SR ~ gibbons_data$plot_invasive_cover_07)

TreeCover.07.pd <- lm(plots.env$PD07 ~ plots.env$TreeCover07)
TreeCover.07.sr <- lm(plots.env$SR07 ~ plots.env$TreeCover07)

#soil.07.pd <- lm(phylo_all_07$PD ~ gibbons_data$soil_index)
#soil.07.sr <- lm(phylo_all_07$SR ~ gibbons_data$soil_index)

elevation.07.pd <- lm(plots.env$PD07 ~ plots.env$elev)
elevation.07.sr <- lm(plots.env$SR07 ~ plots.env$elev)

slope.07.pd <- lm(phylo_all_07$PD ~ gibbons_data$slope)
slope.07.sr <- lm(phylo_all_07$SR ~ gibbons_data$slope)

aspect.07.pd <- lm(phylo_all_07$PD ~ gibbons_data$aspect)
aspect.07.sr <- lm(phylo_all_07$SR ~ gibbons_data$aspect)


# 2018 
# inv.18.pd <- lm(phylo_all_18$PD ~ gibbons_data$plot_invasive_cover_18)
# inv.18.sr <- lm(phylo_all_18$SR ~ gibbons_data$plot_invasive_cover_18)

TreeCover.18.pd <- lm(plots.env$PD18 ~ plots.env$TreeCover18)
TreeCover.18.sr <- lm(plots.env$SR18 ~ plots.env$TreeCover18)

# soil.18.pd <- lm(phylo_all_18$PD ~ gibbons_data$soil_index)
# soil.18.sr <- lm(phylo_all_18$SR ~ gibbons_data$soil_index)

elevation.18.pd <- lm(plots.env$PD18 ~ plots.env$elev)
elevation.18.sr <- lm(plots.env$SR18 ~ plots.env$elev)

slope.18.pd <- lm(plots.env$PD18 ~ plots.env$slope)
slope.18.sr <- lm(plots.env$SR18 ~ plots.env$slope)

aspect.18.pd <- lm(plots.env$PD18D ~ plots.env$aspect)
aspect.18.sr <- lm(plots.env$SR18 ~ plots.env$aspect)

# plot by plot

fit07 <- lm(plots.env$PD07 ~ plots.env$elev + plots.env$slope + plots.env$aspect + plots.env$TreeCover07)

fit18 <- lm(plots.env$PD18 ~ plots.env$elev + plots.env$slope + plots.env$aspect +  plots.env$TreeCover18)


 
theme_set(theme_minimal()) 

treeplt <- plots.env[which(plots.env$TreeCover07 != 0 & plots.env$TreeCover18 != 0),]
png('~/Documents/GitHub/east_woods_work/figures/Fall 2018/plotwise/tree_cover.png')
cols = c('treeplt$PD07'='red', 'treeplt$PD18'='blue')
ggplot()+ 
  geom_point(data = treeplt, aes(x = treeplt$TreeCover07, y = treeplt$PD07), col = 'red', fill = 'yellow', shape = 21) + 
  geom_point(data = treeplt, aes(x = treeplt$TreeCover18, y = treeplt$PD18), col = 'blue', fill = 'darkcyan', shape = 21) + 
  xlab('Tree Cover') + 
  ylab('Phylogenetic Diversity') + 
  ggtitle('Tree Cover Effect on Diverstiy') + 
  theme_minimal() + 
  scale_colour_manual(name="Year",values=cols, guide = guide_legend(override.aes=aes(fill=NA))) + 
  theme(legend.key = element_rect(fill = "white",colour = "white")) + 
  annotate("text", x = 15000, y = 2200, label = 'Blue is 2018, yellow is 2007, legends hate me')
dev.off()  

png('~/Documents/GitHub/east_woods_work/figures/Fall 2018/plotwise/slope.png')
cols = c('plots.env$PD07'='red', 'plots.env$PD18'='blue')
ggplot()+ 
  geom_point(data = plots.env, aes(x = plots.env$slope, y = plots.env$PD07), col = 'red', fill = 'yellow', shape = 21) + 
  geom_point(data = plots.env, aes(x = plots.env$slope, y = plots.env$PD18), col = 'blue', fill = 'darkcyan', shape = 21) + 
  xlab('Slope') + 
  ylab('Phylogenetic Diversity') + 
  ggtitle('Slope Effect on Diverstiy') + 
  scale_colour_manual(name="Year",values=cols, guide = guide_legend(override.aes=aes(fill=NA))) + 
  theme(legend.key = element_rect(fill = "white",colour = "white")) + 
  annotate("text", x = 1, y = 2, label = 'Blue is 2018, yellow is 2007') + 
  annotate("text", x = 0.6, y = 2000, label = '2018 R2 = , 2007 R2 = ')
dev.off()  

png('~/Documents/GitHub/east_woods_work/figures/Fall 2018/plotwise/elev.png')
cols = c('plots.env$PD07'='red', 'plots.env$PD18'='blue')
ggplot()+ 
  geom_point(data = plots.env, aes(x = plots.env$elev, y = plots.env$PD07), col = 'red', fill = 'yellow', shape = 21) + 
  geom_point(data = plots.env, aes(x = plots.env$elev, y = plots.env$PD18), col = 'blue', fill = 'darkcyan', shape = 21) + 
  xlab('Elevation') + 
  ylab('Phylogenetic Diversity') + 
  ggtitle('Elevation Effect on Diverstiy') + 
  scale_colour_manual(name="Year",values=cols, guide = guide_legend(override.aes=aes(fill=NA))) + 
  theme(legend.key = element_rect(fill = "white",colour = "white")) + 
  annotate("text", x = 770, y = 1900, label = 'Blue is 2018, yellow is 2007')
dev.off()    
  

png('~/Documents/GitHub/east_woods_work/figures/Fall 2018/plotwise/aspect.png')
cols = c('plots.env$PD07'='red', 'plots.env$PD18'='blue')
ggplot()+ 
  geom_point(data = plots.env, aes(x = plots.env$aspect, y = plots.env$PD07), col = 'red', fill = 'yellow', shape = 21) + 
  geom_point(data = plots.env, aes(x = plots.env$aspect, y = plots.env$PD18), col = 'blue', fill = 'darkcyan', shape = 21) + 
  xlab('Aspect') + 
  ylab('Phylogenetic Diversity') + 
  ggtitle('Aspect Effect on Diverstiy') + 
  scale_colour_manual(name="Year",values=cols, guide = guide_legend(override.aes=aes(fill=NA))) + 
  theme(legend.key = element_rect(fill = "white",colour = "white")) + 
  annotate("text", x = 220, y = 1900, label = 'Blue is 2018, yellow is 2007')
dev.off()    

theme_set(theme_minimal()) 

treeplt <- plots.env[which(plots.env$TreeCover07 != 0 & plots.env$TreeCover18 != 0),]
png('~/Documents/GitHub/east_woods_work/figures/Fall 2018/plotwise/tree_cover.png')
cols = c('treeplt$PD07'='red', 'treeplt$PD18'='blue')
ggplot()+ 
  geom_point(data = treeplt, aes(x = treeplt$TreeCover07, y = treeplt$PD07), col = 'red', fill = 'yellow', shape = 21) + 
  geom_point(data = treeplt, aes(x = treeplt$TreeCover18, y = treeplt$PD18), col = 'blue', fill = 'darkcyan', shape = 21) + 
  xlab('Tree Cover') + 
  ylab('Phylogenetic Diversity') + 
  ggtitle('Tree Cover Effect on Diverstiy') + 
  theme_minimal() + 
  scale_colour_manual(name="Year",values=cols, guide = guide_legend(override.aes=aes(fill=NA))) + 
  theme(legend.key = element_rect(fill = "white",colour = "white")) + 
  annotate("text", x = 15000, y = 2200, label = 'Blue is 2018, yellow is 2007, legends hate me')
dev.off()  


# Categorical Variables 

png('figures/Fall 2018/plotwise/fortype07.png')
ggplot(data = plots.env, aes(x = as.factor(plots.env$ForType07), y = as.numeric(plots.env$PD07), fill = plots.env$ForType07)) +
  geom_boxplot() + 
  xlab('Forest Type 07') + 
  ylab('PD07')
dev.off() 

png('figures/Fall 2018/plotwise/fortype18.png')
ggplot(data = plots.env, aes(x = as.factor(plots.env$ForType18), y = as.numeric(plots.env$PD18), fill = plots.env$ForType18)) +
  geom_boxplot() + 
  xlab('Forest Type 18') + 
  ylab('PD18')
dev.off() 


png('figures/Fall 2018/plotwise/DomGenus07.png')
ggplot(data = plots.env, aes(x = as.factor(plots.env$DomGenus07), y = as.numeric(plots.env$PD07), fill = plots.env$DomGenus07)) +
  geom_boxplot() + 
  xlab('Dom Genus 07') + 
  ylab('PD07')
dev.off() 

png('figures/Fall 2018/plotwise/DomGenus18.png')
ggplot(data = plots.env, aes(x = as.factor(plots.env$DomGenus18), y = as.numeric(plots.env$PD18), fill = plots.env$DomGenus18)) +
  geom_boxplot() + 
  xlab('Dom Genus 18') + 
  ylab('PD18')
dev.off() 


png('figures/Fall 2018/plotwise/texture07.png')
ggplot(data = plots.env, aes(x = as.factor(plots.env$texture), y = as.numeric(plots.env$PD07), fill = plots.env$texture)) +
  geom_boxplot() + 
  xlab('Texture 07') + 
  ylab('PD07')
dev.off() 

png('figures/Fall 2018/plotwise/texture18.png')
ggplot(data = plots.env, aes(x = as.factor(plots.env$texture), y = as.numeric(plots.env$PD18), fill = plots.env$texture)) +
  geom_boxplot() + 
  xlab('Texture 18') + 
  ylab('PD18')
dev.off() 





