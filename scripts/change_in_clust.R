library(ggplot2)
theme_set(theme_minimal()) 

data('plots.env')

omega_07 <- 'figures/Summer 2019/EcoStructure/Omega/k3dat_mat_all_07.csv'
omega_18 <- 'figures/Summer 2019/EcoStructure/Omega/k3dat_mat_all_18.csv'

k2_2007 <- read.csv(omega_07)
k2_2018 <- read.csv(omega_18)
change_analysis <-cbind(k2_2018, k2_2007)

rownames(change_analysis) <- change_analysis$X
change_analysis$X <- NULL
change_analysis$X <- NULL

names(change_analysis) <- c('g1.2018', 'g2.2018', 'g1.2007', 'g2.2007')
change_analysis$g1.2018 <- as.numeric(change_analysis$g1.2018)
change_analysis$g2.2018 <- as.numeric(change_analysis$g2.2018)
change_analysis$g1.2007 <- as.numeric(change_analysis$g1.2007)
change_analysis$g2.2007 <- as.numeric(change_analysis$g2.2007)
change_analysis$ChangeGroup1 <- abs(change_analysis$g1.2018 - change_analysis$g1.2007)
change_analysis$ChangeGroup2 <- abs(change_analysis$g2.2018 - change_analysis$g2.2007)
change_analysis$OverallChange <- change_analysis$ChangeGroup1 + change_analysis$ChangeGroup2


change_plots <- plots.env[which(rownames(plots.env) %in% rownames(change_analysis)),]
change_plots$Group1Mem18 <- change_analysis$g1.2018[match(rownames(change_analysis_k2), rownames(change_plots))]
change_plots$Group1Mem07 <- change_analysis$g1.2007[match(rownames(change_analysis_k2), rownames(change_plots))]
change_plots$Changek2 <- change_analysis_k2$OverallChange[match(rownames(change_analysis_k2), rownames(change_plots))]

# are the PSV and variation in grup 1 memberhsip correlated?
png('figures/Summer 2019/EcoStructure/Omega/Regression/2018/PSV_group1_18.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Group1Mem18), y = as.numeric(change_plots$PSV18)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Membership in Group 1') +
  ylab('PSV') 
dev.off()


# how do the environmental variables predict Group 1 membership in 2007?
png('figures/Summer 2019/EcoStructure/Omega/Regression/2007/elev_group1_07.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Group1Mem07), y = as.numeric(change_plots$elev), col = change_plots$Group1Mem07))+ 
  geom_point(shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Membership in Group 1') +
  ylab('Elevation') 
dev.off()

# # option for plotting years togehter?? a bit messy looking and legend is not working 
# png('~/Documents/GitHub/east_woods_work/figures/Summer 2019/EcoStructure/Omega/Regression/aspect.png')
# ggplot()+ 
#   geom_point(data = change_plots, aes(x = change_plots$aspect, y = change_plots$Group1Mem07), col = 'red', fill = 'yellow', shape = 21) + 
#   geom_point(data = change_plots, aes(x = change_plots$aspect, y = change_plots$Group1Mem18), col = 'blue', fill = 'darkcyan', shape = 21) + 
#   xlab('Aspect') + 
#   ylab('Membership in group 1') + 
#   ggtitle('Aspect') + 
#   scale_colour_manual(name="Year",values=cols, guide = guide_legend(override.aes=aes(fill=NA))) + 
#   theme(legend.key = element_rect(fill = "white",colour = "white")) + 
#   annotate("text", x = 220, y = 0.9, label = 'Blue is 2018, yellow is 2007')
# dev.off()    

  
png('figures/Summer 2019/EcoStructure/Omega/Regression/2007/aspect_group1_07.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Group1Mem07), y = as.numeric(change_plots$aspect)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Membership in Group 1') +
  ylab('Aspect') 
dev.off()
  
png('figures/Summer 2019/EcoStructure/Omega/Regression/2007/slope_group1_07.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Group1Mem07), y = as.numeric(change_plots$slope)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Membership in Group 1') +
  ylab('Slope') 
dev.off() 

treeplt <- change_plots[which(change_plots$TreeCover07 != 0),]
png('figures/Summer 2019/EcoStructure/Omega/Regression/2007/treecover_group1_07.png')
ggplot(data = treeplt, aes(x = as.numeric(treeplt$Group1Mem07), y = as.numeric(treeplt$TreeCover07)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Membership in Group 1') +
  ylab('Tree Cover') 
dev.off() 


# how do the environmental variables predict Group 1 membership in 2018?
png('figures/Summer 2019/EcoStructure/Omega/Regression/2018/elev_group1_18.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Group1Mem18), y = as.numeric(change_plots$elev)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Membership in Group 1') +
  ylab('Elevation') 
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Regression/2018/aspect_group1_18.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Group1Mem18), y = as.numeric(change_plots$aspect)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Membership in Group 1') +
  ylab('Aspect') 
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Regression/2018/slope_group1_18.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Group1Mem18), y = as.numeric(change_plots$slope)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Membership in Group 1') +
  ylab('Slope') 
dev.off() 

treeplt <- change_plots[which(change_plots$TreeCover18 != 0),]
png('figures/Summer 2019/EcoStructure/Omega/Regression/2018/treecover_group1_18.png')
ggplot(data = treeplt, aes(x = as.numeric(treeplt$Group1Mem18), y = as.numeric(treeplt$TreeCover18)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Membership in Group 1') +
  ylab('Tree Cover') 
dev.off() 

# plot change 

png('figures/Summer 2019/EcoStructure/Omega/Regression/change/elev_change.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Changek2), y = as.numeric(change_plots$elev)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Change (both groups)') +
  ylab('Elevation') 
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Regression/change/aspect_change.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Changek2), y = as.numeric(change_plots$aspect)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Change (both groups)') +
  ylab('Aspect') 
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Regression/change/slope_change.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Changek2), y = as.numeric(change_plots$slope)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Change (both groups)') +
  ylab('Slope') 
dev.off() 

# is the change in tree cover related to whether a plot changed populations? 
change_plots$Tree_change <- abs(change_plots$TreeCover18 - change_plots$TreeCover07)
png('figures/Summer 2019/EcoStructure/Omega/Regression/change/tree_cover_change.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$Tree_change), y = as.numeric(change_plots$Changek2)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('Tree Change') +
  ylab('Amount of Plot Change') 
dev.off() 

# how does phylogenetic diversity affect whether a plot changed populations? is there a relationship between PD in 07 and plot stability
png('figures/Summer 2019/EcoStructure/Omega/Regression/change/PD07_change.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$PD07), y = as.numeric(change_plots$Changek2)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  ylab('Change (both groups)') +
  xlab('PD 07')
dev.off() 

# does the change in phylogenetic species variance correspond to the change in population? 
change_plots$changevar <- abs(change_plots$PSV18 - change_plots$PSV07)
change_plots$Group1change <- change_plots$Group1Mem18 - change_plots$Group1Mem07
png('figures/Summer 2019/EcoStructure/Omega/Regression/change/PSV_change.png')
ggplot(data = change_plots, aes(x = as.numeric(change_plots$changevar), y = as.numeric(change_plots$Changek2)))+ 
  geom_point(col = 'red', fill = 'yellow', shape = 21) + 
  geom_smooth(method = 'lm', formula = y~x, color = 'red') +
  xlab('PSV change') +
  ylab('Change (both groups)')
dev.off() 


# Categorical Variables 

#_-------
# png('figures/Summer 2019/EcoStructure/Omega/Fortype07_group1mem07.png')
# ggplot(data = change_plots, aes(x = as.factor(change_plots$ForType07), y = as.numeric(change_plots$Group1Mem07), fill = change_plots$ForType07)) +
#   geom_boxplot() + 
#   xlab('Forest Type 07') + 
#   ylab('Group 1 Membership 07')
# dev.off() 
# 
# png('figures/Summer 2019/EcoStructure/Omega/Fortype18_group1mem18.png')
# ggplot(data = change_plots, aes(x = as.factor(change_plots$ForType18), y = as.numeric(change_plots$Group1Mem18), fill = change_plots$ForType18)) +
#   geom_boxplot() + 
#   xlab('Forest Type 18') + 
#   ylab('Group 1 Membership 18')
# dev.off() 
# 
# 
# png('figures/Fall 2018/plotwise/DomGenus07.png')
# ggplot(data = change_plots, aes(x = as.factor(change_plots$DomGenus07), y = as.numeric(change_plots$PD07), fill = change_plots$DomGenus07)) +
#   geom_boxplot() + 
#   xlab('Dom Genus 07') + 
#   ylab('PD07')
# dev.off() 
# 
# png('figures/Fall 2018/plotwise/DomGenus18.png')
# ggplot(data = change_plots, aes(x = as.factor(change_plots$DomGenus18), y = as.numeric(change_plots$PD18), fill = change_plots$DomGenus18)) +
#   geom_boxplot() + 
#   xlab('Dom Genus 18') + 
#   ylab('PD18')
# dev.off() 
# 
# 
# png('figures/Summer 2019/EcoStructure/Omega/texture07.png')
# ggplot(data = change_plots, aes(x = as.factor(change_plots$texture), y = as.numeric(change_plots$Group1Mem07), fill = change_plots$texture)) +
#   geom_boxplot() + 
#   xlab('Texture 07') + 
#   ylab('Group 1 Membership 07')
# dev.off() 
# 
# png('figures/Summer 2019/EcoStructure/Omega/texture18.png')
# ggplot(data = change_plots, aes(x = as.factor(change_plots$texture), y = as.numeric(change_plots$Group1Mem18), fill = change_plots$texture)) +
#   geom_boxplot() + 
#   xlab('Texture 18') + 
#   ylab('Group 1 Membership 18')
# dev.off() 
# 
# 
# png('figures/Summer 2019/EcoStructure/Omega/watertbl07.png')
# ggplot(data = change_plots, aes(x = as.factor(change_plots$watertbl), y = as.numeric(change_plots$Group1Mem07), fill = change_plots$watertbl)) +
#   geom_boxplot() + 
#   xlab('watertbl 07') + 
#   ylab('Group 1 Membership 07')
# dev.off() 
# 
# png('figures/Summer 2019/EcoStructure/Omega/watertbl18.png')
# ggplot(data = change_plots, aes(x = as.factor(change_plots$watertbl), y = as.numeric(change_plots$Group1Mem18), fill = change_plots$watertbl)) +
#   geom_boxplot() + 
#   xlab('watertbl 18') + 
#   ylab('Group 1 Membership 18')
# dev.off() 
# 
# 
# png('figures/Summer 2019/EcoStructure/Omega/orgmatter07.png')
# ggplot(data = change_plots, aes(x = as.factor(change_plots$orgmatter), y = as.numeric(change_plots$Group1Mem07), fill = change_plots$orgmatter)) +
#   geom_boxplot() + 
#   xlab('orgmatter 07') + 
#   ylab('Group 1 Membership 07')
# dev.off() 
# 
# png('figures/Summer 2019/EcoStructure/Omega/orgmatter18.png')
# ggplot(data = change_plots, aes(x = as.factor(change_plots$orgmatter), y = as.numeric(change_plots$Group1Mem18), fill = change_plots$orgmatter)) +
#   geom_boxplot() + 
#   xlab('orgmatter 18') + 
#   ylab('Group 1 Membership 18')
# dev.off() 
#-----------




