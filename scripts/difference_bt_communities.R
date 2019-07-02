# is there a difference in environment between the groups pulled out by ecostructure?
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
require(gridExtra)
theme_set(theme_minimal())
data('plots.env')
data('ecostructure_groups')

# again.. this is only set up for k = 3

plots.env$EcoStrucGroup18 <- ifelse(rownames(plots.env)%in%group1_plots18, 'Group1', 
                                    ifelse(rownames(plots.env)%in%group2_plots18, 'Group2', 
                                           ifelse(rownames(plots.env)%in%group3_plots18, 'Group3', 'Mixed')))
plots.env$ComClass18 <- ifelse(plots.env$EcoStrucGroup18 == 'Group1', 'Deep Forest', 
                               ifelse(plots.env$EcoStrucGroup18 == 'Group2', 'Edge', 
                                      ifelse(plots.env$EcoStrucGroup18 == 'Group3', 'Disturbed', 'Mixed')))

plots.env$EcoStrucGroup07 <- ifelse(rownames(plots.env)%in%group1_plots07, 'Group1', 
                                    ifelse(rownames(plots.env)%in%group2_plots07, 'Group2', 
                                           ifelse(rownames(plots.env)%in%group3_plots07, 'Group3', 'Mixed')))

plots.env$ComClass07 <- ifelse(plots.env$EcoStrucGroup07 == 'Group1', 'Deep Forest', 
                               ifelse(plots.env$EcoStrucGroup07 == 'Group2', 'Disturbed', 
                                      ifelse(plots.env$EcoStrucGroup07 == 'Group3', 'Edge', 'Mixed')))



# for each variable in the data perform t test between each group and make plot of data distribution

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/elevation.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass07), y = as.numeric(plots.env$elev), fill = as.factor(plots.env$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Elevation') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$elev), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/slope.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass07), y = as.numeric(plots.env$slope), fill = as.factor(plots.env$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  xlab('') + 
  ylab('Slope') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$slope), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  xlab('') + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22))
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/aspect.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass07), y = as.numeric(plots.env$aspect), fill = as.factor(plots.env$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  xlab('') + 
  ylab('Aspect') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$aspect), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  xlab('') + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22))
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/treecover.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass07), y = as.numeric(plots.env$TreeCover07), fill = as.factor(plots.env$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Tree Cover') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$TreeCover18), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/burncount.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass07), y = as.numeric(plots.env$BurnCount), fill = as.factor(plots.env$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Number of Burns Reported') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$BurnCount), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/tpi.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass07), y = as.numeric(plots.env$tpi), fill = as.factor(plots.env$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('TPI') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$tpi), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/edgedist.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass07), y = as.numeric(plots.env$RoadPathDist), fill = as.factor(plots.env$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Distance from Road or Path (m)') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$RoadPathDist), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()



png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/solarrad.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass07), y = as.numeric(plots.env$SolRad), fill = as.factor(plots.env$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Solar Irradiance (W * hr/m2)') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$SolRad), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/topsoilwgt.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env[which(plots.env$TopDryWgt > -1),], aes(x = as.factor(ComClass07), y = as.numeric(TopDryWgt), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Dry Weight Top 0-10 cm Soil (g)') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env[which(plots.env$TopDryWgt > -1),], aes(x = as.factor(ComClass18), y = as.numeric(TopDryWgt), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/orgmatter.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env[which(plots.env$TopOrgMat < 200),], aes(x = as.factor(ComClass07), y = as.numeric(TopOrgMat), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('% Organic Matter') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env[which(plots.env$TopOrgMat < 200),], aes(x = as.factor(ComClass18), y = as.numeric(TopOrgMat), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/Maps/maps.png', width = 800, height = 800)
plot07 <- ggplot(data = plots.env, aes(x = lon, y = lat, col = as.factor(ComClass07), fill = as.factor(ComClass07)))+
  geom_point(shape = 21) + 
  ylab('Lat') + 
  xlab('Lon') +
  ggtitle('2007') + 
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22))
plot18 <- ggplot(data = plots.env, aes(x = lon, y = lat, col = as.factor(ComClass18), fill = as.factor(ComClass18)))+
  geom_point(shape = 21) + 
  ylab('Lat') +
  xlab('Lon') +
  ggtitle('2018') +
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22))
ggarrange(plot07, plot18, nrow = 2, ncol = 1, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Maps/maps_SR.png', width = 800, height = 800)
plot07 <- ggplot(data = plots.env, aes(x = lon, y = lat, col = as.factor(ComClass07), fill = as.factor(ComClass07), size = SR07))+
  geom_point(shape = 21) + 
  ylab('Lat') + 
  xlab('Lon') +
  ggtitle('2007') + 
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22))
plot18 <- ggplot(data = plots.env, aes(x = lon, y = lat, col = as.factor(ComClass18), fill = as.factor(ComClass18), size = SR18))+
  geom_point(shape = 21) + 
  ylab('Lat') +
  xlab('Lon') +
  ggtitle('2018') +
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22))
ggarrange(plot07, plot18, nrow = 2, ncol = 1, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Maps/maps_PD.png', width = 800, height = 800)
plot07 <- ggplot(data = plots.env, aes(x = lon, y = lat, col = as.factor(ComClass07), fill = as.factor(ComClass07), size = PD07))+
  geom_point(shape = 21) + 
  ylab('Lat') + 
  xlab('Lon') +
  ggtitle('2007') + 
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22))
plot18 <- ggplot(data = plots.env, aes(x = lon, y = lat, col = as.factor(ComClass18), fill = as.factor(ComClass18), size = PD18))+
  geom_point(shape = 21) + 
  ylab('Lat') +
  xlab('Lon') +
  ggtitle('2018') +
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22))
ggarrange(plot07, plot18, nrow = 2, ncol = 1, common.legend = T)
dev.off()

# a little more analytical but what are the diversity differences between communities???

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/SR.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env[which(plots.env$TopDryWgt > -1),], aes(x = as.factor(ComClass07), y = as.numeric(SR07), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Species Richness') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env[which(plots.env$TopDryWgt > -1),], aes(x = as.factor(ComClass18), y = as.numeric(SR18), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/PD.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env[which(plots.env$TopDryWgt > -1),], aes(x = as.factor(ComClass07), y = as.numeric(PD07), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Phylogenetic Diversity') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env[which(plots.env$TopDryWgt > -1),], aes(x = as.factor(ComClass18), y = as.numeric(PD18), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/MNTD.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env[which(plots.env$TopDryWgt > -1),], aes(x = as.factor(ComClass07), y = as.numeric(MNTD07), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Phylogenetic Beta Diversity') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env[which(plots.env$TopDryWgt > -1),], aes(x = as.factor(ComClass18), y = as.numeric(MNTD18), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/PSV.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env[which(plots.env$TopDryWgt > -1),], aes(x = as.factor(ComClass07), y = as.numeric(PSV07), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Phylogenetic Species Variance') + 
  ggtitle('2007') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))
plot18 <- ggplot(data = plots.env[which(plots.env$TopDryWgt > -1),], aes(x = as.factor(ComClass18), y = as.numeric(PSV18), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(label = 'p.signif', method = 't.test', ref.group = '.all.') + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/SRvPD.png', width = 1000, height = 600)
plot07 <- ggplot(data = plots.env, aes(x = SR07, y = PD07, col = ComClass07)) + 
  geom_point(shape = 21) + 
  xlab('Species Richness') + 
  ylab('Phylogenetic Diversity') + 
  ggtitle('2007')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22), 
        legend.position = 'none')
plot18 <- ggplot(data = plots.env, aes(x = SR18, y = PD18, col = ComClass18)) + 
  geom_point(shape = 21) + 
  xlab('Species Richness') + 
  ylab('') + 
  ggtitle('2018')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22), 
        legend.title = element_blank())
ggarrange(plot07, plot18, nrow = 1, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/SRvPSV.png', width = 1000, height = 600)
plot07 <- ggplot(data = plots.env, aes(x = SR07, y = PSV07, col = ComClass07, shape = ComClass07)) + 
  geom_point() + 
  xlab('Species Richness') + 
  ylab('Phylogenetic Species Variance') + 
  ggtitle('2007')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22), 
        legend.position = 'none')
plot18 <- ggplot(data = plots.env, aes(x = SR18, y = PSV07, col = ComClass18, shape = ComClass18)) + 
  geom_point() + 
  xlab('Species Richness') + 
  ylab('') + 
  ggtitle('2018')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22), 
        legend.title = element_blank())
ggarrange(plot07, plot18, nrow = 1, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/SRvPSV_groups.png', width = 1000, height = 600)
group1.07 <- ggplot(data = plots.env[which(plots.env$ComClass07 == 'Group1'),], aes(x = SR07, y = PSV07)) + 
  geom_point(col = '#F8766D', shape = 17) + 
  xlab('Species Richness') + 
  ylab('2007 PSV') + 
  ggtitle('Group 1')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22), 
        axis.title.y = element_text(face = 'bold', size = 22))
group2.07 <- ggplot(data = plots.env[which(plots.env$ComClass07 == 'Group2'),], aes(x = SR18, y = PSV07, col = ComClass07)) + 
  geom_point(col = '#7CAE00', shape = 18) + 
  xlab('Species Richness') + 
  ylab('') + 
  ggtitle('Group 2')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22))
group3.07 <- ggplot(data = plots.env[which(plots.env$ComClass07 == 'Group3'),], aes(x = SR07, y = PSV07, col = ComClass07)) + 
  geom_point(col = '#00BFC4', shape = 15) + 
  xlab('Species Richness') + 
  ylab('') + 
  ggtitle('Group 3')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22))
group4.07 <- ggplot(data = plots.env[which(plots.env$ComClass07 == 'Mixed'),], aes(x = SR07, y = PSV07, col = ComClass07)) + 
  geom_point(col = '#C77CFF', shape = 23) + 
  xlab('Species Richness') + 
  ylab('') + 
  ggtitle('Mixed')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22))

group1.18 <- ggplot(data = plots.env[which(plots.env$ComClass18 == 'Group1'),], aes(x = SR18, y = PSV18)) + 
  geom_point(col = '#F8766D', shape = 17) + 
  xlab('Species Richness') + 
  ylab('2018 PSV') + 
  ggtitle('Group 1')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22), 
        axis.title.y = element_text(face = 'bold', size = 22))
group2.18 <- ggplot(data = plots.env[which(plots.env$ComClass18 == 'Group2'),], aes(x = SR18, y = PSV18, col = ComClass18)) + 
  geom_point(col = '#7CAE00', shape = 18) + 
  xlab('Species Richness') + 
  ylab('') + 
  ggtitle('Group 2')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22))
group3.18 <- ggplot(data = plots.env[which(plots.env$ComClass18 == 'Group3'),], aes(x = SR18, y = PSV18, col = ComClass18)) + 
  geom_point(col = '#00BFC4', shape = 15) + 
  xlab('Species Richness') + 
  ylab('') + 
  ggtitle('Group 3')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22))
group4.18 <- ggplot(data = plots.env[which(plots.env$ComClass18 == 'Mixed'),], aes(x = SR18, y = PSV18, col = ComClass18)) + 
  geom_point(col = '#C77CFF', shape = 23) + 
  xlab('Species Richness') + 
  ylab('') + 
  ggtitle('Mixed')+ 
  theme(aspect.ratio = 1:1, 
        plot.title = element_text(face = 'bold', size = 22))

ggarrange(group1.07, group2.07, group3.07, group4.07, group1.18, group2.18, group3.18, group4.18, nrow = 2, ncol = 4, common.legend = T)
dev.off()


# burn relationship to group 

data <- as.data.frame(table(plots.env$EcoStrucGroup07[which(plots.env$MgmtUnit == 'Annual Burn')]))
data <- cbind(data, as.data.frame(table(plots.env$EcoStrucGroup07[which(plots.env$MgmtUnit == 'Mixed Management')])))
data <- cbind(data, as.data.frame(table(plots.env$EcoStrucGroup07[which(plots.env$MgmtUnit == 'Non-Wooded')])))
data <- cbind(data, as.data.frame(table(plots.env$EcoStrucGroup07[which(plots.env$MgmtUnit == 'Hidden Lake')])))
rownames(data) <- data$Var1
data$Var1 <- NULL
names(data) <- c('Annual Burn', 'Mixed Management', 'Non-Wooded', 'Hidden Lake')

coul = brewer.pal(3, "Pastel2") 
#Transform this data in %
data_percentage=apply(data, 2, function(x){x*100/sum(x,na.rm=T)})

# cant beleive this but Im an idiot and cant format a table
group <- c(rep('Annual Burn', 4), rep('Mixed Management', 4), rep('Non-Wooded', 4), rep('Hidden Lake', 4))
value <- c(5.88, 14.705, 20.588, 58.823, 41.232, 1.895, 07.009, 38.862, 12.578, 47.169, 3.144, 37.106, 51.25, 7.5, 1.25, 40.00)
clust <- c(rep(c('Group1', 'Group2', 'Group3', 'Mixed'), 4))
data <- data.frame(group, value, clust)

# Make a stacked barplot--> it will be in %!
png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/2007/GroupVariation/management_effects.png', width = 700, height = 600)
#barplot(data_percentage, col=coul , border="white", xlab="group")
ggplot(data, aes(fill=clust, y=value, x=group)) + 
  geom_bar(stat="identity", position="fill")
dev.off()



