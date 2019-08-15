# is there a difference in environment between the groups pulled out by ecostructure?
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
require(gridExtra)
theme_set(theme_minimal())
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
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

save(plots.env, file = 'data/plots.env.RData')

ew_plots <- plots.env[which(plots.env$wooded == 'East Woods'),]
hl_plots <- plots.env[which(plots.env$wooded == 'Hidden Lake'),]
# for each variable in the data perform anova between groups and make plot of data distribution
#-----

my_comparisons = list(c('Disturbed', 'Edge'), c('Edge', 'Mixed'), c('Deep Forest', 'Disturbed'), c('Deep Forest', 'Edge'), c('Disturbed', 'Mixed'), c('Deep Forest', 'Mixed'))

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_elevation.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass07), y = as.numeric(hl_plots$elev), fill = as.factor(hl_plots$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Elevation') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass18), y = as.numeric(hl_plots$elev), fill = as.factor(hl_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_slope.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass07), y = as.numeric(hl_plots$slope), fill = as.factor(hl_plots$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  xlab('') + 
  ylab('Slope') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass18), y = as.numeric(hl_plots$slope), fill = as.factor(hl_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  xlab('') + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22)) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_aspect.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass07), y = as.numeric(hl_plots$aspect), fill = as.factor(hl_plots$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  xlab('') + 
  ylab('Aspect') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass18), y = as.numeric(hl_plots$aspect), fill = as.factor(hl_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  xlab('') + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22)) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/compare_elev18.png', width = 1000, height = 500)
all <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$elev), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Elevation') +
  ggtitle('All Plots') + 
  stat_compare_means(comparisons = my_comparisons)+ 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
hl <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass18), y = as.numeric(hl_plots$elev), fill = as.factor(hl_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') + 
  ggtitle('Hidden Lake') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ew <- ggplot(data = ew_plots, aes(x = as.factor(ew_plots$ComClass18), y = as.numeric(ew_plots$elev), fill = as.factor(ew_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('East Woods') + 
  stat_compare_means(comparisons = my_comparisons)+ 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
fig <- ggarrange(all, hl, ew,  ncol = 3, common.legend = T)
annotate_figure(fig, top = text_grob('Elevation', face = 'bold', size = 27))
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/compare_slope18.png', width = 1000, height = 500)
all <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$slope), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Slope') +
  ggtitle('All Plots') + 
  stat_compare_means(comparisons = my_comparisons)+ 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
hl <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass18), y = as.numeric(hl_plots$slope), fill = as.factor(hl_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') + 
  ggtitle('Hidden Lake') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ew <- ggplot(data = ew_plots, aes(x = as.factor(ew_plots$ComClass18), y = as.numeric(ew_plots$slope), fill = as.factor(ew_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('East Woods') + 
  stat_compare_means(comparisons = my_comparisons)+ 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
fig <- ggarrange(all, hl, ew,  ncol = 3, common.legend = T)
annotate_figure(fig, top = text_grob('Slope', face = 'bold', size = 27))
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/compare_aspect18.png', width = 1000, height = 500)
all <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$aspect), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Aspect') +
  ggtitle('All Plots') + 
  stat_compare_means(comparisons = my_comparisons)+ 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
hl <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass18), y = as.numeric(hl_plots$aspect), fill = as.factor(hl_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') + 
  ggtitle('Hidden Lake') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ew <- ggplot(data = ew_plots, aes(x = as.factor(ew_plots$ComClass18), y = as.numeric(ew_plots$aspect), fill = as.factor(ew_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('East Woods') + 
  stat_compare_means(comparisons = my_comparisons)+ 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
fig <- ggarrange(all, hl, ew,  ncol = 3, common.legend = T)
annotate_figure(fig, top = text_grob('Aspect', face = 'bold', size = 27))
dev.off()



png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_treecover.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass07), y = as.numeric(hl_plots$TreeCover07), fill = as.factor(hl_plots$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Tree Cover') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass18), y = as.numeric(hl_plots$TreeCover18), fill = as.factor(hl_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Tree Cover') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons)+ 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/EastWoods_burncount.png', width = 500, height = 1000)
plot07 <- ggplot(data = ew_plots, aes(x = as.factor(ew_plots$ComClass07), y = as.numeric(ew_plots$BurnCount07), fill = as.factor(ew_plots$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Number of Burns Reported') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = ew_plots, aes(x = as.factor(ew_plots$ComClass18), y = as.numeric(ew_plots$BurnCount), fill = as.factor(ew_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Number of Burns Reported') + 
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, nrow = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_tpi.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass07), y = as.numeric(hl_plots$tpi), fill = as.factor(hl_plots$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('TPI') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass18), y = as.numeric(hl_plots$tpi), fill = as.factor(hl_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_edgedist.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass07), y = as.numeric(hl_plots$RoadPathDist), fill = as.factor(hl_plots$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Distance from Road or Path (m)') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass18), y = as.numeric(hl_plots$RoadPathDist), fill = as.factor(hl_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/roaddist.png', width = 800, height = 500)
plot07 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass07), y = as.numeric(plots.env$RoadDist), fill = as.factor(plots.env$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Distance from Road (m)') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = plots.env, aes(x = as.factor(plots.env$ComClass18), y = as.numeric(plots.env$RoadDist), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_solarrad.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass07), y = as.numeric(hl_plots$SolRad), fill = as.factor(hl_plots$ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Solar Irradiance (W * hr/m2)') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(hl_plots$ComClass18), y = as.numeric(hl_plots$SolRad), fill = as.factor(hl_plots$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()


png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/hl_plots_topsoil.png', width = 1500, height = 700)
p1 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass18), y = as.numeric(TopDryWgt), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) +
  ylab('Dry Weight Soil (g)') +
  stat_compare_means(comparisons = my_comparisons) +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = 'bold', size = 19)) +
  scale_fill_manual(values = cbbPalette)
p2 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass18), y = as.numeric(TopWetWgt), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) +
  ylab('Wet Weight Soil (g)') +
  stat_compare_means(comparisons = my_comparisons) +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = 'bold', size = 19)) +
  scale_fill_manual(values = cbbPalette)
p3 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass18), y = as.numeric(TopWtrContent), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) +
  ylab('% Water Content') +
  stat_compare_means(comparisons = my_comparisons) +
  theme(legend.title = element_blank(),
        plot.title = element_text(face='bold', size = 18),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) +
  scale_fill_manual(values = cbbPalette)
p4 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass18), y = as.numeric(TopOrgMat), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) +
  ylab('% Organic Matter') +
  stat_compare_means(comparisons = my_comparisons) +
  theme(legend.title = element_blank(),
        plot.title = element_text(face='bold', size = 18),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) +
  scale_fill_manual(values = cbbPalette)

fig <- ggarrange(p1, p2, p3, p4, ncol = 4, common.legend = T)
annotate_figure(fig, 
                top = text_grob('0-10 cm', size = 25, face = 'bold'))
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/hl_plots_lowsoil.png', width = 1500, height = 700)
p1 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass18), y = as.numeric(LowDryWgt), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) +
  ylab('Dry Weight Soil (g)') +
  stat_compare_means(comparisons = my_comparisons) +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = 'bold', size = 19)) +
  scale_fill_manual(values = cbbPalette)
p2 <- ggplot(data = hl_plots[which(hl_plots$LowWetWgt < 2000),], aes(x = as.factor(ComClass18), y = as.numeric(LowWetWgt), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) +
  ylab('Wet Weight Soil (g)') +
  stat_compare_means(comparisons = my_comparisons) +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = 'bold', size = 19)) +
  scale_fill_manual(values = cbbPalette)
p3 <- ggplot(data = hl_plots[which(hl_plots$LowWtrContent < 200 & hl_plots$LowWtrContent > -400),], aes(x = as.factor(ComClass18), y = as.numeric(LowWtrContent), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) +
  ylab('% Water Content') +
  stat_compare_means(comparisons = my_comparisons) +
  theme(legend.title = element_blank(),
        plot.title = element_text(face='bold', size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = 'bold', size = 19)) +
  scale_fill_manual(values = cbbPalette)
p4 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass18), y = as.numeric(LowOrgMat), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) +
  ylab('% Organic Matter') +
  stat_compare_means(comparisons = my_comparisons) +
  theme(legend.title = element_blank(),
        plot.title = element_text(face='bold', size = 18),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) +
  scale_fill_manual(values = cbbPalette)

fig <- ggarrange(p1, p2, p3, p4, ncol = 4, common.legend = T)
annotate_figure(fig, 
                top = text_grob('10-20 cm', size = 25, face = 'bold'))
dev.off()

# png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/drysoilwgt.png', width = 800, height = 800)
# topplot07 <- ggplot(data = hl_plots[which(hl_plots$TopDryWgt > -1),], aes(x = as.factor(ComClass07), y = as.numeric(TopDryWgt), fill = as.factor(ComClass07)))+
#   geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
#   ylab('Dry Weight Top 0-10 cm Soil (g)') + 
#   ggtitle('2007') + 
#   stat_compare_means(comparisons = my_comparisons) + 
#   theme(legend.title = element_blank(), 
#         legend.position = 'none', 
#         plot.title = element_text(face='bold', size = 22), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_text(face = 'bold', size = 19)) + 
#   scale_fill_manual(values = cbbPalette)
# topplot18 <- ggplot(data = hl_plots[which(hl_plots$TopDryWgt > -1),], aes(x = as.factor(ComClass18), y = as.numeric(TopDryWgt), fill = as.factor(ComClass18)))+
#   geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
#   ylab('') +
#   ggtitle('2018') + 
#   stat_compare_means(comparisons = my_comparisons) + 
#   theme(legend.title = element_blank(), 
#         plot.title = element_text(face='bold', size = 22),
#         axis.title.x = element_blank()) + 
#   scale_fill_manual(values = cbbPalette)
# lowplot07 <- ggplot(data = hl_plots[which(hl_plots$LowDryWgt > -1),], aes(x = as.factor(ComClass07), y = as.numeric(LowDryWgt), fill = as.factor(ComClass07)))+
#   geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
#   ylab('Dry Weight Top 0-10 cm Soil (g)') + 
#   ggtitle('2007') + 
#   stat_compare_means(comparisons = my_comparisons) + 
#   theme(legend.title = element_blank(), 
#         legend.position = 'none', 
#         plot.title = element_text(face='bold', size = 22), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_text(face = 'bold', size = 19)) + 
#   scale_fill_manual(values = cbbPalette)
# lowplot18 <- ggplot(data = hl_plots[which(hl_plots$LowDryWgt > -1),], aes(x = as.factor(ComClass18), y = as.numeric(LowDryWgt), fill = as.factor(ComClass18)))+
#   geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
#   ylab('') +
#   ggtitle('2018') + 
#   stat_compare_means(comparisons = my_comparisons) + 
#   theme(legend.title = element_blank(), 
#         plot.title = element_text(face='bold', size = 22),
#         axis.title.x = element_blank()) + 
#   scale_fill_manual(values = cbbPalette)
# 
# ggarrange(topplot07, topplot18, lowplot07, lowplot18, ncol = 2, nrow = 2, common.legend = T)
# dev.off()
# 
# png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/wetsoilwgt.png', width = 800, height = 800)
# topplot07 <- ggplot(data = hl_plots[which(hl_plots$TopWetWgt > -1),], aes(x = as.factor(ComClass07), y = as.numeric(TopWetWgt), fill = as.factor(ComClass07)))+
#   geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
#   ylab('wet Weight Top 0-10 cm Soil (g)') + 
#   ggtitle('2007') + 
#   stat_compare_means(comparisons = my_comparisons) + 
#   theme(legend.title = element_blank(), 
#         legend.position = 'none', 
#         plot.title = element_text(face='bold', size = 22), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_text(face = 'bold', size = 19)) + 
#   scale_fill_manual(values = cbbPalette)
# topplot18 <- ggplot(data = hl_plots[which(hl_plots$TopWetWgt > -1),], aes(x = as.factor(ComClass18), y = as.numeric(TopWetWgt), fill = as.factor(ComClass18)))+
#   geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
#   ylab('') +
#   ggtitle('2018') + 
#   stat_compare_means(comparisons = my_comparisons) + 
#   theme(legend.title = element_blank(), 
#         plot.title = element_text(face='bold', size = 22),
#         axis.title.x = element_blank()) + 
#   scale_fill_manual(values = cbbPalette)
# lowplot07 <- ggplot(data = hl_plots[which(hl_plots$LowWetWgt > -1),], aes(x = as.factor(ComClass07), y = as.numeric(LowWetWgt), fill = as.factor(ComClass07)))+
#   geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
#   ylab('wet Weight Top 0-10 cm Soil (g)') + 
#   ggtitle('2007') + 
#   stat_compare_means(comparisons = my_comparisons) + 
#   theme(legend.title = element_blank(), 
#         legend.position = 'none', 
#         plot.title = element_text(face='bold', size = 22), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_text(face = 'bold', size = 19)) + 
#   scale_fill_manual(values = cbbPalette)
# lowplot18 <- ggplot(data = hl_plots[which(hl_plots$LowWetWgt > -1),], aes(x = as.factor(ComClass18), y = as.numeric(LowWetWgt), fill = as.factor(ComClass18)))+
#   geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
#   ylab('') +
#   ggtitle('2018') + 
#   stat_compare_means(comparisons = my_comparisons) + 
#   theme(legend.title = element_blank(), 
#         plot.title = element_text(face='bold', size = 22),
#         axis.title.x = element_blank()) + 
#   scale_fill_manual(values = cbbPalette)
# 
# ggarrange(topplot07, topplot18, lowplot07, lowplot18, ncol = 2, nrow = 2, common.legend = T)
# dev.off()
# 
# 
png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/orgmatter.png', width = 800, height = 500)
plottop <- ggplot(data = plots.env[which(plots.env$TopOrgMat < 200),], aes(x = as.factor(ComClass07), y = as.numeric(TopOrgMat), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) +
  ylab('% Organic Matter') +
  ggtitle('0-10 cm') +
  stat_compare_means(comparisons = my_comparisons) +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = 'bold', size = 19)) +
  scale_fill_manual(values = cbbPalette)

plotlow <- ggplot(data = plots.env[which(plots.env$TopOrgMat < 200),], aes(x = as.factor(ComClass18), y = as.numeric(LowOrgMat), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) +
  ylab('') +
  ggtitle('10-20 cm') +
  stat_compare_means(comparisons = my_comparisons) +
  theme(legend.title = element_blank(),
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) +
  scale_fill_manual(values = cbbPalette)
ggarrange(plottop, plotlow, ncol = 2, common.legend = T)
dev.off()
# 
# png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/watercontent.png', width = 800, height = 500)
# plot07 <- ggplot(data = hl_plots[which(hl_plots$TopWtrContent < 200),], aes(x = as.factor(ComClass07), y = as.numeric(TopWtrContent), fill = as.factor(ComClass07)))+
#   geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
#   ylab('Water Content Topsoil') + 
#   ggtitle('2007') + 
#   stat_compare_means(comparisons = my_comparisons) + 
#   theme(legend.title = element_blank(), 
#         legend.position = 'none', 
#         plot.title = element_text(face='bold', size = 22), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_text(face = 'bold', size = 19)) + 
#   scale_fill_manual(values = cbbPalette)
# plot18 <- ggplot(data = hl_plots[which(hl_plots$TopWtrContent < 200),], aes(x = as.factor(ComClass18), y = as.numeric(TopWtrContent), fill = as.factor(ComClass18)))+
#   geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
#   ylab('') +
#   ggtitle('2018') + 
#   stat_compare_means(comparisons = my_comparisons) + 
#   theme(legend.title = element_blank(), 
#         plot.title = element_text(face='bold', size = 22),
#         axis.title.x = element_blank()) + 
#   scale_fill_manual(values = cbbPalette)
# ggarrange(plot07, plot18, ncol = 2, common.legend = T)
# dev.off()
# 

#-----

# maps to show diversity 
#------
png('figures/Summer 2019/EcoStructure/Omega/Maps/HiddenLake_maps.png', width = 800, height = 1000)
plot07 <- ggplot(data = hl_plots, aes(x = lon, y = lat, col = as.factor(ComClass07), fill = as.factor(ComClass07), shape = ComClass07))+
  geom_point(size = 3, stroke = 2) + 
  ylab('Lat') + 
  xlab('Lon') +
  ggtitle('2007') + 
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22)) + 
  scale_color_manual('', values = cbbPalette) + 
  scale_shape_manual('', values = c(15, 16, 17, 18))
plot18 <- ggplot(data = hl_plots, aes(x = lon, y = lat, col = as.factor(ComClass18), fill = as.factor(ComClass18), shape = ComClass18))+
  geom_point(size = 3, stroke = 2) + 
  ylab('Lat') +
  xlab('Lon') +
  ggtitle('2018') +
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22)) + 
  scale_color_manual('', values = cbbPalette) + 
  scale_shape_manual('', values = c(15, 16, 17, 18))
ggarrange(plot07, plot18, nrow = 2, ncol = 1, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Maps/HiddenLake_maps_SR.png', width = 800, height = 1000)
plot07 <- ggplot(data = hl_plots, aes(x = lon, y = lat, col = as.factor(ComClass07), fill = as.factor(ComClass07), size = SR07, shape = ComClass07))+
  geom_point(stroke = 2) + 
  ylab('Lat') + 
  xlab('Lon') +
  ggtitle('2007') + 
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22)) + 
  scale_color_manual('', values = cbbPalette) + 
  scale_shape_manual('', values = c(15, 16, 17, 18))
plot18 <- ggplot(data = hl_plots, aes(x = lon, y = lat, col = as.factor(ComClass18), fill = as.factor(ComClass18), size = SR18, shape = ComClass18))+
  geom_point(stroke = 2) + 
  ylab('Lat') +
  xlab('Lon') +
  ggtitle('2018') +
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22)) + 
  scale_color_manual('', values = cbbPalette) + 
  scale_shape_manual('', values = c(15, 16, 17, 18))
ggarrange(plot07, plot18, nrow = 2, ncol = 1, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Maps/HiddenLake_maps_PD.png', width = 800, height = 1000)
plot07 <- ggplot(data = hl_plots, aes(x = lon, y = lat, col = as.factor(ComClass07), fill = as.factor(ComClass07), size = PD07, shape = ComClass07))+
  geom_point(stroke = 2) + 
  ylab('Lat') + 
  xlab('Lon') +
  ggtitle('2007') + 
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22)) + 
  scale_color_manual('', values = cbbPalette) + 
  scale_shape_manual('', values = c(15, 16, 17, 18))
plot18 <- ggplot(data = hl_plots, aes(x = lon, y = lat, col = as.factor(ComClass18), fill = as.factor(ComClass18), size = PD18, shape = ComClass18))+
  geom_point(stroke =2) + 
  ylab('Lat') +
  xlab('Lon') +
  ggtitle('2018') +
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22)) + 
  scale_color_manual('', values = cbbPalette) + 
  scale_shape_manual('', values = c(15, 16, 17, 18))
ggarrange(plot07, plot18, nrow = 2, ncol = 1, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Maps/HiddenLake_maps_PSV.png', width = 800, height = 1000)
plot07 <- ggplot(data = hl_plots, aes(x = lon, y = lat, col = as.factor(ComClass07), fill = as.factor(ComClass07), size = PSV07, shape = ComClass07))+
  geom_point(stroke = 2) + 
  ylab('Lat') + 
  xlab('Lon') +
  ggtitle('2007') + 
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22)) + 
  scale_color_manual('', values = cbbPalette) + 
  scale_shape_manual('', values = c(15, 16, 17, 18))
plot18 <- ggplot(data = hl_plots, aes(x = lon, y = lat, col = as.factor(ComClass18), fill = as.factor(ComClass18), size = PSV18, shape = ComClass18))+
  geom_point(stroke =2) + 
  ylab('Lat') +
  xlab('Lon') +
  ggtitle('2018') +
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22)) + 
  scale_color_manual('', values = cbbPalette) + 
  scale_shape_manual('', values = c(15, 16, 17, 18))
ggarrange(plot07, plot18, nrow = 2, ncol = 1, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Maps/HiddenLake_maps_MNTD.png', width = 800, height = 1000)
plot07 <- ggplot(data = hl_plots, aes(x = lon, y = lat, col = as.factor(ComClass07), fill = as.factor(ComClass07), size = MNTD07, shape = ComClass07))+
  geom_point(stroke = 2) + 
  ylab('Lat') + 
  xlab('Lon') +
  ggtitle('2007') + 
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22)) + 
  scale_color_manual('', values = cbbPalette) + 
  scale_shape_manual('', values = c(15, 16, 17, 18))
plot18 <- ggplot(data = hl_plots, aes(x = lon, y = lat, col = as.factor(ComClass18), fill = as.factor(ComClass18), size = MNTD18, shape = ComClass18))+
  geom_point(stroke =2) + 
  ylab('Lat') +
  xlab('Lon') +
  ggtitle('2018') +
  theme(legend.title = element_blank(), 
        legend.position = 'right',
        plot.title = element_text(face='bold', size = 22)) + 
  scale_color_manual('', values = cbbPalette) + 
  scale_shape_manual('', values = c(15, 16, 17, 18))
ggarrange(plot07, plot18, nrow = 2, ncol = 1, common.legend = T)
dev.off()


#----

# what are the diversity differences between communities???
#-----
png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_SR.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass07), y = as.numeric(SR07), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Species Richness') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass18), y = as.numeric(SR18), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_PD.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass07), y = as.numeric(PD07), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Phylogenetic Diversity') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass18), y = as.numeric(PD18), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_MNTD.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass07), y = as.numeric(MNTD07), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Phylogenetic Beta Diversity') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19))  + 
  scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass18), y = as.numeric(MNTD18), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank())  + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/HiddenLake_PSV.png', width = 800, height = 500)
plot07 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass07), y = as.numeric(PSV07), fill = as.factor(ComClass07)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Phylogenetic Species Variance') + 
  ggtitle('2007') + 
  stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(label = 'p.signif', method = 'anova') + 
  stat_compare_means(label.y = 50) +
  coord_cartesian(ylim = c(0,0.5)) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
    scale_fill_manual(values = cbbPalette)
plot18 <- ggplot(data = hl_plots, aes(x = as.factor(ComClass18), y = as.numeric(PSV18), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('2018') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
ggarrange(plot07, plot18, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/ew_compare_PSV_SR_18.png', width = 800, height = 500)
PSV <- ggplot(data = ew_plots, aes(x = as.factor(ComClass18), y = as.numeric(PSV18), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Phylogenetic Species Variance') + 
  stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(label = 'p.signif', method = 'anova') + 
  stat_compare_means(label.y = 50) +
  coord_cartesian(ylim = c(0,0.5)) + 
  theme(legend.title = element_blank(), 
        legend.position = 'none', 
        plot.title = element_text(face='bold', size = 22), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
SR <- ggplot(data = ew_plots, aes(x = as.factor(ComClass18), y = as.numeric(SR18), fill = as.factor(ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('Species Richness') +
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = 'bold', size = 19)) + 
  scale_fill_manual(values = cbbPalette)
fig <- ggarrange(PSV, SR, ncol = 2, common.legend = T)
annotate_figure(fig, 
                top = text_grob('PSV and SR 2018', size = 25, face = 'bold'))
dev.off()


# png('figures/Summer 2019/EcoStructure/Omega/SRvPSV.png', width = 1000, height = 600)
# plot07 <- ggplot(data = hl_plots, aes(x = SR07, y = PSV07, col = ComClass07, shape = ComClass07)) + 
#   geom_point() + 
#   xlab('Species Richness') + 
#   ylab('Phylogenetic Species Variance') + 
#   ggtitle('2007')+ 
#   theme(aspect.ratio = 1:1, 
#         plot.title = element_text(face = 'bold', size = 22), 
#         legend.position = 'none') + 
#   scale_fill_manual(values = cbbPalette)
# plot18 <- ggplot(data = hl_plots, aes(x = SR18, y = PSV07, col = ComClass18, shape = ComClass18)) + 
#   geom_point() + 
#   xlab('Species Richness') + 
#   ylab('') + 
#   ggtitle('2018')+ 
#   theme(aspect.ratio = 1:1, 
#         plot.title = element_text(face = 'bold', size = 22), 
#         legend.title = element_blank()) + 
#   scale_fill_manual(values = cbbPalette)
# ggarrange(plot07, plot18, nrow = 1, ncol = 2, common.legend = T)
# dev.off()

# png('figures/Summer 2019/EcoStructure/Omega/SRvPSV_groups.png', width = 1000, height = 600)
# group1.07 <- ggplot(data = hl_plots[which(hl_plots$ComClass07 == 'Deep Forest'),], aes(x = SR07, y = PSV07)) + 
#   geom_point(col = '#F8766D', shape = 17) + 
#   xlab('Species Richness') + 
#   ylab('2007 PSV') + 
#   ggtitle('Deep Forest')+ 
#   theme(aspect.ratio = 1:1, 
#         plot.title = element_text(face = 'bold', size = 22), 
#         axis.title.y = element_text(face = 'bold', size = 22)) 
# group2.07 <- ggplot(data = hl_plots[which(hl_plots$ComClass07 == 'Disturbed'),], aes(x = SR18, y = PSV07, col = ComClass07)) + 
#   geom_point(col = '#7CAE00', shape = 18) + 
#   xlab('Species Richness') + 
#   ylab('') + 
#   ggtitle('Disturbed')+ 
#   theme(aspect.ratio = 1:1, 
#         plot.title = element_text(face = 'bold', size = 22)) 
# group3.07 <- ggplot(data = hl_plots[which(hl_plots$ComClass07 == 'Edge'),], aes(x = SR07, y = PSV07, col = ComClass07)) + 
#   geom_point(col = '#00BFC4', shape = 15) + 
#   xlab('Species Richness') + 
#   ylab('') + 
#   ggtitle('Edge')+ 
#   theme(aspect.ratio = 1:1, 
#         plot.title = element_text(face = 'bold', size = 22)) 
# group4.07 <- ggplot(data = hl_plots[which(hl_plots$ComClass07 == 'Mixed'),], aes(x = SR07, y = PSV07, col = ComClass07)) + 
#   geom_point(col = '#C77CFF', shape = 23) + 
#   xlab('Species Richness') + 
#   ylab('') + 
#   ggtitle('Mixed')+ 
#   theme(aspect.ratio = 1:1, 
#         plot.title = element_text(face = 'bold', size = 22))
# 
# group1.18 <- ggplot(data = hl_plots[which(hl_plots$ComClass18 == 'Deep Forest'),], aes(x = SR18, y = PSV18)) + 
#   geom_point(col = '#F8766D', shape = 17) + 
#   xlab('Species Richness') + 
#   ylab('2018 PSV') + 
#   ggtitle('Deep Forest')+ 
#   theme(aspect.ratio = 1:1, 
#         plot.title = element_text(face = 'bold', size = 22), 
#         axis.title.y = element_text(face = 'bold', size = 22))
# group2.18 <- ggplot(data = hl_plots[which(hl_plots$ComClass18 == 'Disturbed'),], aes(x = SR18, y = PSV18, col = ComClass18)) + 
#   geom_point(col = '#7CAE00', shape = 18) + 
#   xlab('Species Richness') + 
#   ylab('') + 
#   ggtitle('Disturbed')+ 
#   theme(aspect.ratio = 1:1, 
#         plot.title = element_text(face = 'bold', size = 22))
# group3.18 <- ggplot(data = hl_plots[which(hl_plots$ComClass18 == 'Edge'),], aes(x = SR18, y = PSV18, col = ComClass18)) + 
#   geom_point(col = '#00BFC4', shape = 15) + 
#   xlab('Species Richness') + 
#   ylab('') + 
#   ggtitle('Edge')+ 
#   theme(aspect.ratio = 1:1, 
#         plot.title = element_text(face = 'bold', size = 22)) 
# group4.18 <- ggplot(data = hl_plots[which(hl_plots$ComClass18 == 'Mixed'),], aes(x = SR18, y = PSV18, col = ComClass18)) + 
#   geom_point(col = '#C77CFF', shape = 23) + 
#   xlab('Species Richness') + 
#   ylab('') + 
#   ggtitle('Mixed')+ 
#   theme(aspect.ratio = 1:1, 
#         plot.title = element_text(face = 'bold', size = 22)) 
# ggarrange(group1.07, group2.07, group3.07, group4.07, group1.18, group2.18, group3.18, group4.18, nrow = 2, ncol = 4, common.legend = T)
# dev.off()


# burn relationship to group 

data <- as.data.frame(table(plots.env$ComClass18[which(plots.env$MgmtUnit == 'Annual Burn')]))
data <- cbind(data, as.data.frame(table(plots.env$ComClass18[which(plots.env$MgmtUnit == 'Mixed Management')])))
data <- cbind(data, as.data.frame(table(plots.env$ComClass18[which(plots.env$MgmtUnit == 'Non-Wooded')])))
data <- cbind(data, as.data.frame(table(plots.env$ComClass18[which(plots.env$MgmtUnit == 'Hidden Lake')])))
data <- cbind(data, as.data.frame(table(plots.env$ComClass18[which(plots.env$MgmtUnit == 'No Management')])))
rownames(data) <- data$Var1
data$Var1 <- NULL
data$Var1 <- NULL
data$Var1 <- NULL
data$Var1 <- NULL
data$Var1 <- NULL
names(data) <- c('Annual Burn', 'Mixed Management', 'Non-Wooded', 'Hidden Lake', 'No Management')

#Transform this data in %
data_percentage=as.data.frame(apply(data, 2, function(x){x*100/sum(x,na.rm=T)}))

# cant beleive this but Im an idiot and cant format a table
group <- c(rep('Annual Burn', 4), rep('Mixed Management', 4), rep('Non-Wooded', 4), rep('Hidden Lake', 4), rep('No Manangement', 4))
value <- c(data_percentage$`Annual Burn`, data_percentage$`Mixed Management`, data_percentage$`Non-Wooded`, data_percentage$`Hidden Lake`, data_percentage$`No Management`)
clust <- c(rep(c('Deep Woods', 'Disturbed', 'Edge', 'Mixed'), 5))
data <- data.frame(group, value, clust)

# Make a stacked barplot--> it will be in %!
png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/2018/management_effects_18.png', width = 700, height = 600)
ggplot(data, aes(fill=clust, y=value, x=group)) + 
  geom_bar(stat="identity", position="fill") + 
  scale_fill_manual(values = cbbPalette) + 
  ylab('% Plots') + 
  ggtitle('Management in Community Classes') + 
  theme(plot.title = element_text(size = 20, face = 'bold'), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1), 
        legend.title = element_blank())
dev.off()

plots.env$MgmtUnit[which(is.na(plots.env$MgmtUnit))] <- 'No Management'
png('figures/Summer 2019/Maps/Envt_data/mgmt_unit.png', width = 800, height = 600)
ggplot(plots.env, aes(x = lon , y = lat, col = MgmtUnit))+ 
  geom_point(shape = 19, size = 3) + 
  coord_equal() + 
  ggtitle('Management Areas East Woods') + 
  theme(plot.title = element_text(face = 'bold', size = 27)) + 
  scale_color_manual(values = cbbPalette)
dev.off()

png('figures/Summer 2019/Maps/Envt_data/burncount_comclass.png', width = 800, height = 600)
ggplot(plots.env, aes(x = lon , y = lat, col = ComClass18, size = BurnCount))+ 
  geom_point(shape = 19) + 
  coord_equal() + 
  ggtitle('Burn Count and Community Class 2018') + 
  theme(plot.title = element_text(face = 'bold', size = 27)) + 
  scale_color_manual(values = cbbPalette)
dev.off()

plots.env$BurnCount07[which(is.na(plots.env$BurnCount07))] <- 1
plots.env$BurnCount07 <- plots.env$BurnCount07 + 1
png('figures/Summer 2019/Maps/Envt_data/burncount07_comclass.png', width = 800, height = 600)
ggplot(plots.env, aes(x = lon , y = lat, col = ComClass18, size = BurnCount07))+ 
  geom_point(shape = 19) + 
  coord_equal() + 
  ggtitle('Burn Count and Community Class 2007') + 
  theme(plot.title = element_text(face = 'bold', size = 27)) + 
  scale_color_manual(values = cbbPalette)
dev.off()


