

library(vegan)
library(ggplot2)
library(ggpubr)
library(dbplyr)
library(vegan)
library(gtools)
theme_set(theme_classic())
source('scripts/05.community_matrix_function.R')

data("plots.env")
dat.all <- read.csv("data/Species/dat.all.csv")
dat.mat.all.07 <- read.csv('data/Community_Matrix/2007/dat.mat.all.07.csv', row.names = 1)
dat.mat.all.18 <- read.csv('data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)
tr.ew <- read.tree('data/Phylogeny/tr.ew.Spring19')
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# combine the two community matrices for comparing between the years 
dat.all$plot <- paste(dat.all$plot, '_', dat.all$year, sep = '')

ord_outliers <- c('R79_2018', 'LL123_2018')
dat.mat.all <- com_mat(dat.all, tr.ew)
dat.mat.all <- na.replace(dat.mat.all, 0)
dat.mat.all <- dat.mat.all[which(rowSums(dat.mat.all) > 3),]
dat.mat.all <- dat.mat.all[which(!rownames(dat.mat.all) %in% ord_outliers),]
ordall <- metaMDS(dat.mat.all)

ord.df <- data.frame(ordall$points)
ord.df$year <- gsub('.*_', '', rownames(ord.df))
ord.df$ComClass <- ifelse(ord.df$year == '2007', plots.env$ComClass07,
                          plots.env$ComClass18)

png('figures/Summer 2019/Ordination/2007_AND_2018.png', width = 1000, height = 800)
ggplot(data = ord.df, aes(x = MDS1, y = MDS2, col = year))+ 
  geom_point(shape = 21, size = 1.5) + 
#  geom_text(aes(label = rownames(ord.df))) +
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('2007 and 2018') + 
  coord_equal() +
  theme(title = element_text(face = 'bold', size = 22), 
        legend.key.size = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
dev.off()


png('figures/Summer 2019/Ordination/2007_AND_2018_comclasses.png', width = 2000, height = 600)
p1 <- ggplot(data = ord.df[which(ord.df$ComClass == 'Deep Forest'),], aes(x = MDS1, y = MDS2, col = year))+ 
  geom_point(shape = 21, size = 1.5) + 
  #  geom_text(aes(label = rownames(ord.df))) +
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('Deep Forest') + 
  coord_equal() +
  theme(aspect.ratio = 1:1, 
        title = element_text(face = 'bold', size = 22), 
        legend.key.size = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
p2 <- ggplot(data = ord.df[which(ord.df$ComClass == 'Edge'),], aes(x = MDS1, y = MDS2, col = year))+ 
  geom_point(shape = 21, size = 1.5) + 
  #  geom_text(aes(label = rownames(ord.df))) +
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('Edge') + 
  coord_equal() +
  theme(aspect.ratio = 1:1, 
        title = element_text(face = 'bold', size = 22), 
        legend.key.size = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
p3 <- ggplot(data = ord.df[which(ord.df$ComClass == 'Disturbed'),], aes(x = MDS1, y = MDS2, col = year))+ 
  geom_point(shape = 21, size = 1.5) + 
  #  geom_text(aes(label = rownames(ord.df))) +
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('Disturbed') + 
  coord_equal() +
  theme(aspect.ratio = 1:1,
        title = element_text(face = 'bold', size = 22), 
        legend.key.size = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
p4 <- ggplot(data = ord.df[which(ord.df$ComClass == 'Mixed'),], aes(x = MDS1, y = MDS2, col = year))+ 
  geom_point(shape = 21, size = 1.5) + 
  #  geom_text(aes(label = rownames(ord.df))) +
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('Mixed') + 
  coord_equal() +
  theme(aspect.ratio = 1:1, 
        title = element_text(face = 'bold', size = 22), 
        legend.key.size = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
ggarrange(p1, p2, p3, p4, ncol = 4, common.legend = T)
dev.off()

#-----
# what I'd like to do now is just explore some what is going on with the changing plots 
# so this will be looking at the plots that belong to different communities between 2007 and 2018

plots.env$ChangeGroup1 <- plots.env$Group1.Mem18 - plots.env$Group1.Mem07
plots.env$ChangeGroup2 <- plots.env$Group2.Mem18 - plots.env$Group2.Mem07
plots.env$ChangeGroup3 <- plots.env$Group3.Mem18 - plots.env$Group3.Mem07

plots.env$OverallChange <- abs(plots.env$ChangeGroup1) + abs(plots.env$ChangeGroup2) + abs(plots.env$ChangeGroup3)


deep_forest <- plots.env[which(plots.env$ComClass07 == 'Deep Forest' | plots.env$ComClass18 == 'Deep Forest'),]
edge <- plots.env[which(plots.env$ComClass07 == 'Edge' | plots.env$ComClass18 == 'Edge'),]
disturbed <- plots.env[which(plots.env$ComClass07 == 'Disturbed' | plots.env$ComClass18 == 'Disturbed'),]
mixed <- plots.env[which(plots.env$ComClass07 == 'Mixed' | plots.env$ComClass18 == 'Mixed'),]

png('figures/Summer 2019/EcoStructure/Omega/Maps/deep_forest_changes2.png', height = 500, width = 800)
p1 <- ggplot(data = deep_forest, aes(x = lon, y = lat, col = ComClass07, fill = ComClass07,  size = ChangeGroup1))+ 
  geom_point(shape = 21, stroke = 2, size = 3)+ 
  coord_equal() + 
  ggtitle('Impact by 2007 Membership') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 18))
p2 <- ggplot(data = deep_forest, aes(x = lon, y = lat, col = ComClass18, fill = ComClass18, size = ChangeGroup1))+ 
  geom_point(shape = 21)+ 
  coord_equal() + 
  ggtitle('Effect on 2018 Membership') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 18))
fig <- ggarrange(p1, p2, ncol = 2, common.legend = T)
annotate_figure(fig, 
                top = text_grob('Deep Forest Community Changes', size = 22, face = 'bold'))
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Maps/edge_changes2.png', height = 500, width = 800)
p1 <- ggplot(data = edge, aes(x = lon, y = lat, col = ComClass07, fill = ComClass07, size = ChangeGroup2))+ 
  geom_point(shape = 21)+ 
  coord_equal() + 
  ggtitle('Impact by 2007 Membership') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 18))
p2 <- ggplot(data = edge, aes(x = lon, y = lat, col = ComClass18, fill = ComClass18, size = ChangeGroup2))+ 
  geom_point(shape = 21)+ 
  coord_equal() + 
  ggtitle('Effect on 2018 Membership') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 18))
fig <- ggarrange(p1, p2, ncol = 2, common.legend = T)
annotate_figure(fig, 
                top = text_grob('Edge Community Changes', size = 22, face = 'bold'))
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Maps/disturbed_changes2.png', height = 500, width = 800)
p1 <- ggplot(data = disturbed, aes(x = lon, y = lat, col = ComClass07, fill = ComClass07, size = ChangeGroup3))+ 
  geom_point(shape = 21)+ 
  coord_equal() + 
  ggtitle('Impact by 2007 Membership') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 18))
p2 <- ggplot(data = disturbed, aes(x = lon, y = lat, col = ComClass18, fill = ComClass18, size = ChangeGroup3))+ 
  geom_point(shape = 21)+ 
  coord_equal() + 
  ggtitle('Effect on 2018 Membership') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 18))
fig <- ggarrange(p1, p2, ncol = 2, common.legend = T)
annotate_figure(fig, 
                top = text_grob('Disturbed Community Changes', size = 22, face = 'bold'))
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Maps/mixed_changes2.png', height = 500, width = 800)
p1 <- ggplot(data = mixed, aes(x = lon, y = lat, col = ComClass07, fill = ComClass07, size = ChangeGroup3))+ 
  geom_point(shape = 21)+ 
  coord_equal() + 
  ggtitle('Impact by 2007 Membership') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 18))
p2 <- ggplot(data = mixed, aes(x = lon, y = lat, col = ComClass18, fill = ComClass18, size = ChangeGroup3))+ 
  geom_point(shape = 21)+ 
  coord_equal() + 
  ggtitle('Effect on 2018 Membership') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 18))
fig <- ggarrange(p1, p2, ncol = 2, common.legend = T)
annotate_figure(fig, 
                top = text_grob('Mixed Community Changes', size = 22, face = 'bold'))
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/Maps/com_changes.png', height = 1002, width = 1500)
p1 <- ggplot(data = deep_forest, aes(x = lon, y = lat, col = ComClass07, fill = ComClass18, size = ChangeGroup1))+ 
  geom_point(shape = 21, stroke = 2)+ 
  coord_equal() + 
  ggtitle('Deep Forest') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 17, hjust = 0.5), 
        legend.position = 'none')

p2 <- ggplot(data = edge, aes(x = lon, y = lat, col = ComClass07, fill = ComClass18, size = ChangeGroup2))+ 
  geom_point(shape = 21, stroke = 2)+ 
  coord_equal() + 
  ggtitle('Edge') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 17, hjust = 0.5), 
        legend.position = 'none')

p3 <- ggplot(data = disturbed, aes(x = lon, y = lat, col = ComClass07, fill = ComClass18, size = ChangeGroup3))+ 
  geom_point(shape = 21, stroke= 2)+ 
  coord_equal() + 
  ggtitle('Disturbed') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 17, hjust = 0.5), 
        legend.position = 'none')

p4 <- ggplot(data = mixed, aes(x = lon, y = lat, col = ComClass07, fill = ComClass18, size = OverallChange))+ 
  geom_point(shape = 21, stroke = 2)+ 
  coord_equal() + 
  ggtitle('Mixed') + 
  scale_fill_manual(values=cbbPalette) + 
  scale_color_manual(values = cbbPalette) + 
  theme(plot.title = element_text(face = 'bold', size = 17, hjust = 0.5), 
        legend.position = 'none')

fig <- ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = F)
annotate_figure(fig, 
                top = text_grob('Changes to Community Classes', face = 'bold', size = 30))
dev.off()

save(plots.env, file = 'data/plots.env.RData')
write.csv(plots.env, 'data/plots.env.csv')




