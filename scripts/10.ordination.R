
library(picante)
library(vegan)
library(RColorBrewer)
library(readxl)
library(ggplot2)
library(ggpubr)
theme_set(theme_classic()) 
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data("plots.env")
dat.18 <- read.csv('~/Documents/GitHub/east_woods_work/data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)
dat.07 <- read.csv('~/Documents/GitHub/east_woods_work/data/Community_Matrix/2007/dat.mat.all.07.csv', row.names = 1)

# outlier plots
outlier_plots_07 <- c('T147', 'OO122', 'PP122', 'WW119', 'X138', 'RR116', 'LL123', 'AX132', 'P138')
outlier_plots_18 <- c('LL123', 'R79')

dat.07 <- dat.07[which(!rownames(dat.07) %in% outlier_plots_07),]
dat.18 <- dat.18[which(!rownames(dat.18) %in% outlier_plots_18),]

# only looking at plots with more than 3 species
dat.18 <- dat.18[which(rowSums(dat.18) > 3),]
dat.07 <- dat.07[which(rowSums(dat.07) > 3),]

# want to be able to consider EW and HL separately 
ew_plots <- plots.env[which(plots.env$wooded== 'East Woods'),]
hl_plots <- plots.env[which(plots.env$wooded == 'Hidden Lake'),]
dat_ew <- dat.18[rownames(ew_plots),]
dat_hl <- dat.18[rownames(hl_plots),]
dw_plots <- plots.env[which(plots.env$ComClass18 == 'Deep Forest'),]
ed_plots <- plots.env[which(plots.env$ComClass18 == 'Edge'),]
ds_plots <- plots.env[which(plots.env$ComClass18 == 'Disturbed'),]
dat_dw <- dat.18[rownames(dw_plots),]
dat_ed <- dat.18[rownames(ed_plots),]
dat_ds <- dat.18[rownames(ds_plots),]

dat_hl <- na.omit(dat_hl)

ord18 <- metaMDS(dat.18)
ord07 <- metaMDS(dat.07)
ord_ew <- metaMDS(dat_ew)
ord_hl <- metaMDS(dat_hl)

dat_ed <- na.omit(dat_ed)
ord_dw <- metaMDS(dat_dw)
ord_ed <- metaMDS(dat_ed)
ord_ds <- metaMDS(dat_ds)

ord18.df <- data.frame(ord18$points)
ord18.df$TreeCover07 <- plots.env$TreeCover07[match(rownames(ord18.df), rownames(plots.env))]
ord18.df$TreeCover18 <- plots.env$TreeCover18[match(rownames(ord18.df), rownames(plots.env))]
ord18.df$ComClass07 <- plots.env$ComClass07[match(rownames(ord18.df), rownames(plots.env))]
ord18.df$ComClass18 <- plots.env$ComClass18[match(rownames(ord18.df), rownames(plots.env))]
ord18.df$EWorHL <- plots.env$wooded[match(rownames(ord18.df), rownames(plots.env))]

ord07.df <- data.frame(ord07$points)
ord07.df$TreeCover07 <- plots.env$TreeCover07[match(rownames(ord07.df), rownames(plots.env))]
ord07.df$TreeCover07 <- plots.env$TreeCover07[match(rownames(ord07.df), rownames(plots.env))]
ord07.df$ComClass07 <- plots.env$ComClass07[match(rownames(ord07.df), rownames(plots.env))]
ord07.df$ComClass18 <- plots.env$ComClass18[match(rownames(ord07.df), rownames(plots.env))]
ord07.df$EWorHL <- plots.env$wooded[match(rownames(ord07.df), rownames(plots.env))]

png('figures/Summer 2019/Ordination/all/ordination_w_ew_vs_hl.png', height = 800, width = 1500)
p1 <- ggplot(data = ord18.df, aes(x = MDS1, y = MDS2))+ 
  geom_point(shape = 21) + 
  stat_ellipse(aes(x= MDS1, y = MDS2, col = EWorHL)) + 
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('2018') + 
  coord_equal() +
  theme(title = element_text(face = 'bold', size = 22), 
        legend.title = element_blank(), 
        legend.key.size = unit(2, 'cm'), 
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
p2 <- ggplot(data = ord07.df, aes(x = MDS1, y = MDS2))+ 
  geom_point(shape = 21) + 
  stat_ellipse(aes(x= MDS1, y = MDS2, col = EWorHL)) + 
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('2007') + 
  coord_equal() + 
  theme(title = element_text(face = 'bold', size = 22), 
        legend.title = element_blank(), 
        legend.key.size = unit(2, 'cm'))
ggarrange(p1, p2, ncol = 2, common.legend = T)
dev.off()

png('figures/Summer 2019/Ordination/all/ordination_w_ecostructure_community.png', height = 800, width = 1500)
p1 <- ggplot(data = ord18.df, aes(x = MDS1, y = MDS2, col = ComClass18, fill = ComClass07))+ 
  geom_point(shape = 21, stroke = 2) + 
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('2018') + 
  #  geom_text(aes(label = rownames(ord18.df))) +
  coord_equal() +
  theme(title = element_text(face = 'bold', size = 22), 
        legend.key.size = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
p2 <- ggplot(data = ord07.df, aes(x = MDS1, y = MDS2, col = ord07.df$ComClass18, fill = ord07.df$ComClass07))+ 
  geom_point(shape = 21, stroke = 2) + 
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('2007') + 
  #  geom_text(aes(label = rownames(ord07.df))) +
  coord_equal() + 
  theme(title = element_text(face = 'bold', size = 22))
ggarrange(p1, p2, ncol = 2, common.legend = T)
dev.off()


ew.df <- data.frame(ord_ew$points)
ew.df$TreeCover07 <- plots.env$TreeCover07[match(rownames(ew.df), rownames(plots.env))]
ew.df$TreeCover18 <- plots.env$TreeCover18[match(rownames(ew.df), rownames(plots.env))]
ew.df$ComClass07 <- plots.env$ComClass07[match(rownames(ew.df), rownames(plots.env))]
ew.df$ComClass18 <- plots.env$ComClass18[match(rownames(ew.df), rownames(plots.env))]
ew.df$EWorHL <- plots.env$wooded[match(rownames(ew.df), rownames(plots.env))]

hl.df <- data.frame(ord_hl$points)
hl.df$TreeCover07 <- plots.env$TreeCover07[match(rownames(hl.df), rownames(plots.env))]
hl.df$TreeCover07 <- plots.env$TreeCover07[match(rownames(hl.df), rownames(plots.env))]
hl.df$ComClass07 <- plots.env$ComClass07[match(rownames(hl.df), rownames(plots.env))]
hl.df$ComClass18 <- plots.env$ComClass18[match(rownames(hl.df), rownames(plots.env))]
hl.df$EWorHL <- plots.env$wooded[match(rownames(hl.df), rownames(plots.env))]


png('figures/Summer 2019/Ordination/all/ordination_ew_v_hl.png', height = 800, width = 1500)
p1 <- ggplot(data = ew.df, aes(x = MDS1, y = MDS2, col = ComClass18, fill = ComClass07, size = ))+ 
  geom_point(shape = 21, stroke = 2) + 
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('East Woods') + 
  #  geom_text(aes(label = rownames(ord18.df))) +
  coord_equal() +
  theme(title = element_text(face = 'bold', size = 22), 
        legend.key.size = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
p2 <- ggplot(data = hl.df, aes(x = MDS1, y = MDS2, col = ComClass18, fill = ComClass07))+ 
  geom_point(shape = 21, stroke = 2) + 
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('Hidden Lake') + 
  #  geom_text(aes(label = rownames(ord07.df))) +
  coord_equal() + 
  theme(title = element_text(face = 'bold', size = 22))
ggarrange(p1, p2, ncol = 2, common.legend = T)
dev.off()


ord.dw.df <- as.data.frame(ord_dw$points)
ord.dw.df$unit <- plots.env$unit[match(rownames(ord.dw.df), rownames(plots.env))]
ord.dw.df$arb_area <- plots.env$arb_area[match(rownames(ord.dw.df), rownames(plots.env))]
ord.dw.df$elev <- plots.env$elev[match(rownames(ord.dw.df), rownames(plots.env))]
ord.dw.df$MgmtUnit <- plots.env$MgmtUnit[match(rownames(ord.dw.df), rownames(plots.env))]

png('figures/Summer 2019/Ordination/dw_ord.png', height = 600, width = 800)
ggplot(data = ord.dw.df, aes(x = MDS1, y = MDS2, col = MgmtUnit, size = elev))+ 
  # stat_ellipse(aes(x= MDS1, y = MDS2, col = MgmtUnit)) + 
  # scale_fill_manual(values = cbbPalette) + 
  # scale_color_manual(values = cbbPalette) +
  ggtitle('Deep Woods') + 
  geom_text(aes(label = rownames(ord_dw$points))) +
  coord_equal() +
  theme(title = element_text(face = 'bold', size = 22), 
        legend.key.size = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
dev.off()



ord.ed.df <- as.data.frame(ord_ed$points)
ord.ed.df$unit <- plots.env$unit[match(rownames(ord.ed.df), rownames(plots.env))]
ord.ed.df$arb_area <- plots.env$arb_area[match(rownames(ord.ed.df), rownames(plots.env))]
ord.ed.df$elev <- plots.env$elev[match(rownames(ord.ed.df), rownames(plots.env))]
ord.ed.df$MgmtUnit <- plots.env$MgmtUnit[match(rownames(ord.ed.df), rownames(plots.env))]

png('figures/Summer 2019/Ordination/ed_ord.png', height = 700, width = 800)
ggplot(data = ord.ed.df, aes(x = MDS1, y = MDS2, col = unit, size = elev))+ 
  ggtitle('Edge') + 
  geom_text(aes(label = rownames(ord.ed.df))) +
  coord_equal() +
  theme(title = element_text(face = 'bold', size = 22), 
        legend.key.size = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
dev.off()

ord.ds.df <- as.data.frame(ord_ds$points)
ord.ds.df$unit <- plots.env$unit[match(rownames(ord.ds.df), rownames(plots.env))]
ord.ds.df$arb_area <- plots.env$arb_area[match(rownames(ord.ds.df), rownames(plots.env))]
ord.ds.df$elev <- plots.env$elev[match(rownames(ord.ds.df), rownames(plots.env))]
ord.ds.df$MgmtUnit <- plots.env$MgmtUnit[match(rownames(ord.ds.df), rownames(plots.env))]

png('figures/Summer 2019/Ordination/ds_ord.png', height = 700, width = 800)
ggplot(data = ord.ds.df, aes(x = MDS1, y = MDS2, col = MgmtUnit, size = elev))+ 
  ggtitle('Disturbed') + 
  geom_text(aes(label = rownames(ord.ds.df))) +
  coord_equal() +
  theme(title = element_text(face = 'bold', size = 22), 
        legend.key.size = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'),
        legend.text = element_text(face = 'bold', size = 15), 
        legend.key.height = unit(2, 'cm'))
dev.off()



ord18.df$EdgeWoods <- ifelse(ord18.df$EWorHL == 'East Woods', 'Woods', 
                             ifelse(ord18.df$EWorHL == 'Hidden Lake', 'Woods', 'Edge'))
ord18.df$EdgeWoods[which(is.na(ord18.df$EdgeWoods))] <- 'Edge'
ord07.df$EdgeWoods <- ifelse(ord07.df$EWorHL == 'East Woods', 'Woods', 
                             ifelse(ord07.df$EWorHL == 'Hidden Lake', 'Woods', 'Edge'))
ord07.df$EdgeWoods[which(is.na(ord07.df$EdgeWoods))] <- 'Edge'
# Compare edge and woods separation between 2007 and 2018 
png('figures/Summer 2019/Ordination/all/separation_bt_edge_woods18.png', height = 800, width = 800)
ggplot(data = ord18.df, aes(x = MDS1, y = MDS2, col = ComClass18, size = TreeCover18))+ 
  stat_ellipse(geom = 'polygon', alpha = 0.1, aes(x= MDS1, y = MDS2, col = EdgeWoods, fill = EdgeWoods)) + 
  geom_point(shape = 21) + 
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('2018') + 
  coord_equal() +
  theme(title = element_text(face = 'bold', size = 22))
dev.off()
png('figures/Summer 2019/Ordination/all/separation_bt_edge_woods07.png', height = 800, width = 800)
ggplot(data = ord07.df, aes(x = MDS1, y = MDS2, col = ComClass07, size = TreeCover07))+ 
  stat_ellipse(geom = 'polygon', alpha = 0.1, aes(x = MDS1, y = MDS2, col = EdgeWoods, fill = EdgeWoods)) + 
  geom_point(shape = 21) + 
  scale_fill_manual(values = cbbPalette) + 
  scale_color_manual(values = cbbPalette) +
  ggtitle('2007') + 
  coord_equal() + 
  theme(title = element_text(face = 'bold', size = 22), 
        legend.title = element_blank(), 
        legend.text = element_blank())
dev.off()
ggarrange(p1, p2, ncol = 2, common.legend = T)
dev.off()

pb1 = ggplot_build(p1)
el1 = pb1$data[[2]][c('x', 'y')]
ctr1 = MASS::cov.trob(el)$center
dist2center1 <- sqrt(rowSums((t(t(el1)-ctr1))^2))
area1 <- pi*min(dist2center1)*max(dist2center1)

pb2 = ggplot_build(p2)
el2 = pb2$data[[2]][c('x', 'y')]
ctr2 = MASS::cov.trob(el)$center
dist2center2 <- sqrt(rowSums((t(t(el2)-ctr2))^2))
area2 <- pi*min(dist2center2)*max(dist2center2)

overlap.x = intersect(el1$x, el2$x)
ggplot(data = el, aes(x = x, y = y))+ 
  geom_encircle()



