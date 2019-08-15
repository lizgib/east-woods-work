# script to assess: 
# 1. are there species that are indicators of whether a plot will change
# 2. is there an environmental factor that predicts change? 

library(ggplot2)
library(ggpubr)
library(ggtree)
theme_set(theme_minimal())
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# correlation of presence of each species and change in a plot community 

data('plots.env')
dat.mat.all.18 <- read.csv('data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)
dat.mat.all.07 <- read.csv('data/Community_Matrix/2007/dat.mat.all.07.csv', row.names = 1)
tree.file <- read.tree('outputs/tr.ew.Spring19')


# how does the frequency of each species in 07 plots predict the community of that plot in 2018

# what is the correlation between each species in 2007 and the community in 2018? 
cor_07_group1 <- cor(dat.mat.all.07, plots.env$Group1.Mem18, use = 'complete.obs')
cor_07_group2 <- cor(dat.mat.all.07, plots.env$Group2.Mem18, use = 'complete.obs')
cor_07_group3 <- cor(dat.mat.all.07, plots.env$Group3.Mem18, use = 'complete.obs')

all_cor <- cbind(cor_07_group1, cor_07_group2, cor_07_group3)
all_cor <- as.data.frame(all_cor) 
names(all_cor) <- c('Deep Forest', 'Edge', 'Disturbed')


# tree <- ggtree(tree.file)
# tree <- tree + geom_tiplab(size = 0.5)
# tiff('figures/Summer 2019/EcoStructure/Omega/tree_heatmap_07_grup18.tiff', units = 'in', height = 10, width = 12, res = 400)
# gheatmap(tree, as.data.frame(cor_07_group3), width = 0.25,
#          colnames_position = 'bottom',
#          low = "white", high = "darkcyan", color = "white",
#          colnames_angle = 270, 
#          font.size = 3, hjust = 0)
# dev.off()
# 
# deepforest <- all_cor[order(all_cor$`Deep Forest`, decreasing = T),]
# edge <- all_cor[order(all_cor$Edge, decreasing = T),]
# disturbed <- all_cor[order(all_cor$Disturbed, decreasing = T),]

# png('figures/Summer 2019/EcoStructure/Omega/corr_07_grup_18.png', width = 1000, height = 2000)
# 
# p1 <- ggplot(data = deepforest[1:10,], aes(x = rownames(deepforest)[1:10], y = deepforest$`Deep Forest`[1:10])) + 
#   geom_bar(stat = 'identity') + 
#   ylab('Correlation to Group 18') + 
#   ggtitle("Deep Forest") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15, face = 'bold'),
#         axis.title.x = element_blank(), 
#         axis.title.y = element_text(size = 22, face = 'bold'))
# 
# p2 <- ggplot(data = edge[1:10,], aes(x = rownames(edge)[1:10], y = edge$Edge[1:10])) + 
#   geom_bar(stat = 'identity') + 
#   ylab('Correlation to Group 18') + 
#   ggtitle('Edge') + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15, face = 'bold'),
#         axis.title.x = element_blank(), 
#         axis.title.y = element_text(size = 22, face = 'bold'))
# 
# p3 <- ggplot(data = disturbed[1:10,], aes(x = rownames(disturbed)[1:10], y = disturbed$Disturbed[1:10])) + 
#   geom_bar(stat = 'identity') + 
#   ylab('Correlation to Group 18') + 
#   ggtitle('Disturbed') + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15, face = 'bold'),
#         axis.title.x = element_blank(), 
#         axis.title.y = element_text(size = 22, face = 'bold'))
# ggarrange(p1, p2, p3, nrow = 3)
# dev.off()

#gzoom(tree.file, grep("Solidago", tree.file$tip.label))


# which plots changed the most and what species are in them? 

plots.env$Group1.Change <- plots.env$Group1.Mem18 - plots.env$Group1.Mem07
plots.env$Group2.Change <- plots.env$Group2.Mem18 - plots.env$Group3.Mem07
plots.env$Group3.Change <- plots.env$Group3.Mem18 - plots.env$Group2.Mem07

plots.env$OverallChange <- abs(plots.env$Group1.Change) + abs(plots.env$Group2.Change) + abs(plots.env$Group3.Change)

ggplot(plots.env, aes(x = lon, y = lat, col = OverallChange, size = OverallChange))+ 
  geom_point()+
  coord_equal()

# ------ 

# what is the average change for each species 

# overall change for each plot 
plots <- plots.env['OverallChange']

# which species occur in that plot? 


rank_spp_change <- function(dat.mat, plots){
  species <- c()
  values <- c()
  for (sp in colnames(dat.mat)){
    plt_vect = c()
    for (p in rownames(dat.mat)){
      if(dat.mat[p, sp] == 1){
        plt_vect <- c(plt_vect, plots$OverallChange[which(rownames(plots) == p)])
      }
    }
    values <- c(values, mean(plt_vect))
    species <- c(species, sp)
  }
  
  species_plots <- as.data.frame(species)
  species_plots$value <- values
  return(species_plots)
}

species_plots_18 <- rank_spp_change(dat.mat.all.18, plots)
species_plots_07 <-rank_spp_change(dat.mat.all.07, plots)

# clean up the community matrix: get rid of plots with less than 3 species 

dat.mat.18.clean <- dat.mat.all.18[which(colSums(dat.mat.all.18) > 3),] # tried getting rid of species that only appear 3 or less times 
dat.mat.07.clean <- dat.mat.all.07[which(colSums(dat.mat.all.07) > 3),]
dat.mat.18.clean <- dat.mat.18.clean[which(rowSums(dat.mat.18.clean) > 3),] # also got rid of plots with 3 or fewer species 
dat.mat.07.clean <- dat.mat.07.clean[which(rowSums(dat.mat.07.clean) > 3),]

clean_species_plots_18 <- rank_spp_change(dat.mat.18.clean, plots)
clean_species_plots_07 <- rank_spp_change(dat.mat.07.clean, plots)

plots.env$ChangePSV <- plots.env$PSV07 - plots.env$PSV18
plots.env$ChangeSR <- plots.env$SR07 - plots.env$SR18

png('figures/Summer 2019/Maps/plot_changes.png', width = 1000, height = 400)
p1 <- ggplot(data = plots.env, aes(x = lon, y = lat, size = OverallChange, col = OverallChange))+ 
  geom_point() + 
  coord_equal() + 
  ggtitle('Overall Plot Change')

p2 <- ggplot(data = plots.env, aes(x = lon, y = lat, size = ChangeSR, col = ChangeSR))+ 
  geom_point() + 
  coord_equal() + 
  ggtitle('SR Plot Change')

p3 <- ggplot(data = plots.env, aes(x = lon, y = lat, size = ChangePSV, col = ChangePSV))+ 
  geom_point() + 
  coord_equal() + 
  ggtitle('PSV Plot Change')
ggarrange(p1, p2, p3, ncol = 3, common.legend = T)
dev.off()


png('figures/Summer 2019/Maps/change_edge.png', width = 800, height = 600)
ggplot(data = plots.env[which(plots.env$ComClass07 != plots.env$ComClass18),], aes(x = lon, y = lat, size = Group1.Change, col = ComClass07, fill = ComClass18))+ 
  geom_point(shape = 21, stroke = 2) + 
  coord_equal() + 
  scale_color_manual(values = cbbPalette) + 
  scale_fill_manual(values = cbbPalette) +
  ggtitle('Change in Deep Forest')
dev.off()

png('figures/Summer 2019/Maps/change_disturbed.png', width = 800, height = 600)
ggplot(data = plots.env[which(plots.env$ComClass07 != plots.env$ComClass18),], aes(x = lon, y = lat, size = Group3.Change, col = ComClass07, fill = ComClass18))+ 
  geom_point(shape = 21, stroke = 2) + 
  coord_equal() + 
  scale_color_manual(values = cbbPalette) + 
  scale_fill_manual(values = cbbPalette) +
  ggtitle('Change in Disturbed')
dev.off()

png('figures/Summer 2019/Maps/change_edge.png', width = 800, height = 600)
ggplot(data = plots.env[which(plots.env$ComClass07 != plots.env$ComClass18),], aes(x = lon, y = lat, size = Group2.Change, col = ComClass07, fill = ComClass18))+ 
  geom_point(shape = 21, stroke = 2) + 
  coord_equal() + 
  scale_color_manual(values = cbbPalette) + 
  scale_fill_manual(values = cbbPalette) +
  ggtitle('Change in Edge')
dev.off()



# ok based on this lets look at something else: 
# in these plots which became more deep forest, which species did they gain? which species did they lose? 
data('ecostructure_groups')
dat.all <- read.csv('data/species/dat.all.csv')

# ----- 
# deep forest 
# take all the plots which are deep forest in either 2007 or 2018

species_change <- function(comname, group07, group18, dat.all, plots.env, colval, fname_lost, fname_new){
  dat.community <- dat.all[which(dat.all$plot %in% group18 | dat.all$plot %in% group07),]
  
  # which plots became that community class
  new_plots <- rownames(plots.env)[which(plots.env$ComClass18 == comname & plots.env$ComClass07 != comname)]
  new_plots <- dat.community[which(dat.community$plot %in% new_plots),]
  # which plots switched from that community class to another 
  lost_plots <- rownames(plots.env)[which(plots.env$ComClass18 != comname & plots.env$ComClass07 == comname)]
  lost_plots <- dat.community[which(dat.community$plot %in% lost_plots),]
  
  lost_spp = list()
  new_spp = list()
  for (plt in lost_plots$plot){
    spp07 <- lost_plots$accepted_name[which(lost_plots$plot == plt & lost_plots$year == '2007')]
    spp18 <- lost_plots$accepted_name[which(lost_plots$plot == plt & lost_plots$year == '2018')]
    lost_spp[[plt]] = setdiff(spp07, spp18)
    new_spp[[plt]] = setdiff(spp18, spp07)
  }
  
  lost_spp <- as.data.frame(table(unlist(lost_spp)))
  lost_spp <- lost_spp[which(lost_spp$Freq > 3),]
  new_spp <- as.data.frame(table(unlist(new_spp)))
  new_spp <- new_spp[which(new_spp$Freq > 3),]
  
  lost_spp <- lost_spp[order(lost_spp$Freq, decreasing = T),]
  new_spp <- new_spp[order(new_spp$Freq, decreasing = T),]
  title = paste('Old ', comname, ' Plot Species Changes', sep = '')
  png(fname_lost, width = 1000, height = 1000)
  p1 <- ggplot(lost_spp, aes(x = as.factor(lost_spp$Var1), y = lost_spp$Freq)) + 
    geom_bar(aes(x = reorder(Var1, -Freq), y = Freq), stat = 'identity', fill = colval) + 
    ylab('Number Plots Lost') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15, face = 'bold'),
          axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 22, face = 'bold'))
  
  p2 <- ggplot(new_spp, aes(x = as.factor(new_spp$Var1), y = new_spp$Freq)) + 
    geom_bar(aes(x = reorder(Var1, -Freq), y = Freq), stat = 'identity', fill = colval) + 
    ylab('Number Plots Gained') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15, face = 'bold'),
          axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 22, face = 'bold'))
  figure1 <- ggarrange(p1, p2, nrow = 2)
  annotate_figure(figure1, 
                  top = text_grob(title, face = 'bold', size = 27))
  ggsave(filename = fname_lost)
  dev.off()
  
  lost_spp = list()
  new_spp = list()
  for (plt in new_plots$plot){
    spp07 <- new_plots$accepted_name[which(new_plots$plot == plt & new_plots$year == '2007')]
    spp18 <- new_plots$accepted_name[which(new_plots$plot == plt & new_plots$year == '2018')]
    lost_spp[[plt]] = setdiff(spp07, spp18)
    new_spp[[plt]] = setdiff(spp18, spp07)
  }
  
  lost_spp <- as.data.frame(table(unlist(lost_spp)))
  lost_spp <- lost_spp[which(lost_spp$Freq > 3),]
  new_spp <- as.data.frame(table(unlist(new_spp)))
  new_spp <- new_spp[which(new_spp$Freq > 3),]
  
  lost_spp <- lost_spp[order(lost_spp$Freq, decreasing = T),]
  new_spp <- new_spp[order(new_spp$Freq, decreasing = T),]
  title = paste('New ', comname, ' Plot Species Changes', sep = '')
  png(fname_new, width = 1000, height = 1000)
  p1 <- ggplot(lost_spp, aes(x = as.factor(lost_spp$Var1), y = lost_spp$Freq)) + 
    geom_bar(aes(x = reorder(Var1, -Freq), y = Freq), stat = 'identity', fill = colval) + 
    ylab('Number Plots Lost') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15, face = 'bold'),
          axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 22, face = 'bold'))
  
  p2 <- ggplot(new_spp, aes(x = as.factor(new_spp$Var1), y = new_spp$Freq)) + 
    geom_bar(aes(x = reorder(Var1, -Freq), y = Freq),stat = 'identity', fill = colval) + 
    ylab('Number Plots Gained') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15, face = 'bold'),
          axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 22, face = 'bold'))
  figure2 <- ggarrange(p1, p2, nrow = 2)
  annotate_figure(figure2, 
                  top = text_grob(title, face = 'bold', size = 27))
  ggsave(filename = fname_new)
  dev.off()
}

species_change('Deep Forest', group1_plots07, group1_plots18, dat.all, plots.env, "#000000", 'figures/Summer 2019/EcoStructure/PredictorSpp/DeepWoods_lost_spp.png', 'figures/Summer 2019/EcoStructure/PredictorSpp/DeepWoods_new_spp.png')
species_change('Edge', group3_plots07, group2_plots18, dat.all, plots.env, "#56B4E9",'figures/Summer 2019/EcoStructure/PredictorSpp/Edge_lost_spp.png', 'figures/Summer 2019/EcoStructure/PredictorSpp/Edge_new_spp.png')
species_change('Disturbed', group2_plots07, group3_plots18, dat.all, plots.env, "#E69F00",'figures/Summer 2019/EcoStructure/PredictorSpp/Disturbed_lost_spp.png', 'figures/Summer 2019/EcoStructure/PredictorSpp/Disturbed_new_spp.png')




# burning / management effect on change 

png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/mgmt_effect_on_changeall.png', width = 800, height = 600)
ggplot(data = plots.env, aes(x = as.factor(plots.env$MgmtUnit), y = as.numeric(plots.env$OverallChange), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('Change Overall') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
dev.off()
png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/mgmt_effect_on_change1.png', width = 800, height = 600)
ggplot(data = plots.env, aes(x = as.factor(plots.env$MgmtUnit), y = as.numeric(plots.env$ChangeGroup1), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('Change Deep Woods') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
dev.off()
png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/mgmt_effect_on_change2.png', width = 800, height = 600)
ggplot(data = plots.env, aes(x = as.factor(plots.env$MgmtUnit), y = as.numeric(plots.env$ChangeGroup2), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('Change Edge Plots') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
dev.off()
png('figures/Summer 2019/EcoStructure/Omega/BoxPlots/mgmt_effect_on_change3.png', width = 800, height = 600)
ggplot(data = plots.env, aes(x = as.factor(plots.env$MgmtUnit), y = as.numeric(plots.env$ChangeGroup3), fill = as.factor(plots.env$ComClass18)))+
  geom_boxplot(outlier.fill = 'white', outlier.color = 'black', outlier.shape = 21) + 
  ylab('') +
  ggtitle('Change Disturbed Plots') + 
  stat_compare_means(comparisons = my_comparisons) + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(face='bold', size = 22),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbbPalette)
dev.off()
fig <- ggarrange(p1, p2, p3, p4, ncols = 2, nrows = 2, common.legend = T)
annotate_figure(fig, 
                top = text_grob('Management Effects on Change', face = 'bold', size = 30))
dev.off()

png('figures/Summer 2019/Maps/Envt_data/change_tree_cover.png', width = 800, height = 600)
ggplot(plots.env, aes(x = lon, y = lat, size = abs(TreeCover18-TreeCover07))) + 
  geom_point(shape = 21, col = 'chartreuse', fill = 'darkgreen') + 
  coord_equal() + 
  ggtitle('Change Tree Cover 2007 - 2018') + 
  theme(plot.title = element_text(face = 'bold', size = 27), legend.position = 'none')
dev.off()

png('figures/Summer 2019/Maps/Envt_data/fire_freq_est.png', width = 800, height = 600)
ggplot(plots.env, aes(x = lon, y = lat, size = BurnCount)) + 
  geom_point(shape = 21, col = 'red', fill = 'orange') + 
  coord_equal() + 
  ggtitle('Burn Count Estimate 2007 - 2018') + 
  theme(plot.title = element_text(face = 'bold', size = 27), legend.position = 'none')
dev.off()

# alliaria change 
dat.07 <- dat.all[which(dat.all$year == '2007'),]
dat.18 <- dat.all[which(dat.all$year == '2018'),]

get_spp_cover <- function(dat, spp){
  dat <- dat[which(!is.na(dat$cover)),]
  dat <- dat[which(dat$cover != 0),]
  dat$cover <- as.numeric(dat$cover)
  total_cover <- 
    data.frame(
      cov = sapply(unique(dat$plot), function(x){
        sum(dat$cover[which(dat$plot == x & dat$accepted_name %in% spp)], na.rm = T)
      }
      ),
      row.names = unique(dat$plot)
    )
  total_cover$cov <- as.numeric(as.character(total_cover$cov))
  return(total_cover)
}

ALLPET_cov07 <- get_spp_cover(dat.07, 'Alliaria petiolata')
ALLPET_cov18 <- get_spp_cover(dat.18, 'Alliaria petiolata')

plots.env$AllPet07 <- ALLPET_cov07$cov[match(rownames(plots.env), rownames(ALLPET_cov07))]
plots.env$AllPet18 <- ALLPET_cov18$cov[match(rownames(plots.env), rownames(ALLPET_cov18))]

png('figures/Summer 2019/Maps/change_alliaria.png', width = 1500, height = 600)
p1<- ggplot(plots.env, aes(x = lon, y = lat, size = AllPet07)) + 
  geom_point(shape = 21, col = 'darkgreen', fill = 'green') + 
  coord_equal() + 
  ggtitle('Alliaria Cover Estimate 2007') + 
  theme(plot.title = element_text(face = 'bold', size = 27))
p2 <- ggplot(plots.env, aes(x = lon, y = lat, size = AllPet18)) + 
  geom_point(shape = 21, col = 'darkgreen', fill = 'green') + 
  coord_equal() + 
  ggtitle('Alliaria Cover Estimate 2018') + 
  theme(plot.title = element_text(face = 'bold', size = 27))
ggarrange(p1, p2, ncol = 2, common.legend = T)
dev.off()

ggplot(plots.env, aes(x = lon, y = lat, size = AllPet18 - AllPet07)) + 
  geom_point(shape = 21, col = 'darkgreen', fill = 'green') + 
  coord_equal() + 
  ggtitle('Alliaria Cover Change 2007 - 2018')

TILIA_cov07 <- get_spp_cover(dat.07, 'Tilia americana')
TILIA_cov18 <- get_spp_cover(dat.18, 'Tilia americana')

plots.env$Tilia07 <- TILIA_cov07$cov[match(rownames(plots.env), rownames(TILIA_cov07))]
plots.env$Tilia18 <- TILIA_cov18$cov[match(rownames(plots.env), rownames(TILIA_cov18))]

png('figures/Summer 2019/Maps/change_tilia.png', width = 1500, height = 600)
p1<- ggplot(plots.env, aes(x = lon, y = lat, size = Tilia07)) + 
  geom_point(shape = 21, col = 'darkgreen', fill = 'green') + 
  coord_equal() + 
  ggtitle('Tilia Cover Estimate 2007') + 
  theme(plot.title = element_text(face = 'bold', size = 27))
p2 <- ggplot(plots.env, aes(x = lon, y = lat, size = Tilia18)) + 
  geom_point(shape = 21, col = 'darkgreen', fill = 'green') + 
  coord_equal() + 
  ggtitle('Tilia Cover Estimate 2018') + 
  theme(plot.title = element_text(face = 'bold', size = 27))
ggarrange(p1, p2, ncol = 2, common.legend = T)
dev.off()

Frax_cov07 <- get_spp_cover(dat.07, c('Fraxinus americana', 'Fraxinus pennsylvanica', 'Fraxinus quadrangulata', 'Fraxinus'))
Frax_cov18 <- get_spp_cover(dat.18, c('Fraxinus americana', 'Fraxinus pennsylvanica', 'Fraxinus quadrangulata', 'Fraxinus'))
plots.env$Frax07 <- Frax_cov07$cov[match(rownames(plots.env), rownames(Frax_cov07))]
plots.env$Frax18 <- Frax_cov18$cov[match(rownames(plots.env), rownames(Frax_cov18))]

png('figures/Summer 2019/Maps/change_Frax.png', width = 1500, height = 600)
p1<- ggplot(plots.env, aes(x = lon, y = lat, size = Frax07)) + 
  geom_point(shape = 21, col = 'darkgreen', fill = 'green') + 
  coord_equal() + 
  ggtitle('Fraxinus Cover Estimate 2007') + 
  theme(plot.title = element_text(face = 'bold', size = 27))
p2 <- ggplot(plots.env, aes(x = lon, y = lat, size = Frax18)) + 
  geom_point(shape = 21, col = 'darkgreen', fill = 'green') + 
  coord_equal() + 
  ggtitle('Fraxinus Cover Estimate 2018') + 
  theme(plot.title = element_text(face = 'bold', size = 27))
ggarrange(p1, p2, ncol = 2, common.legend = T)
dev.off()
