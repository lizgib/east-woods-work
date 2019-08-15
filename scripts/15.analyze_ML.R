
library(ggplot2)
theme_set(theme_classic())
species_imp <- read.csv('data/MachineLearning/Matrix/species_change.csv_RF_imp.csv', header = F)
plot_imp <- read.csv('data/MachineLearning/Matrix/plots_change.csv_RF_imp', sep = '\t', header = F)
allfeat_imp <- read.csv('data/MachineLearning/Matrix/all_change.csv_RF_imp', sep = '\t', header = F)

png('figures/Summer 2019/MachineLearning/FeatureImportance/most_important_species.png', width = 800, height = 700)
ggplot(species_imp[which(species_imp$V2 > 0.01),]) + 
  geom_bar(aes(x = reorder(V1, -V2), y = V2), stat = 'identity', fill = '#E69F00') + 
  ggtitle('Top Predictors For Change - Species') + 
  theme(axis.text.x = element_text(angle = 90, size = 15),
        axis.title.x = element_blank(),
        plot.title = element_text(face = 'bold', size = 27))
dev.off()

png('figures/Summer 2019/MachineLearning/FeatureImportance/most_important_all.png', width = 800, height = 700)
ggplot(allfeat_imp[which(allfeat_imp$V2 > 0.01),]) + 
  geom_bar(aes(x = reorder(V1, -V2), y = V2), stat = 'identity', fill = '#56B4E9') + 
  ggtitle('Top Predictors For Change - All Data') + 
  theme(axis.text.x = element_text(angle = 45, size = 15, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(face = 'bold', size = 27))
dev.off()

png('figures/Summer 2019/MachineLearning/FeatureImportance/most_important_plot.png', width = 800, height = 700)
ggplot(plot_imp[which(plot_imp$V2 > 0.01),]) + 
  geom_bar(aes(x = reorder(V1, -V2), y = V2), stat = 'identity', fill = '#009E73') + 
  ggtitle('Top Predictors for Change - Plot Data') + 
  theme(axis.text.x = element_text(angle = 45, size = 15, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(face = 'bold', size = 27))
dev.off()

