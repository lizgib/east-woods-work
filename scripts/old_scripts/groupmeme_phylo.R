# messing around wiht another figure
# looking at phylogenetic relationship to memebership in a community
library(ggtree)

tr.ewv3 <- read.tree('outputs/tr.ew.Spring19')
data('plots.env')
groupmeme18 <- read.csv('figures/Summer 2019/EcoStructure/Omega/2018/k3dat_mat_all_18.csv')
names(groupmeme18) <- c('Plot', 'Group1.18', 'Group2.18', 'Group3.18')
groupmeme07 <- read.csv('figures/Summer 2019/EcoStructure/Omega/2007/k3dat_mat_all_07.csv')
names(groupmeme07) <- c('Plot', 'Group1.07', 'Group2.07', 'Group3.07')
plots.env$Group1.18 <- as.numeric(groupmeme18$Group1.18[match(rownames(plots.env), groupmeme18$Plot)])
plots.env$Group2.18 <- as.numeric(groupmeme18$Group2.18[match(rownames(plots.env), groupmeme18$Plot)])
plots.env$Group3.18 <- as.numeric(groupmeme18$Group3.18[match(rownames(plots.env), groupmeme18$Plot)])
plots.env$Group1.07 <- as.numeric(groupmeme07$Group1.07[match(rownames(plots.env), groupmeme07$Plot)])
plots.env$Group2.07 <- as.numeric(groupmeme07$Group2.07[match(rownames(plots.env), groupmeme07$Plot)])
plots.env$Group3.07 <- as.numeric(groupmeme07$Group3.07[match(rownames(plots.env), groupmeme07$Plot)])

dat.mat.all.18 <- read.csv('data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)
dat.mat.all.07 <- read.csv('data/Community_Matrix/2007/dat.mat.all.07.csv', row.names = 1)

group1.r <- cor(dat.mat.all.18, plots.env$Group1.18, use = 'complete.obs')
group2.r <-cor(dat.mat.all.18, plots.env$Group2.18, use = "complete.obs")
group3.r <- cor(dat.mat.all.18, plots.env$Group3.18, use = "complete.obs")
group1.r.07 <- cor(dat.mat.all.07, plots.env$Group1.07, use = 'complete.obs')
group2.r.07 <- cor(dat.mat.all.07, plots.env$Group2.07, use = 'complete.obs')
group3.r.07 <- cor(dat.mat.all.07, plots.env$Group3.07, use = 'complete.obs')

all_cor_18 <- cbind(group1.r, group2.r, group3.r)
all_cor_18 <- as.data.frame(all_cor_18)
all_cor_07 <- cbind(group1.r.07, group2.r.07, group3.r.07)
all_cor_07 <- as.data.frame(all_cor_07)
names(all_cor_18) <- c('Deep Forest', 'Edge', 'Disturbed')
names(all_cor_07) <- c('Deep Forest', 'Disturbed', 'Edge')


# redo this with the average number occurences each figure in plot with the degree membership in that group

# also annotate the phylogeny 

tree <- ggtree(tr.ewv3)

png('figures/Summer 2019/EcoStructure/Omega/phylo_signal_EcoStrc_group18.png',  height = 1000, width = 800)
gheatmap(tree, all_cor_18, width = 0.25,
              colnames_position = 'bottom',
              colnames_angle = 270,
              low = 'yellow', 
              high = 'mediumblue',
              font.size = 3, hjust = 0)
dev.off()

png('figures/Summer 2019/EcoStructure/Omega/phylo_signal_EcoStrc_group07.png', height = 1000, width = 800)
gheatmap(tree, all_cor_07, width = 0.25,
         colnames_position = 'bottom',
         colnames_angle = 270,
         low = 'yellow', 
         high = 'mediumblue',
         font.size = 3, hjust = 0)
dev.off()



