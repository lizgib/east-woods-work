# Species correlation with environmental data

setwd('~/Documents/GitHub/east_woods_work/')
dat.mat.all.07 <- read.csv('data/Community_Matrix/2007/dat.mat.all.07.csv', row.names = 1)
dat.mat.all.18 <- read.csv('data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)
data("plots.env")
library(ggtree)
library(ggplot2)
library(ggrepel)
theme_set(theme_minimal())
tree.file <- read.tree('data/Phylogeny/tr.ew.Spring19')

#-------------------------------------------------------------------------------------------------

plots.env$elev <- as.numeric(plots.env$elev)
plots.env$slope <- as.numeric(plots.env$slope)
plots.env$aspect <- as.numeric(plots.env$aspect)
plots.env$tpi <- as.numeric(plots.env$tpi)
plots.env$TreeCover07 <- as.numeric(plots.env$TreeCover07)
plots.env$TreeCover18 <- as.numeric(plots.env$TreeCover18)
plots.env$BurnCount <- as.numeric(plots.env$BurnCount)
plots.env$TopOrgMat <- as.numeric(plots.env$TopOrgMat)
plots.env$LowOrgMat <- as.numeric(plots.env$LowOrgMat)
plots.env$TopDryWgt <- as.numeric(plots.env$TopDryWgt)
plots.env$LowDryWgt <- as.numeric(plots.env$TopWtrContent)
plots.env$LowWtfContent <- as.numeric(plots.env$LowWtfContent)
plots.env$PathDist <- as.numeric(plots.env$PathDist)
plots.env$RoadDist <- as.numeric(plots.env$RoadDist)
plots.env$RoadPathDist <- as.numeric(plots.env$RoadPathDist)
plots.env$SolRad <- as.numeric(plots.env$SolRad)


all_cor <- data.frame(colnames(dat.mat.all.18))
# all_cor$elev <- cor(dat.mat.all.18, plots.env$elev, use = 'complete.obs')
# all_cor$slope <- cor(dat.mat.all.18, plots.env$slope, use = "complete.obs")
# all_cor$aspect <- cor(dat.mat.all.18, plots.env$aspect, use = "complete.obs")
# all_cor$tpi <- cor(dat.mat.all.18, plots.env$tpi, use = "complete.obs")
# all_cor$TreeCover07 <- cor(dat.mat.all.18, plots.env$TreeCover07, use = 'complete.obs')
# all_cor$TreeCover18 <- cor(dat.mat.all.18, plots.env$TreeCover18,  use = 'complete.obs')
# all_cor$BurnCount <- cor(dat.mat.all.18, plots.env$BurnCount, use = 'complete.obs')
all_cor$TopOrgMat <- cor(dat.mat.all.18, plots.env$TopOrgMat, use = "complete.obs")
all_cor$LowOrgMat <- cor(dat.mat.all.18, plots.env$LowOrgMat, use = "complete.obs")
all_cor$TopDryWgt <- cor(dat.mat.all.18, plots.env$TopDryWgt, use = "complete.obs")
all_cor$LowDryWgt <- cor(dat.mat.all.18, plots.env$LowDryWgt, use = "complete.obs")
all_cor$TopWtrContent <- cor(dat.mat.all.18, plots.env$TopWtrContent, use = "complete.obs")
all_cor$LowWtrContent <- cor(dat.mat.all.18, plots.env$LowWtfContent, use = "complete.obs")
all_cor$PathDist <- cor(dat.mat.all.18, plots.env$PathDist, use = "complete.obs")
all_cor$RoadDist <- cor(dat.mat.all.18, plots.env$RoadDist, use = "complete.obs")
all_cor$RoadPathDist <- cor(dat.mat.all.18, plots.env$RoadPathDist, use = "complete.obs")
all_cor$SolRad <- cor(dat.mat.all.18, plots.env$SolRad, use = "complete.obs")

rownames(all_cor) <- all_cor$colnames.dat.mat.all.18.
all_cor$colnames.dat.mat.all.18. <- NULL

tree.file <- drop.tip(tree.file, tip = tree.file$tip.label[!tree.file$tip.label %in% colnames(dat.mat.all.18)])
tree <- ggtree(tree.file)
tree <- tree + geom_tiplab(size = 0.5)
tiff('figures/Summer 2019/Phylogeny/species_cor_envt.tiff', units = 'in', height = 10, width = 20, res = 400)
gheatmap(tree, all_cor, width = 0.25,
         colnames_position = 'bottom',
         colnames_angle = 270, 
         low = 'pink', high = 'blue',
         font.size = 3, hjust = 0)
dev.off()

