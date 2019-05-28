#species correlation with environmental data

setwd('~/Documents/GitHub/east_woods_work/')
dat.mat.all.07 <- read.csv('data/dat.mat.all.07.csv', row.names = 1)
dat.mat.all.18 <- read.csv('data/dat.mat.all.18.csv', row.names = 1)
gibbons_data <- read.csv('data/gibbons_data.csv')
library(ggtree)
library(ggplot2)
library(ggrepel)

#-------------------------------------------------------------------------------------------------

dat.mat.all.18 <- dat.mat.all.18[which(rownames(dat.mat.all.18) %in% gibbons_data$plots),]
gibbons_data <- gibbons_data[which(gibbons_data$plots %in% rownames(dat.mat.all.18)),]
gibbons_data$burn_count <- as.numeric(as.character(gibbons_data$burn_count))
gibbons_data$soil_index <- as.numeric(as.character(gibbons_data$soil_index))
gibbons_data$geo_drainage <- as.numeric(as.character(gibbons_data$geo_drainage))
gibbons_data$canopy18 <- as.numeric(as.character(gibbons_data$canopy18))
gibbons_data$inv_ratio18 <- as.numeric(as.character(gibbons_data$inv_ratio18))


all.r.aspect <- apply(dat.mat.all.18, 2, cor, gibbons_data$aspect)
all.r.burn_count <-cor(dat.mat.all.18, gibbons_data$burn_count, use = "complete.obs")
all.r.soil_index <- cor(dat.mat.all.18, gibbons_data$soil_index, use = "complete.obs")
all.r.drainage <- cor(dat.mat.all.18, gibbons_data$drainage, use = "complete.obs")
all.r.canopy18 <- cor(dat.mat.all.18, gibbons_data$canopy18, use = "complete.obs")
all.r.invasive_ratio18 <- cor(dat.mat.all.18, gibbons_data$inv_ratio18, use = "complete.obs")

all_cor <- data.frame(all.r.burn_count)
all_cor$soil_index <- cbind(all.r.soil_index)
all_cor$drainage <- cbind(all.r.drainage)
all_cor$canopy18 <- cbind(all.r.canopy18)
all_cor$invasive_ratio18 <- cbind(all.r.invasive_ratio18)

# x[order(x)[1:5]]
topburn <- all_cor[order(all_cor$all.r.burn_count)[1:10],]
lowburn <- all_cor[order(-all_cor$all.r.burn_count)[1:10],]
topsoil <- all_cor[order(all_cor$soil_index)[1:10],]
lowsoil <- all_cor[order(-all_cor$soil_index)[1:10],]
topdrain <- all_cor[order(all_cor$drainage)[1:10],]
lowdrain <- all_cor[order(-all_cor$drainage)[1:10],]
topcanopy <- all_cor[order(all_cor$canopy18)[1:10],]
lowcanopy <- all_cor[order(-all_cor$canopy18)[1:10],]
topinv <- all_cor[order(all_cor$invasive_ratio18)[1:10],]
lowinv <- all_cor[order(-all_cor$invasive_ratio18)[1:10],]

rsq_plots <- function(df, column, ax_label, clr){
  stupid <- c(1,2,3,4,5,6,7,8,9,10)
  theme_set(theme_minimal())
  p1 <- ggplot()+
    geom_point(aes(x = stupid, y = column), color = clr) + 
    geom_label_repel(aes(x = stupid,  y = column), label = row.names(df),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') + 
    xlab(' ') + 
    ylab('R2') + 
    ggtitle(ax_label)
  ggsave(filename = paste('spp_cor',ax_label,'.png', sep = ''), plot = p1,
         device = 'png', width = 6, height = 6, path = '~/Desktop/plots')
}

rsq_plots(topburn, topburn$all.r.burn_count, 'Burn Frequency', clr = 'blue')
rsq_plots(topcanopy, topcanopy$canopy18, 'Canopy', clr = 'red')
rsq_plots(topdrain, topdrain$drainage, 'Drainage', clr = 'purple')
rsq_plots(topinv, topinv$invasive_ratio18, 'Invasives Ratio', clr = 'green')
rsq_plots(topsoil, topsoil$soil_index, 'Soil Index', clr = 'orange')

rsq_plots(lowburn, lowburn$all.r.burn_count, 'low Burn Frequency', clr = 'blue')
rsq_plots(lowcanopy, lowcanopy$canopy18, 'low Canopy', clr = 'red')
rsq_plots(lowdrain, lowdrain$drainage, 'low Drainage', clr = 'purple')
rsq_plots(lowinv, lowinv$invasive_ratio18, 'low Invasives Ratio', clr = 'green')
rsq_plots(lowsoil, lowsoil$soil_index, 'low Soil Index', clr = 'orange')

stupid <- c(1,2,3,4,5,6,7,8,9,10)
theme_set(theme_minimal())
ggplot()+
  geom_point(aes(x = stupid, y = topburn$all.r.burn_count), color = 'blue') + 
  #geom_text(aes(x = stupid,  y = topburn$all.r.burn_count), label = row.names(topburn),hjust=0, vjust=0) + 
  geom_label_repel(aes(x = stupid,  y = topburn$all.r.burn_count), label = row.names(topinv),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') + 
  xlab(' ') + 
  ylab('R2') + 
  ggtitle('Burn Frequency')
ggplot()+
  geom_point(aes(x = stupid, y = topsoil$soil_index), color = 'red') + 
  #geom_text(aes(x = stupid,  y = topsoil$soil_index), label = row.names(topsoil),hjust=0, vjust=0) + 
  geom_label_repel(aes(x = stupid, y = topsoil$soil_index), label = row.names(topsoil),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') + 
  xlab(' ') + 
  ylab('R2') + 
  ggtitle('Soil Index')
ggplot()+
  geom_point(aes(x = stupid, y = topdrain$drainage), color = 'purple') + 
  #geom_text(aes(x = stupid, y = topdrain$drainage),label = row.names(topdrain),hjust=0, vjust=0) + 
  geom_label_repel(aes(x = stupid, y = topdrain$drainage), label = row.names(topdrain),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') + 
  xlab(' ') + 
  ylab('R2') + 
  ggtitle('Drainage')

ggplot()+
  geom_point(aes(x = stupid, y = topcanopy$canopy18), color = 'yellow') + 
  #geom_text(aes(x = stupid, y = topcanopy$canopy18),label = row.names(topcanopy),hjust=0, vjust=0) +
  geom_label_repel(aes(x = stupid, y = topcanopy$canopy18), label = row.names(topcanopy),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') + 
  xlab(' ') + 
  ylab('R2') + 
  ggtitle('Canopy')

ggplot()+ 
  geom_point(aes(x = stupid, y = topinv$invasive_ratio18), color = 'orange') + 
  #geom_text(aes(x = stupid, y = topinv$invasive_ratio18), label = row.names(topinv),hjust=0, vjust=0) + 
  geom_label_repel(aes(x = stupid, y = topinv$invasive_ratio18), label = row.names(topinv),
                 box.padding   = 0.35, 
                 point.padding = 0.5,
                 segment.color = 'grey50') + 
  xlab(' ') + 
  ylab('R2') + 
  ggtitle('Invasive Ratio')


# topburn <- max(all_cor[,1], na.rm = T)
# lowburn <- min(all_cor[,1], na.rm = T)
# topsoil <- max(all_cor[,2], na.rm = T)
# lowsoil <- min(all_cor[,2], na.rm = T)
# topdrain <- max(all_cor[,3], na.rm = T)
# lowdrain <- min(all_cor[,3], na.rm = T)
# tocanopy <- max(all_cor[,4], na.rm = T)
# lowcanopy <- min(all_cor[,4], na.rm = T)
# topinv <- max(all_cor[,5], na.rm = T)
# lowinv <- min(all_cor[,5], na.rm = T)
# points <- c(topburn, lowburn, topsoil, lowsoil, topdrain, lowdrain, tocanopy, lowcanopy, topinv, lowinv)
# spp <- c('Acer Saccharum', 'Poa pratensis', '')

# will continue adding to this as I generate more continous variables 
tree <- ggtree(tr.ewv4)
p <- gheatmap(tree, all_cor, width = 0.25,
              colnames_position = 'bottom',
              colnames_angle = 270,
              low = 'yellow', 
              high = 'mediumblue',
              font.size = 3, hjust = 0)
p

