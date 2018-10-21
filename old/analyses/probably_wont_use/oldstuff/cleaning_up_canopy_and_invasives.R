#fixing the canopy analysis 

source('~/Documents/morton arb/east_woods_phylogeny/analyses/wow.R')
source('~/Documents/morton arb/east_woods_phylogeny/data_processing/SCRIPTS/02.cover.R')

marlin_data.canopy <- marlin_data[which(marlin_data$CanopyOpenness < 50),]
new_canopy_dist <- dist(marlin_data.canopy$CanopyOpenness, method = 'euclidean')

#need to make a new community matrix excluding these plots now 

dat <- dat.all07[which(dat.all07$plot %in% intersect(dat.all07$plot, rownames(marlin_data.canopy))),]
dat$cover <- NULL
dat$veg_type <- NULL
dat$percent_total_cover <- NULL
names(dat) <- c('plot', 'sp', 'accname')
vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$sp)),
              accname = unique(sort(dat$accname)))
vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                  dimnames = list(vects$plots, vects$accname))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accname'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accname']] <- 1
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv3$tip.label, colnames(dat.mat))
dat.mat.all.canopy <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
dat.mat.all.canopy <- dat.mat.all.canopy[which(rownames(dat.mat.all.canopy) %in% intersect(rownames(marlin_data), rownames(dat.mat.all.canopy))),]

#now make that into the phylo dist 
canopy_Dnn_all.pa <- comdistnt(dat.mat.all.canopy, cophenetic(tr.ewv3))


ggplot()+
  geom_point(aes(new_canopy_dist, canopy_Dnn_all.pa), color = 'orange') + 
  xlab(' ') + 
  ylab(' ')+
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))

canopy_mantel <- mantelMultiple(canopy_Dnn_all.pa, X = list(new_canopy_dist))

ggplot()+ 
   geom_point(aes(PD_trees$PD, PD_herbs$PD), color = 'orange') + 
   xlab(' ')+ 
   ylab(' ')+
   ggtitle(' ')



###############################################################################
#while we're at it.. lets just do the figure for invasive ratio too shall we? 

marlin_data.invasives <- marlin_data[which(!is.na(marlin_data$AlienNativeRatio)),]
marlin_data.invasives <- marlin_data.invasives[which(marlin_data.invasives$AlienNativeRatio < 1.2),]
invasives_dist <- dist(marlin_data.invasives$AlienNativeRatio, method = 'euclidean')

#need to make a new community matrix excluding these plots now 

dat <- dat.all07[which(dat.all07$plot %in% intersect(dat.all07$plot, rownames(marlin_data.invasives))),]
dat$cover <- NULL
dat$veg_type <- NULL
dat$percent_total_cover <- NULL
names(dat) <- c('plot', 'sp', 'accname')
vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$sp)),
              accname = unique(sort(dat$accname)))
vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                  dimnames = list(vects$plots, vects$accname))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accname'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accname']] <- 1
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv3$tip.label, colnames(dat.mat))
dat.mat.all.invasives<- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
dat.mat.all.invasives <- dat.mat.all.invasives[which(rownames(dat.mat.all.invasives) %in% intersect(rownames(marlin_data), rownames(dat.mat.all.invasives))),]

#now make that into the phylo dist 
invasives_Dnn_all.pa <- comdistnt(dat.mat.all.invasives, cophenetic(tr.ewv3))


ggplot()+
  geom_point(aes(invasives_dist, invasives_Dnn_all.pa), color = 'orange') + 
  xlab(' ') + 
  ylab(' ')+
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))



###############################################################################
#also doing tree group just for the hell of it 

marlin_data.trgrp <- marlin_data[which(!is.na(marlin_data$Grp)),]
tr_groups <- unique(marlin_data$Grp)
marlin_data.trgrp$Grp <- gsub('AshElm', 1, marlin_data.trgrp$Grp)
marlin_data.trgrp$Grp <- gsub('Cherry', 2, marlin_data.trgrp$Grp)
marlin_data.trgrp$Grp <- gsub('B.wood', 3, marlin_data.trgrp$Grp)
marlin_data.trgrp$Grp <- gsub('WOak', 4, marlin_data.trgrp$Grp)
marlin_data.trgrp$Grp <- gsub('BurOak', 5, marlin_data.trgrp$Grp)
marlin_data.trgrp$Grp <- gsub('ROak', 6, marlin_data.trgrp$Grp)
marlin_data.trgrp$Grp <- gsub('Maple', 7, marlin_data.trgrp$Grp)
marlin_data.trgrp$Grp <- gsub('GAsh', 8, marlin_data.trgrp$Grp)
marlin_data.trgrp$Grp <- gsub('WnutElm', 9, marlin_data.trgrp$Grp)
marlin_data.trgrp$Grp <- as.numeric(marlin_data.trgrp$Grp)

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
diversity2 <- read.csv('OUTPUTS/diversity_metrics.csv')
rownames(diversity2) <- diversity2$X
diversity2 <- diversity2[which(rownames(diversity2) %in% intersect(rownames(marlin_data.trgrp), rownames(diversity2))),]
pieceofshit <- marlin_data.trgrp[which(rownames(marlin_data.trgrp) %in% intersect(rownames(marlin_data.trgrp), rownames(diversity2))),]
pieceofshit <- pieceofshit[,-c(1,2,3,4,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)]
pieceofshit$PD <- diversity2$PD

pieceofshit <- pieceofshit[order(pieceofshit$Grp),]
one <- pieceofshit$PD[1:30]
two <-pieceofshit$PD[31:51]
three <- pieceofshit$PD[52:82]
four <- pieceofshit$PD[83:(83+90)]
five <- pieceofshit$PD[(83+90):(83+90+26)]
six <- pieceofshit$PD[(83+90+26):(83+90+26+60)]
seven <- pieceofshit$PD[(83+90+26+60):(83+90+26+60+41)]
eight <- pieceofshit$PD[(83+90+26+60+41):(83+90+26+60+41+11)]
nine <-pieceofshit$PD[(83+90+26+60+41+11):(83+90+26+60+41+11+26)]
newdf <- cbind(one,two,three,four,five,six,seven,eight,nine)
newdf <- data.frame(newdf)


#1  2  3  4  5  6  7  8  9 
#30 20 30 90 26 60 41 11 26 
mantelMultiple(pieceofshit$Grp, X = list(diversity2$PD))

group_dist <- dist(marlin_data.trgrp$Grp, method = 'euclidean')

dat <- dat.all07[which(dat.all07$plot %in% intersect(dat.all07$plot, rownames(marlin_data.trgrp))),]
dat$cover <- NULL
dat$veg_type <- NULL
dat$percent_total_cover <- NULL
names(dat) <- c('group','plot', 'sp', 'accname')
vects <- list(group = unique(sort(dat$group)),
              accname = unique(sort(dat$accname)))
vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$group), length(vects$accname),
                  dimnames = list(vects$groups, vects$accname))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accname'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'group'], dat[i, 'accname']] <- 1
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv3$tip.label, colnames(dat.mat))
dat.mat.all.trgrp<- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
dat.mat.all.trgrp <- dat.mat.all.trgrp[which(rownames(dat.mat.all.trgrp) %in% intersect(rownames(marlin_data), rownames(dat.mat.all.trgrp))),]

#now make that into the phylo dist 
trgrp_Dnn_all.pa <- comdistnt(dat.mat.all.trgrp, cophenetic(tr.ewv3))

ugh <- mantelMultiple(trgrp_Dnn_all.pa, X = list(group_dist))

ggplot()+
  geom_point(aes(group_dist, trgrp_Dnn_all.pa), color = 'orange') + 
  xlab('trgroup') + 
  ylab('PBD')+
  theme(
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(size = 20))

