#COMMUNITY DATA MATRICES PRESENCE ABSENCE
#creates a presense absence based community matrix for herbs, shrubs, and trees

source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/newstuff/00.read_data.R')
source('~/Documents/morton arb/east_woods_phylogeny/data_processing/SCRIPTS/02.cover.R')
setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/') #i think that my directory is getting changed when 
                                                               #i source in the cover script from data processing 
                                                               #i dont even know at this point just trying fix this
###################################################################################################

#HERBS

# dat <- allherbs[2:6]
# dat <- dat[c(-2,-4)]
dat <- dat.all07[which(dat.all07$veg_type == 'H'),]
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
dat.mat.herbs <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
dat.mat.herbs <- dat.mat.herbs[which(rownames(dat.mat.herbs) %in% intersect(rownames(marlin_data), rownames(dat.mat.herbs))),]

write.csv(dat.mat.herbs, 'OUTPUTS/community_matrices/herbs_matrix_pres_abs.csv') #write file to appropriate csv

###################################################################################################
#SHRUBS

# dat <- allshrubs[2:6]
# dat <- dat[c(-2,-4)]
dat <- dat.all07[which(dat.all07$veg_type == 'S'),]
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
dat.mat.shrubs <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
dat.mat.shrubs <- dat.mat.shrubs[which(rownames(dat.mat.shrubs) %in% intersect(rownames(marlin_data), rownames(dat.mat.shrubs))),]

write.csv(dat.mat.shrubs, 'OUTPUTS/community_matrices/shrubs_matrix_pres_abs.csv') #write file to appropriate csv

###################################################################################################
#trees

# dat <- alltrees[2:6]
# dat <- dat[c(-2,-4)]
dat <- dat.all07[which(dat.all07$veg_type == 'T'),]
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
dat.mat.trees <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
dat.mat.trees <- dat.mat.trees[which(rownames(dat.mat.trees) %in% intersect(rownames(marlin_data), rownames(dat.mat.trees))),]


write.csv(dat.mat.trees, 'OUTPUTS/community_matrices/trees_matrix_pres_abs.csv') #write file to appropriate csv
###################################################################################################
# dat <- allshrubs[2:6]
# dat <- dat[c(-2,-4)]
dat <- dat.all07
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
dat.mat.all <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
dat.mat.all <- dat.mat.all[which(rownames(dat.mat.all) %in% intersect(rownames(marlin_data), rownames(dat.mat.all))),]
dat.mat.all <- dat.mat.all[which(rownames(dat.mat.all) != 'BB140'),]
dat.mat.all <- dat.mat.all[which(rownames(dat.mat.all) != 'PP132'),]

write.csv(dat.mat.shrubs, 'OUTPUTS/community_matrices/all_matrix_pres_abs.csv')
