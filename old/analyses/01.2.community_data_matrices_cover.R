#COMMUNITY DATA MATRICES COVER
#creates a cover based community matrix for herbs, shrubs, and trees

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
source('~/Documents/morton arb/east_woods_phylogeny/analyses/SCRIPTS/newstuff/00.read_data.R')
source('~/Documents/morton arb/east_woods_phylogeny/data_processing/SCRIPTS/02.cover.R')
setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/') #i think that my directory is getting changed when 
                                                               #i source in the cover script from data processing 
                                                               #i dont even know at this point just trying fix this
##################################################################################################

#HERBS

dat <- dat.all07[which(dat.all07$veg_type == 'H'),]
dat$cover <- NULL
dat$veg_type <- NULL
dat$percent_total_cover <- as.numeric(dat$percent_total_cover)
names(dat) <- c('plot', 'sp', 'cover', 'accname')
vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$accname)))
vects$sp <- vects$sp[which(!vects$sp %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$sp),
                  dimnames = list(vects$plots, vects$sp))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accname'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accname']] <- 
    ifelse(!is.na(dat.mat[dat[i, 'plot'], dat[i, 'accname']]),
           ((dat.mat[dat[i, 'plot'], dat[i, 'accname']]) + dat[i,'cover']),
           dat[i, 'cover'])
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv3$tip.label, colnames(dat.mat))
dat.mat.herbs.c <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
dat.mat.herbs.c <- dat.mat.herbs.c[which(rownames(dat.mat.herbs.c) %in% intersect(rownames(marlin_data), rownames(dat.mat.herbs.c))),]

write.csv(dat.mat.herbs.c, '~/Documents/morton arb/east_woods_phylogeny/analyses/OUTPUTS/community_matrices/herbs_matrix_cover.csv') #write file to appropriate csv


###################################################################################################
#SHRUBS

dat <- dat.all07[which(dat.all07$veg_type == 'S'),]
dat$cover <- NULL
dat$veg_type <- NULL
dat$percent_total_cover <- as.numeric(dat$percent_total_cover)
names(dat) <- c('plot', 'sp', 'cover', 'accname')
vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$accname)))
vects$sp <- vects$sp[which(!vects$sp %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$sp),
                  dimnames = list(vects$plots, vects$sp))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accname'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accname']] <- 
    ifelse(!is.na(dat.mat[dat[i, 'plot'], dat[i, 'accname']]),
           ((dat.mat[dat[i, 'plot'], dat[i, 'accname']]) + dat[i,'cover']),
           dat[i, 'cover'])
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv3$tip.label, colnames(dat.mat))
dat.mat.shrubs.c <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
dat.mat.shrubs.c <- dat.mat.shrubs.c[which(rownames(dat.mat.shrubs.c) %in% intersect(rownames(marlin_data), rownames(dat.mat.shrubs.c))),]

write.csv(dat.mat.shrubs.c, '~/Documents/morton arb/east_woods_phylogeny/analyses/OUTPUTS/community_matrices/shrubs_matrix_cover.csv') #write file to appropriate csv

###################################################################################################
#trees

dat <- dat.all07[which(dat.all07$veg_type == 'T'),]
dat$cover <- NULL
dat$veg_type <- NULL
dat$percent_total_cover <- as.numeric(dat$percent_total_cover)
names(dat) <- c('plot', 'sp', 'cover', 'accname')
vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$accname)))
vects$sp <- vects$sp[which(!vects$sp %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$sp),
                  dimnames = list(vects$plots, vects$sp))

for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accname'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accname']] <- 
    ifelse(!is.na(dat.mat[dat[i, 'plot'], dat[i, 'accname']]),
           ((dat.mat[dat[i, 'plot'], dat[i, 'accname']]) + dat[i,'cover']),
           dat[i, 'cover'])
}

colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
names_in_tree <- intersect(tr.ewv3$tip.label, colnames(dat.mat))
dat.mat.trees.c <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
dat.mat.trees.c <- dat.mat.trees.c[which(rownames(dat.mat.trees.c) %in% intersect(rownames(marlin_data), rownames(dat.mat.trees.c))),]

write.csv(dat.mat.trees.c, '~/Documents/morton arb/east_woods_phylogeny/analyses/OUTPUTS/community_matrices/trees_matrix_cover.csv') #write file to appropriate csv
