#make a community data matrix with values for how much a species covers a plot 

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses')
dat <- read.csv('../data_processing/OUTPUTS/cover_18.csv', as.is = T, header = T)[2:5]
dat$cover <- as.numeric(dat$cover)
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
colnames(dat.mat) <- gsub('-', '_', colnames(dat.mat))
colnames(dat.mat) <- gsub(' ', '_', colnames(dat.mat))
write.csv(dat.mat, 'OUTPUTS/cover_mat_18.csv')

#row.names(comm_dat_mat) <- comm_dat_mat$X
#comm_dat_mat$X <- NULL

