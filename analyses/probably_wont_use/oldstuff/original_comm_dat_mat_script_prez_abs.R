#make community matrix for presence/absence of spp in plot

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')

dat <- read.csv('../data_processing/OUTPUTS/allshrubs.csv', as.is = T, header = T)
dat$X <- NULL
dat$code <- NULL
dat$stems.alive <- NULL
names(dat) <- c('plot', 'sp', 'accname')
vects <- list(plots = unique(sort(dat$plot)),
              sp = unique(sort(dat$accname)))
vects$sp <- vects$sp[which(!vects$sp %in% c('', ' ', '0'))]
dat.mat <- matrix(0, length(vects$plots), length(vects$sp),
                  dimnames = list(vects$plots, vects$sp))


for(i in 1:dim(dat)[1]){
  if(!dat[i, 'accname'] %in% dimnames(dat.mat)[[2]]) next 
  dat.mat[dat[i, 'plot'], dat[i, 'accname']] <- 1
    # ifelse(is.na(dat.mat[dat[i, 'plot'], dat[i, 'accname']]),
    #        1, dat.mat[dat[i, 'plot'], dat[i, 'accname']] + 1)
   # yields counts... probably not ideal
# to yield presence / absence, just put 1 in the cells rather than the sum of the cell and 1
}
colnames(dat.mat) <- gsub(' ', '_', colnames(dat.mat))
write.csv(dat.mat, 'OUTPUTS/shrubs_mat.csv')

# dat.mat.0s <- dat.mat # you may never need this
# dat.mat.0s[is.na(dat.mat.0s)] <- 0
# 
# mean[x[which(x > 0)]]
