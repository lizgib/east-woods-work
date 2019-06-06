# pulling the trees out from the understory matrix

dat <- read.csv('~/Documents/GitHub/east_woods_work/data/Community_Matrix/2018/dat.mat.understory.18.csv', row.names = 1)

# I looked at all the genuses reported in the 2018 tree matrix and im gonna remove these 

dat[,grep('Quercus', colnames(dat))] <- NULL
dat[,grep('Acer', colnames(dat))] <- NULL
dat[,grep('Ulmus', colnames(dat))] <- NULL
dat[,grep('Fraxinus', colnames(dat))] <- NULL
dat[,grep('Rhamus', colnames(dat))] <- NULL
dat[,grep('Populus', colnames(dat))] <- NULL
dat[,grep('Prunus', colnames(dat))] <- NULL
dat[,grep('Salix', colnames(dat))] <- NULL
dat[,grep('Tilia', colnames(dat))] <- NULL
dat[,grep('Rhus', colnames(dat))] <- NULL
dat[,grep('Juglans', colnames(dat))] <- NULL
dat[,grep('Robinia', colnames(dat))] <- NULL
dat[,grep('Pinus', colnames(dat))] <- NULL
dat[,grep('Ostrya', colnames(dat))] <- NULL
dat[,grep('Morus', colnames(dat))] <- NULL
dat[,grep('Maclura', colnames(dat))] <- NULL
dat[,grep('Liriodendron', colnames(dat))] <-NULL
dat[,grep('Gymnocladus', colnames(dat))] <- NULL
dat[,grep('Cercis', colnames(dat))] <- NULL
dat[,grep('Celtis', colnames(dat))] <- NULL
dat[,grep('Carya', colnames(dat))] <- NULL
dat[,grep('Aesculus', colnames(dat))] <- NULL


write.csv(dat, '~/Documents/GitHub/east_woods_work/data/Community_Matrix/2018/no_trees.csv')




