library(picante)
library(vegan)

data("dune")
dat <- read.csv('~/Documents/GitHub/east_woods_work/data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)

ord <- metaMDS(dat)
plot(ord, type = 'n')
#points(ord, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
#text(ord, display = 'spec', cex = 0.7, col = 'blue')
points(ord, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
text(ord, display = 'sites')

dat2 <- dat[which(!row.names(dat) %in% c('Y121', 'GG103', 'LL122', 'X138', 'AX131', 'LL123', 'QQ121', 'R79', 'YY146', 'AY117', "OO122", "PP122", "WW119")),]

ord2 <- metaMDS(dat2)
plot(ord2, type = 'n')
points(ord2, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
text(ord2, display = 'sites', cex = 0.6)

# row.names(ord2$points[which(!ord2$points > -3),])

colnames(dat[which(dat['Y121',]==1)])
# character(0)
colnames(dat[which(dat['GG103',]==1)])
#" Sparganium_eurycarpum"
colnames(dat[which(dat['LL122',]==1)])
# "Salix_nigra"
colnames(dat[which(dat['X138',]==1)])
# "Ambrosia_artemisiifolia" "Apocynum_cannabinum"
colnames(dat[which(dat['AX131',]==1)])
# "Calystegia_sepium"    "Phalaris_arundinacea"
colnames(dat[which(dat['LL123',]==1)])
# "Phalaris_arundinacea" "Phragmites_australis" "Populus_deltoides"    "Salix_nigra"
colnames(dat[which(dat['QQ121',]==1)])
#  "Cirsium_arvense"      "Phalaris_arundinacea"
colnames(dat[which(dat['R79',]==1)])
#[1] "Alisma_subcordatum"      "Bidens_frondosa"         "Leersia_oryzoides"       "Lycopus_uniflorus"       "Persicaria_punctata"     "Scutellaria_lateriflora"
#[7] "Sium_suave"              "Sparganium_eurycarpum"   "Spartina_pectinata"
colnames(dat[which(dat['YY146',]==1)])
#[1] "Bolboschoenus_fluviatilis" "Epilobium_hirsutum"        "Impatiens_capensis"        "Onoclea_sensibilis"        "Persicaria_amphibia"      
#[6] "Sagittaria_latifolia"      "Scutellaria_galericulata"  "Solanum_dulcamara"         "Typha_latifolia" 
colnames(dat[which(dat['AY117',]==1)])
# "Impatiens_capensis"      "Scutellaria_lateriflora"
colnames(dat[which(dat['00122',]==1)])
# character(0)
colnames(dat[which(dat['PP122',]==1)])
# "Phalaris_arundinacea"
colnames(dat[which(dat['WW119',]==1)])
# "Phalaris_arundinacea"

# spp.counts <- as.matrix(colSums(dat))
# all.spp <- colnames(dat)
# barplot(spp.counts[,1], main = 'Freq of each spp 2018', horiz = TRUE,
#         names.arg = all.spp)


