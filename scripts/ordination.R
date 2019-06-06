library(picante)
library(vegan)
library(RColorBrewer)

dat <- read.csv('~/Documents/GitHub/east_woods_work/data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)

#----------------------------------
# here I wanted to see what species were coming up as so significantly different in the ordination 
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
# Phalaris_arundinacea
colnames(dat[which(dat['PP122',]==1)])
# "Phalaris_arundinacea"
colnames(dat[which(dat['WW119',]==1)])
# "Phalaris_arundinacea"

# spp.counts <- as.matrix(colSums(dat))
# all.spp <- colnames(dat)
# barplot(spp.counts[,1], main = 'Freq of each spp 2018', horiz = TRUE,
#         names.arg = all.spp)
#----------------------------------

# looking at the groupings of plots based on different units (management and area)
plots.env <- read.csv('~/Documents/GitHub/east_woods_work/data/East_Woods/Inventory_2018/Analyses_Rollinson/point_info_GIS.csv')
plots.env$PlotID <- gsub('-', '', plots.env$PlotID)
plots.burn <- read.csv('~/Documents/GitHub/east_woods_work/data/East_Woods/Inventory_2018/Analyses_Rollinson/point_info_GIS_burnhistory.csv')
plots.burn$PlotID <- gsub('-', '', plots.burn$PlotID)


# first clean up spp pool 

# not sure which spp name the impatiens were supposed to go with so gonna hold off on running this till I do 
# dat$Impatiens_ <- dat$Impatiens_capensis + dat$Impatiens_pallida
# dat$Impatiens_capensis <- NULL 
# dat$Impatiens_pallida <- NULL

# first grab only plots in East Woods 
ew_plots <- plots.env$PlotID[which(plots.env$AreaName== 'East Woods')]
dat_ew <- dat[ew_plots,]
# then only look at which plots have more than 3 species present in them
dat_ew <- dat_ew[which(rowSums(dat_ew) > 3),]

# these are likely wetland plots.. were showing up as outliers on the ordination
dat_ew <- dat_ew[which(!row.names(dat_ew) %in% c('R79', 'LL123')),]

ord_ew <- metaMDS(dat_ew)
plot(ord_ew, type = 'n')
points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
#text(ord_ew, display = 'sites', cex = 0.6)
title('East Woods Plots with > 3 species')

# burned plots 

burned <- plots.burn$PlotID[which(!is.na(plots.burn$Burn_Date))]
unburned <- plots.burn$PlotID[which(is.na(plots.burn$Burn_Date))]

burn_plots <- data.frame(ord_ew$points)
burn_plots$burn_stat <- ifelse(row.names(burn_plots) %in% burned, 'Burned', 'Unburned')
burn_plots <- burn_plots[,-c(1, 2)]

plot(ord_ew, type = 'n')
points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
#text(ord_ew, display = 'sites', cex = 0.6)
title('East Woods Plots with > 3 species Burned and Unburned')
ordiellipse(ord_ew, burn_plots$burn_stat, col = c('black', 'blue'), lwd = 2, label= FALSE)
ordispider(ord_ew, burn_plots$burn_stat, col = c('black', 'blue'), label = FALSE)
legend('topleft', legend = unique(burn_plots$burn_stat), col = c('black', 'blue'), pch = 21, pt.bg = c('black', 'blue'), cex = 0.6)

burn_plots$ComClass <- plots.env$ComClass[which(plots.env$PlotID %in% row.names(burn_plots))]

colvec = c('yellow', 'white', 'black', 'red', 'blue3', '' )

plot(ord_ew, type = 'n')
points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = 'red', bg = 'yellow')
#text(ord_ew, display = 'sites', cex = 0.6)
title('East Woods Plots with > 3 species ComClasses')
ordiellipse(ord_ew, burn_plots$ComClass, col = colvec[unique(burn_plots$ComClass)], label = TRUE)
#ordispider(ord_ew, burn_plots$ComClass, label = FALSE) 
legend("topleft", legend = unique(burn_plots$ComClass), col = colvec[unique(burn_plots$ComClass)], 
       pt.bg = colvec[unique(burn_plots$ComClass)], bty = 'n', pch = 21, cex = 0.8)


# woodland plots use : 'Mesic woodland', 'Dry mesic woodland, Mesic-wet mesic forest, Mesic woodland' , 'Dry mesic woodland, Mesic-wet mesic forest, Savanna' ,
# 'Mesic-wet mesic forest, Mesic-wet mesic shrubland' ,  'Mesic-wet mesic shrubland, Mesic savanna, Mesic woodland' ,  'Mesic-wet mesic woodland, Mesic woodland' , 
#  'Savanna'

# grassland plots: 'Mesic savanna, Mesic woodland' ,  'Mesic-wet mesic shrubland' , 'Wet mesic prairie and savanna' , 




