library(Biobase)
library(methClust)
library(CountClust)
library(maptpx)
library(ecostructure)
library(sf)
library(RColorBrewer)

args = commandArgs(TRUE)
commat07 <- args[1]
commat18 <- args[2]
k = args[3]
shpfile = args[4]

# commat07 <- 'dat.mat.all.07'
# commat18 <- 'dat.mat.all.18'
# k = 3
# shpfile = '~/Documents/GIS/WGS/WGS_merged_roads.shp'

filepath_07 <- paste('data/Community_Matrix/2007/', commat07, '.csv', sep = '')
filepath_18 <- paste('data/Community_Matrix/2018/', commat18, '.csv', sep = '')

print(filepath_07)
print(filepath_18)

#-------------
# NOTE: if you are having problems loading look at the note written in the ecostructure github folder!!

# load in site metadata 
data('plots.env')
#my_site_metadata <- plots.env[which(plots.env$wooded == 'East Woods'),]
#my_site_metadata <- na.omit(plots.env)
my_site_metadata <- plots.env

# load in the community matrix
dat.07 <- read.csv(filepath_07, row.names = 1)
dat.18 <- read.csv(filepath_18, row.names = 1)


# blocker and order variables 
#-----
mgmt_blocks <- factor(my_site_metadata$MgmtUnit, levels = c("Mixed Management", "Annual Burn"))
fortype07 <- factor(my_site_metadata$ForType07, levels = c('Other', 'Beech-Maple', 'Oak-Hickory'))
domgenus07 <- as.factor(my_site_metadata$DomGenus07)
fortype18 <- factor(my_site_metadata$ForType18, levels = c('Other', 'Beech-Maple', 'Oak-Hickory'))
domgenus18 <- as.factor(my_site_metadata$DomGenus18)
drainage <- as.factor(my_site_metadata$Drainage)
texture <- as.factor(my_site_metadata$texture)
consis <- as.factor(my_site_metadata$consistenc)
permeability <- as.factor(my_site_metadata$Permeabili)
wtrhdlgcap <- as.factor(my_site_metadata$wtrhldgcap)
wtrtbl <- as.factor(my_site_metadata$watertbl)
orgmatter <- as.factor(my_site_metadata$orgmatter)
rootdep <- as.factor(my_site_metadata$rootdevdep)

elevation <- as.numeric(my_site_metadata$elev)
plot_ID <- as.numeric(my_site_metadata$point.ID)
aspect <- as.numeric(my_site_metadata$aspect)
slope <- as.numeric(my_site_metadata$slope)
longitude <- as.numeric(my_site_metadata$lon)
latitude <- as.numeric(my_site_metadata$lat)
burn_count <- as.numeric(my_site_metadata$BurnCount)
pd07 <- as.numeric(my_site_metadata$PD07)
pd18 <- as.numeric(my_site_metadata$PD18)
treecover07 <- as.numeric(my_site_metadata$TreeCover07)
treecover18 <- as.numeric(my_site_metadata$TreeCover18)
tpi <- as.numeric(my_site_metadata$tpi)

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colvec = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
myColors = colvec[1:length(unique(my_site_metadata$DomGenus18))]
#-----

run_model <- function(dat, my_site_metadata, k){
  my_species_counts <- as.matrix(dat[which(rownames(dat) %in% rownames(my_site_metadata)),])
  fit <- ecos_fit(my_species_counts, K = as.numeric(k), tol = 0.1, num_trials = 10)
  return(fit)
}  

extract_top_spp <- function(fit, fname){
  features <- CountClust::ExtractTopFeatures(fit$theta, top_features = 10,
                                             method = "poisson", options = "max")
  top_predictor_spp <- t(apply(features$indices, c(1,2), function(x) return(rownames(fit$theta)[x])))
  write.csv(top_predictor_spp, fname, row.names = F, quote = F)
}


# fit for given k 
fit07 <- run_model(dat.07, my_site_metadata, k)
fit18 <- run_model(dat.18, my_site_metadata, k)

# extract predictor species 
predspp_outfile07 = paste('figures/Summer 2019/EcoStructure/PredictorSpp/2007/k', k, '_', commat07, '.csv', sep = '')
predspp_outfile18 = paste('figures/Summer 2019/EcoStructure/PredictorSpp/2018/k', k, '_', commat18, '.csv', sep = '')

extract_top_spp(fit07, predspp_outfile07)
extract_top_spp(fit18, predspp_outfile18)

# plot map of population 
latmax = max(my_site_metadata$lat) + 1
latmin = min(my_site_metadata$lat) - 1
lonmax = max(my_site_metadata$lon) + 1
#lonmin = min(my_site_metadata$lon) - 1
lonmin = -88.063

cord_mat <- my_site_metadata[c('lon', 'lat')]
cord_mat <- cord_mat[sort(row.names(cord_mat)),]
cord_mat07 <- cord_mat[which(rownames(cord_mat)%in%rownames(fit07$omega)),]
cord_mat18 <- cord_mat[which(rownames(cord_mat)%in%rownames(fit18$omega)),]

outfile07 <- gsub('.', '_', commat07, fixed = T)
outfile18 <- gsub('.', '_', commat18, fixed = T)

gstrc07_out <- paste('figures/Summer 2019/EcoStructure/GeoStructurePlots/2007/k', k, '_', outfile07, '.png', sep = '')
gstrc18_out <- paste('figures/Summer 2019/EcoStructure/GeoStructurePlots/2018/k', k, '_', outfile18, '.png', sep = '')

print(gstrc07_out)
print(gstrc18_out)

ecos_plot_pie(omega = fit07$omega,
              coords = cord_mat07,
              bgmap_path = shpfile,
              long_lim = c(lonmin, lonmax), 
              lat_lim = c(latmin, latmax),
              coastline_lwd = 2,
              radius = 0.0002,
              image_width = 1500, 
              image_height = 1500,
              path = gstrc07_out)

ecos_plot_pie(omega = fit18$omega,
              coords = cord_mat18,
              bgmap_path = shpfile,
              long_lim = c(lonmin, lonmax), 
              lat_lim = c(latmin, latmax),
              coastline_lwd = 2,
              radius = 0.0002,
              image_width = 1500, 
              image_height = 1500,
              path = gstrc18_out)


# structure plots (does envt explain composition?)
#-----
# fit = k3_2018
# blocker_var = orgmatter
# blocker_name = 'Organic Matter'
# order_var = elevation
# order_name = 'Elevation'
# blocks_outfile = 'figures/Summer 2019/EcoStructure/StructurePlots/2018/SoilClass/group1k3_2018_orgmat_wtrhld.png'
# source('~/Documents/GitHub/ecostructure/R/ecos_blocks.R') # I had to modify this because the plotting was not working for me :(
# png(blocks_outfile, width = 1000, height = 1600)
# ecos_blocks(fit$omega,
#             blocker_metadata = blocker_var,
#             order_metadata = order_var, 
#             structure_control = list(split_line=list(split_lwd = 1,
#                                                      split_col = "white"),
#                                      axis_tick = list(axis_ticks_length = .1,
#                                                       axis_ticks_lwd_y = .1,
#                                                       axis_ticks_lwd_x = .1,
#                                                       axis_label_size = 15,
#                                                       axis_label_face = "bold"),
#                                      plot_labels = TRUE,
#                                      levels_decreasing=FALSE,
#                                      order_sample=F,
#                                      round_off=4,
#                                      panel_title_size=23,
#                                      panel_title_font=4,
#                                      main_title= blocker_name,
#                                      main_title_size = 20,
#                                      yaxis_label = order_name))
# dev.off()
#------

# save population membership to file 

omega_out07 <- paste('figures/Summer 2019/EcoStructure/Omega/2007/k', k, outfile07, '.csv', sep = '')
omega_out18 <- paste('figures/Summer 2019/EcoStructure/Omega/2018/k', k, outfile18, '.csv', sep = '')

write.csv(as.data.frame(fit07$omega), omega_out07, quote = F)
write.csv(as.data.frame(fit18$omega), omega_out18, quote = F)

