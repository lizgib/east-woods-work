library(Biobase)
library(methClust)
library(CountClust)
library(maptpx)
library(ecostructure)
library(sf)
library(RColorBrewer)
#-------------
# NOTE: 6/13 if you are having problems loading look at the note written in the ecostructure github folder!!

# load in site metadata 
data('plots.env')
my_site_metadata <- plots.env[which(plots.env$wooded == 'East Woods'),]
my_site_metadata <- na.omit(my_site_metadata)


# load in the community matrix
dat.07 <- read.csv('~/Documents/GitHub/east_woods_work/data/Community_Matrix/2007/dat.mat.all.07.csv', row.names = 1)
dat.18 <- read.csv('~/Documents/GitHub/east_woods_work/data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)

# BB115 is not in either community matrix 
my_site_metadata <- my_site_metadata[which(rownames(my_site_metadata) != 'BB115'),] 

# blocker and order variables 
mgmt_blocks <- factor(my_site_metadata$MgmtUnit, levels = c("Mixed Management", "Annual Burn"))
fortype07 <- factor(my_site_metadata$ForType07, levels = c('Other', 'Beech-Maple', 'Oak-Hickory'))
domgenus07 <- as.factor(my_site_metadata$DomGenus07)
fortype18 <- factor(my_site_metadata$ForType18, levels = c('Other', 'Beech-Maple', 'Oak-Hickory'))
domgenus18 <- as.factor(my_site_metadata$DomGenus18)

elevation <- as.numeric(my_site_metadata$elev)
plot_ID <- as.numeric(my_site_metadata$point.ID)
aspect <- as.numeric(my_site_metadata$aspect)
slope <- as.numeric(my_site_metadata$slope)
longitude <- as.numeric(my_site_metadata$lon)
latitude <- as.numeric(my_site_metadata$lat)

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colvec = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
myColors = colvec[1:length(unique(my_site_metadata$DomGenus18))]


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
k2_2007 <- run_model(dat.07, my_site_metadata, 2)
k2_2018 <- run_model(dat.18, my_site_metadata, 2)
k3_2007 <- run_model(dat.07, my_site_metadata, 3)
k3_2018 <- run_model(dat.18, my_site_metadata, 3)


# extract predictor species 
extract_top_spp(k2_2007, 'figures/Summer 2019/EcoStructure/PredictorSpp/k2_2007.csv')
extract_top_spp(k2_2018, 'figures/Summer 2019/EcoStructure/PredictorSpp/k2_2018.csv')
extract_top_spp(k3_2007, 'figures/Summer 2019/EcoStructure/PredictorSpp/k3_2007.csv')
extract_top_spp(k3_2018, 'figures/Summer 2019/EcoStructure/PredictorSpp/k3_2018.csv')


# plot map of population 
latmax = max(my_site_metadata$lat) + 0.05
latmin = min(my_site_metadata$lat) - 0.05
lonmax = max(my_site_metadata$lon) + 0.05
#lonmin = min(my_site_metadata$lon) - 0.05
lonmin = -88.060

cord_mat <- my_site_metadata[c('lon', 'lat')]
cord_mat <- cord_mat[sort(row.names(cord_mat)),]

ecos_plot_pie(omega = k2_2007$omega,
              coords = cord_mat,
              bgmap_path = '~/Desktop/WGS_MortArb_boundary_line.shp',
              long_lim = c(lonmin, lonmax), 
              lat_lim = c(latmin, latmax),
              radius = 0.0002,
              image_width = 1500, 
              image_height = 1000,
              path = 'figures/Summer 2019/EcoStructure/GeoStructurePlots/2007/geostrc_k2_2007.png')

ecos_plot_pie(omega = k2_2018$omega,
              coords = cord_mat,
              bgmap_path = '~/Desktop/WGS_MortArb_boundary_line.shp',
              long_lim = c(lonmin, lonmax), 
              lat_lim = c(latmin, latmax),
              radius = 0.0002,
              image_width = 1500, 
              image_height = 1000,
              path = 'figures/Summer 2019/EcoStructure/GeoStructurePlots/2018/geostrc_k2_2018.png')

ecos_plot_pie(omega = k3_2007$omega,
              coords = cord_mat,
              bgmap_path = '~/Desktop/WGS_MortArb_boundary_line.shp',
              long_lim = c(lonmin, lonmax), 
              lat_lim = c(latmin, latmax),
              radius = 0.0002,
              image_width = 1500, 
              image_height = 1000,
              path = 'figures/Summer 2019/EcoStructure/GeoStructurePlots/2007/geostrc_k3_2007.png')

ecos_plot_pie(omega = k3_2018$omega,
              coords = cord_mat,
              bgmap_path = '~/Desktop/WGS_MortArb_boundary_line.shp',
              long_lim = c(lonmin, lonmax), 
              lat_lim = c(latmin, latmax),
              radius = 0.0002,
              image_width = 1500, 
              image_height = 1000,
              path = 'figures/Summer 2019/EcoStructure/GeoStructurePlots/2007/k3_2018.png')

# structure plots (does envt explain structure?)

fit = k3_2018
blocker_var = mgmt_blocks
blocker_name = 'Management Effects 2018'
order_var = elevation
order_name = 'Elevation'
blocks_outfile = 'figures/Summer 2019/EcoStructure/StructurePlots/2018/MgmtUnit/k3_2018_elevation.png'
source('~/Documents/GitHub/ecostructure/R/ecos_blocks.R') # I had to modify this because the plotting was not working for me :(
png(blocks_outfile, width = 1000, height = 1600)
ecos_blocks(fit$omega,
            blocker_metadata = blocker_var,
            order_metadata = order_var, 
            structure_control = list(split_line=list(split_lwd = 1,
                                                     split_col = "white"),
                                     axis_tick = list(axis_ticks_length = .1,
                                                      axis_ticks_lwd_y = .1,
                                                      axis_ticks_lwd_x = .1,
                                                      axis_label_size = 15,
                                                      axis_label_face = "bold"),
                                     plot_labels = TRUE,
                                     levels_decreasing=FALSE,
                                     order_sample=F,
                                     round_off=1,
                                     panel_title_size=23,
                                     panel_title_font=4,
                                     main_title= blocker_name,
                                     main_title_size = 20,
                                     yaxis_label = order_name))
dev.off()


# save population membership to file 

write.csv(as.data.frame(k2_2007$omega), 'figures/Summer 2019/EcoStructure/Omega/k2_2007.csv', quote = F)
write.csv(as.data.frame(k3_2007$omega), 'figures/Summer 2019/EcoStructure/Omega/k3_2007.csv', quote = F)
write.csv(as.data.frame(k2_2018$omega), 'figures/Summer 2019/EcoStructure/Omega/k2_2018.csv', quote = F)
write.csv(as.data.frame(k3_2018$omega), 'figures/Summer 2019/EcoStructure/Omega/k3_2018.csv', quote = F)

