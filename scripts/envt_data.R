library(ggplot2)
# Get all the environmental data/plot metadata processed in one place


plots.env <- read.csv('/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Rollinson/data_processed/point_info_GIS.csv', row.names = 7)
row.names(plots.env) <- gsub('-', '', row.names(plots.env))

#---------------------------------------
# Burning/Management 

plots.env$MgmtUnit <- ifelse(is.na(plots.env$wooded), "Non-Wooded",
                             ifelse(plots.env$wooded=="Hidden Lake", "Hidden Lake",
                                    ifelse(plots.env$unit=="South 40 South", "Annual Burn",
                                           ifelse(!is.na(plots.env$unit), "Mixed Management", "No Management"))))


#-----------------------------------------
# Dominant tree genus

dat.all <- read.csv('data/species/dat.all.csv', as.is = T)
tree07 <- dat.all[which(dat.all$datset == 'T' & dat.all$year == '2007'),]
tree18 <- dat.all[which(dat.all$datset == 'T' & dat.all$year == '2018'),]


 
source('scripts/cover.R')   

tree_type_plots_18 <- get_dominant_tree_group(tree18)
plots.env$DomGenus18 <- tree_type_plots_18$tree_type[match(row.names(plots.env), tree_type_plots_18$unique.tree.dat.plot.)]
tree_type_plots_07 <- get_dominant_tree_group(tree07)
plots.env$DomGenus07 <- tree_type_plots_07$tree_type[match(row.names(plots.env), tree_type_plots_07$unique.tree.dat.plot.)]


#------------------------------------------
# Oak or Acer plot? 
# since there are a lot of different genuses look only at oak and maple for dominant classes and less common ones all grouped as 'other'


oak_hickory <- c('Quercus', 'Fraxinus', 'Carya', 'Cornus', 'Crataegus', 'Lirodendron')
beech_maple <- c('Acer', 'Ulmus', 'Tilia', 'Prunus')
plots.env$ForType18 <- ifelse(plots.env$DomGenus18 %in% oak_hickory, "Oak-Hickory",
                              ifelse(plots.env$DomGenus18 %in% beech_maple, "Beech-Maple",
                                     (ifelse(is.na(plots.env$DomGenus18), 'Other', 'Other'))))

plots.env$ForType07 <- ifelse(plots.env$DomGenus07 %in% oak_hickory, "Oak-Hickory",
                             ifelse(plots.env$DomGenus07 %in% beech_maple, "Beech-Maple",
                                    (ifelse(is.na(plots.env$DomGenus07), 'Other', 'Other'))))

plots.env$DomGenus18 <- as.character(plots.env$DomGenus18)
plots.env$DomGenus18[is.na(plots.env$DomGenus18)] <- 'No trees'
plots.env$DomGenus07 <- as.character(plots.env$DomGenus07)
plots.env$DomGenus07[is.na(plots.env$DomGenus07)] <- 'No trees'

#-----------------------------------------
# Invasive removal? 
#-----------------------------------------
# Invasive presence 

# this is set up to go but I really dont think I can use this abundance data will add it to the dataframe anyways and try it out maybe? 

understory18 <- dat.all[which(!dat.all$accepted_name %in% tree18$accepted_name & dat.all$year == '2018' & dat.all$sample_period == 'SUM'),]
understory07 <- dat.all[which(!dat.all$accepted_name %in% tree07$accepted_name & dat.all$year == '2007' & dat.all$sample_period == 'SUM'),]

InvCover18 <- get_invasive_cover(plots.env, understory18)
InvCover07 <- get_invasive_cover(plots.env, understory07)

save(plots.env, file = 'data/plots.env.RData')

# writing a new local copy because I want to be able to look at the trees groups in QGIS
write.csv(plots.env, '~/Documents/GitHub/east_woods_work/data/plots.env.csv')


theme_set(theme_minimal()) 
ggplot(plots.env, aes(x = lon, y = lat, col = plots.env$ForType18, fill = plots.env$ForType07))+
  geom_point(shape = 21, size = 3) +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size () +
  coord_equal() +
  ggtitle('Forest Type Changes between 2007 and 2018')


 
