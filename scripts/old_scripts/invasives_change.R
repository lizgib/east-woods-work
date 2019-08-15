# I really felt invasive species must be playing a role in the changing of 
# plot communities just based on the species present in them, but I haven't been able to 
# detect anything significant so far. Going to take a deeper look here 

library(ggplot2)
library(ggpubr)
theme_set(theme_minimal())
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data("plots.env")
dat.all <- read.csv('data/species/dat.all.csv') 
dat.07 <- dat.all[which(dat.all$year == '2007'),]
dat.18 <- dat.all[which(dat.all$year == '2018'),]

ALLPET_plots_07 <- dat.07$plot[grep('Alliaria petiolata', dat.07$accepted_name)]
ALLPET_plots_18 <- dat.18$plot[grep('Alliaria petiolata', dat.18$accepted_name)]

ggplot(data = plots.env[which(rownames(plots.env) %in% ALLPET_plots_07),], aes(x = lon, y = lat, col = ComClass07))+ 
  geom_point() + 
  coord_equal()

ggplot(data = plots.env[which(rownames(plots.env) %in% ALLPET_plots_18),], aes(x = lon, y = lat, col = ComClass18))+ 
  geom_point()+ 
  coord_equal()

new_ALLPET <- setdiff(ALLPET_plots_18, ALLPET_plots_07)
lost_ALLPET <- setdiff(ALLPET_plots_07, ALLPET_plots_18)

ggplot()+ 
  geom_point(data = plots.env[which(rownames(plots.env) %in% new_ALLPET),], aes(x = lon, y = lat, col = 'red')) +
  geom_point(data = plots.env[which(rownames(plots.env) %in% lost_ALLPET),], aes(x = lon, y = lat, col = 'blue')) + 
  coord_equal()


# -------

RHACAT_plots_07 <- dat.07$plot[grep('Rhamnus cathartica', dat.07$accepted_name)]
RHACAT_plots_18 <- dat.18$plot[grep('Rhamnus cathartica', dat.18$accepted_name)]
new_RHACAT <- setdiff(RHACAT_plots_18, RHACAT_plots_07)
lost_RHACAT <- setdiff(RHACAT_plots_07, RHACAT_plots_18)

ggplot()+ 
  geom_point(data = plots.env[which(rownames(plots.env) %in% new_RHACAT),], aes(x = lon, y = lat, col = 'red')) +
  geom_point(data = plots.env[which(rownames(plots.env) %in% lost_RHACAT),], aes(x = lon, y = lat, col = 'blue')) + 
  coord_equal()


# ----

LON_plots_07 <- dat.07$plot[grep('Lonicera', dat.07$accepted_name)]
LON_plots_18 <- dat.18$plot[grep('Lonicera', dat.18$accepted_name)]

new_LON <- setdiff(LON_plots_18, LON_plots_07)
lost_LON <- setdiff(LON_plots_07, LON_plots_18)

ggplot()+ 
  geom_point(data = plots.env[which(rownames(plots.env) %in% new_LON),], aes(x = lon, y = lat, col = 'red', shape = ComClass18)) +
  geom_point(data = plots.env[which(rownames(plots.env) %in% lost_LON),], aes(x = lon, y = lat, col = 'blue', shape = ComClass07)) + 
  coord_equal()



