
dat.all <- read.csv('data/species/dat.all.csv')
dat.all$species <- as.factor(dat.all$species)
dat.all$accepted_name <- as.factor(dat.all$accepted_name)
dat.07 <- dat.all[which(dat.all$year == '2007'),]
dat.18 <- dat.all[which(dat.all$year == '2018'),]

spp07_cleaned <- sort(unique(dat.07$accepted_name)) # get unique sorted names from tnrs 
spp18_cleaned <- sort(unique(dat.18$accepted_name)) # for each year/group
allspp_cleaned <- sort(unique(dat.all$accepted_name))

spp07_uncleaned <- sort(unique(dat.07$species)) # do the same for the uncleaned names 
spp18_uncleaned <- sort(unique(dat.18$species))
allspp_uncleaned <- sort(unique(dat.all$species))

length(spp07_cleaned)   # 267 species in 2007 (6/18)
length(spp18_cleaned)   # 408 species in 2018 (6/18) this seems wayy too high by comparison
length(allspp_cleaned)  # 456 unique species reported over 2007 and 2018

length(spp07_uncleaned)   # 313 species reported in 2007 
length(spp18_uncleaned)   # 436 species reported in 2018
length(allspp_uncleaned)  # 582 species between years               

#---------------------------------------------------------------------------------------------------------


# raw name  # cleaned name # num plots 2007 # num plots 2018 

new_dat <- data.frame(allspp_uncleaned)
tnrs_all <- data.frame(read.csv('data/species/tnrs_results.csv', as.is =T))
new_dat$cleaned_name <- tnrs_all$Accepted_name[match(allspp_uncleaned, tnrs_all$Name_submitted)]

num_plots_07 <- c()
for (spp in new_dat$allspp_uncleaned){
  plots <- c()
  plots <- c(plots, unique(dat.07$plot[which(dat.07$species == spp)]))
  num_plots_07 <- c(num_plots_07, length(plots))
}
new_dat$num_plots_07 <- num_plots_07


num_plots_18 <- c()
for (spp in new_dat$allspp_uncleaned){
  plots <- c()
  plots <- c(plots, unique(dat.07$plot[which(dat.18$species == spp)]))
  num_plots_18 <- c(num_plots_18, length(plots))
}
new_dat$num_plots_18 <- num_plots_18

spp_counts_07 <- as.data.frame(table(dat.07$accepted_name))
spp_counts_18 <- as.data.frame(table(dat.18$accepted_name))

new_dat$spp_counts_07  <- spp_counts_07$Freq[match(new_dat$cleaned_name, spp_counts_07$Var1)]
new_dat$spp_counts_18  <- spp_counts_18$Freq[match(new_dat$cleaned_name, spp_counts_18$Var1)]


# Raw name -- Cleaned name from TNRS -- 2007 total plots -- 2018 total plots
names(new_dat) <- c('Raw_name', 'Cleaned_name', '2007_total_plots', '2018_total_plots', '2007_total_species', '2018_total_species')

write.csv(new_dat, 'data/species/plot_species_record.csv', row.names = F, quote = F)
#rm(list = ls())


