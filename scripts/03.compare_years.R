source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/15.falltranslationkey.R')

spp07_cleaned <- sort(unique(dat.07$accepted_name))
spp18_cleaned <- sort(unique(dat.18$accepted_name))
allspp_cleaned <- sort(unique(dat.all$accepted_name))

spp07_uncleaned <- sort(unique(dat.07$species))
spp18_uncleaned <- sort(unique(dat.18$species))
allspp_uncleaned <- sort(unique(dat.all$species))

length(spp07_cleaned)   # 285 species reported in 2007
length(spp18_cleaned)   # 346 species reported in 2018 
length(allspp_cleaned)  # 437 species between years 

length(spp07_uncleaned)   # 318 species reported in 2007
length(spp18_uncleaned)   # 442 species reported in 2018 
length(allspp_uncleaned)  # 598 species between years 

#####################################################################################################

# CLEANED 

both_07_and_18 <- sort(intersect(spp07_cleaned, spp18_cleaned))  # 194 species in both 2007 and 2018
in_07_not_18 <- sort(setdiff(spp07_cleaned, spp18_cleaned))  # 91 species found only in 2007 
in_18_not_07 <- sort(setdiff(spp18_cleaned, spp07_cleaned))  # 152 species found only in 2018

both <- c()
only_07 <- c()
only_18 <- c()
whatschanged <- data.frame(allspp_cleaned) 
for (sp in whatschanged$allspp){
  ifelse(sp %in% both_07_and_18, 
         both <-c(both, 1), both <- c(both, 0))
  ifelse(sp %in% in_07_not_18, 
         only_07 <- c(only_07, 1), only_07 <- c(only_07, 0))
  ifelse(sp %in% in_18_not_07, 
         only_18 <- c(only_18, 1), only_18 <- c(only_18, 0))
}

whatschanged$both_07_and_18 <- both
whatschanged$only_in_07 <- only_07
whatschanged$only_in_18 <- only_18

spp_counts_07 <- data.frame(table(dat.07$accepted_name))
spp_counts_18 <- data.frame(table(dat.18$accepted_name))

whatschanged$freq_07  <- spp_counts_07$Freq[match(whatschanged$allspp_cleaned, spp_counts_07$Var1)]
whatschanged$freq_18  <- spp_counts_18$Freq[match(whatschanged$allspp_cleaned, spp_counts_18$Var1)]

write.csv(whatschanged, 'tnrs.spp_compare_years.csv')

#####################################################################################################

# UNCLEANED 

both_07_and_18 <- sort(intersect(spp07_uncleaned, spp18_uncleaned))  
in_07_not_18 <- sort(setdiff(spp07_uncleaned, spp18_uncleaned))  
in_18_not_07 <- sort(setdiff(spp18_uncleaned, spp07_uncleaned))  

both <- c()
only_07 <- c()
only_18 <- c()
whatschanged <- data.frame(allspp_uncleaned) 
for (sp in whatschanged$allspp){
  ifelse(sp %in% both_07_and_18, 
         both <-c(both, 1), both <- c(both, 0))
  ifelse(sp %in% in_07_not_18, 
         only_07 <- c(only_07, 1), only_07 <- c(only_07, 0))
  ifelse(sp %in% in_18_not_07, 
         only_18 <- c(only_18, 1), only_18 <- c(only_18, 0))
}

whatschanged$both_07_and_18 <- both
whatschanged$only_in_07 <- only_07
whatschanged$only_in_18 <- only_18

spp_counts_07 <- data.frame(table(dat.07$species))
spp_counts_18 <- data.frame(table(dat.18$species))

whatschanged$freq_07  <- spp_counts_07$Freq[match(whatschanged$allspp_uncleaned, spp_counts_07$Var1)]
whatschanged$freq_18  <- spp_counts_18$Freq[match(whatschanged$allspp_uncleaned, spp_counts_18$Var1)]

write.csv(whatschanged, 'uncleaned.spp_compare_years.csv')


######################################################################################################


# raw name  # cleaned name # num plots 2007 # num plots 2018 

new_dat <- data.frame(allspp_uncleaned)

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

# Raw name -- Cleaned name from TNRS -- 2007 total plots -- 2018 total plots
names(new_dat) <- c('Raw_name', 'Cleaned_name_from_TNRS', '2007_total_plots', '2018_total_plots')

write.csv(new_dat, '../OUTPUTS/plot_species_record.csv')
