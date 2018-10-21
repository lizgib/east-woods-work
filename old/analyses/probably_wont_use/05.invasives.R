#invasive spp 

#first need to evaluate which plots have invasive spp and how many (want the cover data for this)

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
invasives <- read.csv('OUTPUTS/processed_invasives_list.csv')[2]
all_spp_18 <- read.csv('../data_processing/OUTPUTS/cover_18.csv')
all_spp_07 <- read.csv('../data_processing/OUTPUTS/cover_07.csv')

all_spp_18$accepted_name <- gsub(' ', '_', all_spp_18$accepted_name) #make acc names and
all_spp_07$accepted_name <- gsub(' ', '_', all_spp_07$accepted_name) #plot ids match each other
all_spp_07$plot <- gsub('-', '', all_spp_07$plot)

affected_plots_18 <- NULL
for (spp in invasives$x){
  affected_plots_18 <- rbind(affected_plots_18, all_spp_18[which(spp == all_spp_18$accepted_name),])
}
#total number affected plots 18 == 265

affected_plots_07 <- NULL
for (spp in invasives$x){
  affected_plots_07 <- rbind(affected_plots_07, all_spp_07[which(spp == all_spp_07$accepted_name),])
}
#total number affected plots 07 == 384

#once we know which plots have invasives, determine how these have changed from 2007 
#both in terms of presence/abs but particularly just cover changes 
affected_both_years <- intersect(affected_plots_07$plot, affected_plots_18$plot)
newdf <- NULL
for (plot in affected_both_years){
  newdf <- rbind(newdf, all_spp_07[which(plot == all_spp_07$plot),])
} #this has given me a dataframe with all the plot data (invasive and native) for all affected plots in 18

newdf$cover <- as.numeric(as.character(newdf$cover))
newdf$cover18 <- all_spp_18$cover[which(newdf$plot %in% all_spp_18$plot)] #this adds the cover 
                                                                              # data for all the plots in 
  #just realized...i dont think i account for newly affected plots in 18..    # 2018 that are still affected
                                                                              # by invasives (this does not include newly affected plots)
#gotta take care of those missing plots.. 
missing_18_plots <- data.frame(affected_plots_18[which(!affected_plots_18$plot %in% newdf$plot),'plot'])
names(missing_18_plots) <- 'plot'
cover18 <- c()
for(plt in missing_18_plots$plot){ ####OKKKKK so at 9:11 on 8/1 im just trying to make sure i have all plots that are affected by invasives being considered (im missing the newly affected plots trying to add them in)
  cover18 <- c(cover18, affected_plots_18[which(affected_plots_18$plot == plt),'cover'])
}

newdf <- na.omit(newdf) # remove any rows that there is no cover in 07 or 18 (for whatever reason)
newdf$change_in_invasives <- newdf$cover - newdf$cover18

# plots <- data.frame(unique(newdf$plot))     #this is cool but i think i want something different...
# cover <- c()                                #this gives me the sum of all the changes in invasives which
# for(plt in plots$unique.newdf.plot.){       #im not sure is meaningful like i think it is
#   invasive_cover <- sum(newdf$change_in_invasives[which(newdf$plot == plt)])
#   cover <- c(cover, invasive_cover)
# }
# plots$total_invasive_cover <- cover

plots <- data.frame(unique(newdf$plot))
cover <- c()
for(plt in plots$unique.newdf.plot.){
  invasive_cover <- sum(newdf$cover18[which(newdf$plot == plt)])
  cover <- c(cover, invasive_cover)
}
plots$total_invasive_cover <- cover

n_cover <- c()
all_plots <- data.frame(unique(all_spp_18$plot))
for(plt in all_plots$unique.all_spp_18.plot.){
  total_cover <- sum(all_spp_18$cover[which(all_spp_18$plot == plt)])
  n_cover <- c(n_cover, total_cover)
}
all_plots$total_covered_area <- n_cover

newcol <- c()
for(plt in all_plots$unique.all_spp_18.plot.){
  ifelse(plt %in% plots$unique.newdf.plot.,
    newcol <- c(newcol, plots$total_invasive_cover[which(plots$unique.newdf.plot. ==plt)]),
    newcol <- c(newcol, 0))
}
all_plots$invasive_cover <- newcol
all_plots$native_invasive_ratio <- all_plots$invasive_cover/(all_plots$total_covered_area) 


