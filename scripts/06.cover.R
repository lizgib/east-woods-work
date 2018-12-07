#make my dataframe w plot, spp, accname, and cover
# ok so the gameplan here is that i will keep the cover of each veg type as a separate unit 
# each veg type will amount to 100% for each plot (so the sum of all plant %cover should = 100%, 
# the sum of all basal areas of trees in a plot = 100%, and the total number of stems for shrubs
# = 100%)
# each plot will have a total cover of 3 
# I am not going to be comparing the relative quantities of trees to shrubs to herbs 
# I am comparing the composition of shrubs, herbs, trees, between plots
#-----------------------------------------------------------------------------------------------------------
# DATA 

dat.all <- read.csv('data/dat.all.csv')

#-----------------------------------------------------------------------------------------------------------
# FUNCTIONS 

# both of these i am just setting up to take dat.all, not individually masked dataframes like before

get_BA <- function(dat){
  BA <- c() #calculate the basal area (just the area of the trunk)
  for (c in dat$cover[which(dat$datset == 'T')]){
    c <- c * 0.0328084 #converting the cm DBH into feet  (so the final area is in sq feet)
    basal_area <- pi * (c/2)**2
    BA <- c(BA, basal_area)
  }
  dat$cover <- BA # rewriting the cover column as BA so it works with the get_plot_cover function
  return(dat)
}

get_plot_cover <- function(dat){
  dat <- dat[which(!is.na(dat$cover)),]
  dat <- dat[which(dat$cover != 0),]
  total_cover <- 
    data.frame(
      cov = sapply(unique(dat$plot), function(x){
        sum(dat$cover[dat$plot == x], na.rm = T)
      }
      )
    )
  total_cover$cov <- as.numeric(as.character(total_cover$cov))
  dat$plot_herb_cover <- total_cover$cov[match(dat$plot, row.names(total_cover))]
  dat$spp_percent_total_herb_cover <- dat$cover/dat$plot_herb_cover
  return(dat)
}

#-----------------------------------------------------------------------------------------------------------

# FUNCTION CALL 

dat.all$cover <- as.numeric(dat.all$cover)
dat.all$cover <- dat.all$cover[which(dat.all$datset == 'T'),]

dat.07 <- dat.all[which(dat.all$year == '2007'),]
dat.18 <- dat.all[which(dat.all$year == '2018'),]






herbs07 <- dat.07[which(dat.07$datset == 'H'),]
herbs18 <- dat.18[which(dat.18$datset == 'H'),]

shrubs07 <- dat.07[which(dat.07$datset == 'S'),]
shrubs18 <- dat.18[which(dat.18$datset == 'S'),]

trees07 <- dat.07[which(dat.07$datset == 'T'),]
trees18 <- dat.18[which(dat.18$datset == 'T'),]

invasives07 <- dat.07[which(dat.07$nativestatus == 'i'),]
invasives07 <- rbind(invasives07, dat.07[which(dat.07$nativestatus == 'x'),])
invasives18 <- dat.18[which(dat.18$nativestatus == 'i'),]
invasives18 <- rbind(invasives18, dat.18[which(dat.18$nativestatus == 'x'),])

herbs07 <- get_plot_cover(herbs07)
herbs18 <- get_plot_cover(herbs18)

shrubs07 <- get_plot_cover(shrubs07)
shrubs18 <- get_plot_cover(shrubs18)

trees07 <- get_BA(trees07)
trees07 <- get_plot_cover(trees07)
trees18 <- get_BA(trees18)
trees18 <- get_plot_cover(trees18)

invasives07 <- get_plot_cover(invasives07)
invasives18 <- get_plot_cover(invasives18)


# 12/6
# not sure how i want to go about this... 
# ultimately I want to replace the original cover values I got from my original data
# with these updated cover....





