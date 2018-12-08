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
setwd('~/Documents/GitHub/east_woods_work/')
dat.all <- read.csv('data/dat.all.csv')

#-----------------------------------------------------------------------------------------------------------
# FUNCTIONS 

# both of these i am just setting up to take dat.all, not individually masked dataframes like before

get_BA <- function(dat){
  BA <- c() #calculate the basal area (just the area of the trunk)
  for (c in dat$cover){
      c <- c * 0.0328084 #converting the cm DBH into feet  (so the final area is in sq feet)
    basal_area <- pi * (c/2)**2
    BA <- c(BA, basal_area)
  }
  dat$cover <- BA # im replacing the cover column so I can use the other function on it (wouldnt work if I 
                  # renamed BA)
  return(dat)
  
}


get_plot_cover <- function(dat){
  dat <- dat[which(!is.na(dat$cover)),]
  dat <- dat[which(dat$cover != 0),]
  total_cover <- 
    data.frame(
      cov = sapply(unique(dat$plot), function(x){
        sum(dat$cover[which(dat$plot == x)], na.rm = T)
      }
    )
  )
  total_cover$cov <- as.numeric(as.character(total_cover$cov))
  return(total_cover)
  # dat$plot_cover <- total_cover$cov[match(dat$plot, row.names(total_cover))]
  # dat$spp_percent_total_cover <- dat$cover/dat$plot_cover
  # return(dat)
}

#-----------------------------------------------------------------------------------------------------------

# FUNCTION CALL 

dat.all$cover <- as.numeric(dat.all$cover)


dat.07 <- dat.all[which(dat.all$year == '2007'),]
dat.18 <- dat.all[which(dat.all$year == '2018'),]

# Canopy/Tree level 

trees07 <- dat.07[which(dat.07$datset == 'T'),]
trees18 <- dat.18[which(dat.07$datset == 'T'),]
trees07 <- get_BA(trees07)
trees18 <- get_BA(trees18)
test <- get_plot_cover(trees07)

#trees07 <- get_plot_cover(trees07)
#trees18 <- get_plot_cover(trees18)

# trees07 <- trees07[order(trees07$plot),]
# trees18 <- trees18[order(trees18$plot),]
# Understory

understory07 <- dat.07[which(dat.07$datset == 'H'),]
understory07 <- rbind(understory07, dat.07[which(dat.07$datset == 'S'),])

understory18 <- dat.18[which(dat.18$datset == 'H'),]
understory18 <- dat.18[which(dat.18$datset == 'S'),]

understory07 <- get_plot_cover(understory07)
understory18 <- get_plot_cover(understory18)

understory07 <- understory07[order(understory07$plot),]
understory18 <- understory18[order(understory18$plot),]

# # write out 
# 
# trees.all <- rbind(trees07, trees18)
# understory.all <- rbind(understory07, understory18)
# 
# write.csv(trees.all, 'data/trees.all.csv')
# write.csv(understory.all, 'data/understory.all.csv')
# 
# rm(list = ls())
# 

