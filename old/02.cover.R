#make my dataframe w plot, spp, accname, and cover
# ok so the gameplan here is that i will keep the cover of each veg type as a separate unit 
# each veg type will amount to 100% for each plot (so the sum of all plant %cover should = 100%, 
# the sum of all basal areas of trees in a plot = 100%, and the total number of stems for shrubs
# = 100%)
# each plot will have a total cover of 3 
# I am not going to be comparing the relative quantities of trees to shrubs to herbs 
# I am comparing the composition of shrubs, herbs, trees, between plots


setwd("~/Documents/morton arb/east_woods_phylogeny/data_processing/DATA")

#open up those csvs and put them in dataframes
dat.herb18 <- read.csv(file = "spp.herb.2018.csv", as.is = T)
dat.herb18 <- data.frame(dat.herb18[3:6])
dat.shrub18 <- read.csv(file = "spp.shrub.2018.csv", as.is = T)
dat.shrub18 <- data.frame(dat.shrub18[3:8])
dat.shrub18[4:5] <- NULL
dat.tree18 <- read.csv(file = "spp.trees.2018.csv", as.is = T)
dat.tree18 <- data.frame(dat.tree18[3:6])
dat.herb18$X6.Letter.Code<-NULL
dat.shrub18$X6.Letter.Code<-NULL
dat.tree18$X6.Letter.Code<-NULL
names(dat.herb18) <- c('plot', 'sp', 'cover')
names(dat.shrub18) <- c('plot', 'sp', 'cover')
names(dat.tree18) <- c('plot', 'sp', 'cover')
##########################################################################################################
herb_plots <- data.frame(unique(dat.herb18$plot)) #get all the herb plots
total_cover <- c()
for (plt in herb_plots$unique.dat.herb18.plot.){ #for each herb plot I find the sum of all the percent covers
  temp <- sum(dat.herb18$cover[which(dat.herb18$plot == plt)]) #this will be over 100% for some plots
  total_cover <- c(total_cover, temp) 
}
herb_plots$total_cover <- total_cover #I make a new dataframe that has the total cover for each plot 

dat.herb18 <- dat.herb18[which(!dat.herb18$cover == 0),] #I get rid of any rows that I won't be able to divide on
percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(cover in dat.herb18$cover){ #...each spp in the plot takes up
   ptc <- cover/herb_plots$total_cover[which(herb_plots$unique.dat.herb18.plot. == dat.herb18$plot[cover])] 
   percent_of_total_cover <- c(percent_of_total_cover, ptc)
} ####>>>>?>?>>?>?>? this is almost perfect but i have total covers over 100% which i dont want......<<<<<??
dat.herb18$percent_total_cover <- percent_of_total_cover #add this percent of total area to the original

veg_type <- c()
for(x in 1:nrow(dat.herb18)){
  veg_type <-c(veg_type, 'H')
}
dat.herb18$veg_type <- veg_type

##########################################################################################################
#adjust the trees to be in basal area 
dat.tree18$cover <- as.double(dat.tree18$cover) #for some reason my cover is a factor so convert so I can do math
dat.tree18 <- na.omit(dat.tree18) #get rid of the na's because they mess up the number of rows later
dat.tree18 <- dat.tree18[which(!dat.tree18$cover == 0),] #get rid of rows that have no trees reported
#plot area = 33.82 sq ft
BA <- c() #calculate the basal area (just the area of the trunk)
for (c in dat.tree18$cover){
  c <- c * 0.0328084 #converting the cm DBH into feet  (so the final area is in sq feet)
  basal_area <- pi * (c/2)**2
  BA <- c(BA, basal_area)
}
dat.tree18$cover <- BA #replace the DBH values with the basal area. we don't care about dbh anymore

tree_plots <- data.frame(unique(dat.tree18$plot)) #get all the tree plots
total_cover <- c() #doing the same thing i did for herbs, take all the basal areas in a plot and 
for (plt in tree_plots$unique.dat.tree18.plot.){ #... add them together so you have the total area 
  temp <- sum(dat.tree18$cover[which(dat.tree18$plot == plt)]) # .. that is occupied by trees
  total_cover <- c(total_cover, temp)
}
tree_plots$total_cover <- total_cover 

for(x in 1:length(dat.tree18$plot)){ #for each spp in the original data
  dat.tree18$percent_total_cover <- 
    dat.tree18$cover[x]/tree_plots$total_cover[which(tree_plots$unique.dat.tree18.plot. == dat.tree18$plot[x])]
  #divide the cover that species occupies by the total tree cover value for that plot 
}
veg_type <- c()
for(x in 1:nrow(dat.tree18)){
  veg_type <-c(veg_type, 'T')
}
dat.tree18$veg_type <- veg_type

##########################################################################################################
#adjust shrubs to be in percent cover 
shrub_plots <- data.frame(unique(dat.shrub18$plot)) #get all the shrub plots
total_cover <- c()
for (plt in shrub_plots$unique.dat.shrub18.plot.){ #for each shrub plot I find the sum of all the percent covers
  temp <- sum(dat.shrub18$cover[which(dat.shrub18$plot == plt)]) #this will be over 100% for some plots
  total_cover <- c(total_cover, temp) 
}
shrub_plots$total_cover <- total_cover #I make a new dataframe that has the total cover for each plot 

dat.shrub18 <- dat.shrub18[which(!dat.shrub18$cover == 0),] #I get rid of any rows that I won't be able to divide on
percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(cover in dat.shrub18$cover){ #...each spp in the plot takes up
  ptc <- cover/shrub_plots$total_cover[which(shrub_plots$unique.dat.shrub18.plot. == dat.shrub18$plot[cover])] 
  percent_of_total_cover <- c(percent_of_total_cover, ptc)
}
dat.shrub18$percent_total_cover <- percent_of_total_cover #add this percent of total area to the original

veg_type <- c()
for(x in 1:nrow(dat.shrub18)){
  veg_type <-c(veg_type, 'S')
}
dat.shrub18$veg_type <- veg_type

#####################################################################################################
dat.all18 <- rbind.data.frame(dat.herb18, dat.shrub18)
dat.all18 <- rbind.data.frame(dat.all18, dat.tree18)
dat.all18$sp <- gsub(' ', '', dat.all18$sp, fixed = T)

#translate all the names using the translation key
trans_key <- data.frame(read.csv('../DATA/collection_priority.csv', as.is = T)[2:4])
trans_key$Original_name <- gsub(' ', '', trans_key$Original_name, fixed = T)
dat.all18$accepted_name <- trans_key$acceptedMOR[match(dat.all18$sp,
                                                     trans_key$Original_name)]
dat.all18 <- dat.all18[order(dat.all18$plot),]

write.csv(dat.all18, '../OUTPUTS/cover_18.csv')
#####################################################################################################
#Do for 2007
#####################################################################################################

#open up those csvs and put them in dataframes
dat.herb07 <- read.csv(file = "spp.herb.2007.csv", as.is = T)[1:4]
dat.shrub07 <- read.csv(file = "spp.shrub.2007.csv", as.is = T)[1:4]
dat.tree07 <- read.csv(file = "spp.trees.2007.csv", as.is = T)[1:4]
dat.herb07$code<-NULL
dat.shrub07$code<-NULL
dat.tree07$code<-NULL
names(dat.herb07) <- c('plot', 'sp', 'cover')
names(dat.shrub07) <- c('plot', 'sp', 'cover')
names(dat.tree07) <- c('plot', 'sp', 'cover')
#####################################################################################################
herb_plots <- data.frame(unique(dat.herb07$plot)) #get all the herb plots
total_cover <- c()
dat.herb07$cover <- as.double(dat.herb07$cover) #cover columns are as characters in 2007 data :(
for (plt in herb_plots$unique.dat.herb07.plot.){ #for each herb plot I find the sum of all the percent covers
  temp <- sum(dat.herb07$cover[which(dat.herb07$plot == plt)]) #this will be over 100% for some plots
  total_cover <- c(total_cover, temp) 
}
herb_plots$total_cover <- total_cover #I make a new dataframe that has the total cover for each plot 

dat.herb07 <- dat.herb07[which(!dat.herb07$cover == 0),] #I get rid of any rows that I won't be able to divide on
percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(i in 1:length(dat.herb07$plot)){ #...each spp in the plot takes up
  percent_of_total_cover <- 
  c(percent_of_total_cover, ((dat.herb07[i,'cover'])/(herb_plots[which(herb_plots$unique.dat.herb07.plot. == dat.herb07[i,'plot']), 'total_cover'])))
}

dat.herb07$percent_total_cover <- percent_of_total_cover #add this percent of total area to the original

veg_type <- c()
for(x in 1:nrow(dat.herb07)){
  veg_type <-c(veg_type, 'H')
}
dat.herb07$veg_type <- veg_type

##########################################################################################################
#adjust the trees to be in basal area 
dat.tree07$cover <- as.double(dat.tree07$cover) #for some reason my cover is a factor so convert so I can do math
dat.tree07 <- na.omit(dat.tree07) #get rid of the na's because they mess up the number of rows later
dat.tree07 <- dat.tree07[which(!dat.tree07$cover == 0),] #get rid of rows that have no trees reported
#plot area = 33.82 sq ft
BA <- c() #calculate the basal area (just the area of the trunk)
for (c in dat.tree07$cover){
  c <- c * 0.0328084 #converting the cm DBH into feet  (so the final area is in sq feet)
  basal_area <- pi * (c/2)**2
  BA <- c(BA, basal_area)
}
dat.tree07$cover <- BA #replace the DBH values with the basal area. we don't care about dbh anymore

tree_plots <- data.frame(unique(dat.tree07$plot)) #get all the tree plots
total_cover <- c() #doing the same thing i did for herbs, take all the basal areas in a plot and 
for (plt in tree_plots$unique.dat.tree07.plot.){ #... add them together so you have the total area 
  temp <- sum(dat.tree07$cover[which(dat.tree07$plot == plt)]) # .. that is occupied by trees
  total_cover <- c(total_cover, temp)
}
tree_plots$total_cover <- total_cover 

percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(i in 1:length(dat.tree07$plot)){ #...each spp in the plot takes up
  percent_of_total_cover <- 
    c(percent_of_total_cover, ((dat.tree07[i,'cover'])/(tree_plots[which(tree_plots$unique.dat.tree07.plot. == dat.tree07[i,'plot']), 'total_cover'])))
}

dat.tree07$percent_total_cover <- percent_of_total_cover
veg_type <- c()
for(x in 1:nrow(dat.tree07)){
  veg_type <-c(veg_type, 'T')
}
dat.tree07$veg_type <- veg_type

##########################################################################################################
#adjust shrubs to be in percent cover 
dat.shrub07$cover <- as.double(dat.shrub07$cover) #again just have to convert cover col to numerical values for math
shrub_plots <- data.frame(unique(dat.shrub07$plot)) #get all the shrub plots
total_cover <- c()
for (plt in shrub_plots$unique.dat.shrub07.plot.){ #for each shrub plot I find the sum of all the percent covers
  temp <- sum(dat.shrub07$cover[which(dat.shrub07$plot == plt)]) #this will be over 100% for some plots
  total_cover <- c(total_cover, temp) 
}
shrub_plots$total_cover <- total_cover #I make a new dataframe that has the total cover for each plot 

dat.shrub07 <- dat.shrub07[which(!dat.shrub07$cover == 0),] #I get rid of any rows that I won't be able to divide on
percent_of_total_cover <- c() #creating a new column which will have the percent of total plot area that
for(i in 1:length(dat.shrub07$plot)){ #...each spp in the plot takes up
  percent_of_total_cover <- 
    c(percent_of_total_cover, ((dat.shrub07[i,'cover'])/(shrub_plots[which(shrub_plots$unique.dat.shrub07.plot. == dat.shrub07[i,'plot']), 'total_cover'])))
}

dat.shrub07$percent_total_cover <- percent_of_total_cover #add this percent of total area to the original

veg_type <- c()
for(x in 1:nrow(dat.shrub07)){
  veg_type <-c(veg_type, 'S')
}
dat.shrub07$veg_type <- veg_type

#####################################################################################################
dat.all07 <- rbind.data.frame(dat.herb07, dat.shrub07)
dat.all07 <- rbind.data.frame(dat.all07, dat.tree07)
dat.all07$sp <- gsub(' ', '', dat.all07$sp, fixed = T)
dat.all07$plot <- gsub('-', '', dat.all07$plot)

#translate all the names using the translation key
trans_key <- data.frame(read.csv('../DATA/collection_priority.csv', as.is = T)[2:4])
trans_key$Original_name <- gsub(' ', '', trans_key$Original_name, fixed = T)
dat.all07$accepted_name <- trans_key$acceptedMOR[match(dat.all07$sp,
                                                     trans_key$Original_name)]
dat.all07 <- dat.all07[order(dat.all07$plot),]

write.csv(dat.all07, '../OUTPUTS/cover_07.csv')
