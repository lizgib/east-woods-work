#drought index 

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
marlin_data <- read.csv('DATA/marlins_data.csv')[2:8]
diversity_metrics <- read.csv('OUTPUTS/diversity_metrics.csv', row.names = 1)
marlin_data$Plot <- gsub('-', '', marlin_data$Plot)
marlin_data[4:6] <- NULL

#probably want to do a topographic wetness index?
#don't think I can I need the upslope?? still looking for how to find that may need GIS
newdf <- NULL
for (p in marlin_data$Plot){
  plots <- diversity_metrics[match(p, row.names(diversity_metrics)),]
  newdf <- rbind(newdf, plots)
}

newdf$aspect <- marlin_data$Aspect
newdf$slope <- marlin_data$Slope
newdf$elevation <- marlin_data$Elevation
newdf <- na.omit(newdf) #theres a bunch of NAs because there are a lot of the marlin plots that arent survey plots!! :( 

#AFFECT OF SLOPE 
cor.test(newdf$PD, newdf$slope, cor.method = 'pearson')
#AFFECT OF ASPECT
cor.test(newdf$PD, newdf$aspect, cor.method = 'pearson')
#AFFECT OF ELEVATION 
cor.test(newdf$PD, newdf$elevation, cor.method = 'pearson')
#AFFECT OF SLOPE X ASPECT
newdf$slopeXaspect <- newdf$slope * newdf$aspect
cor.test(newdf$PD, newdf$slopeXaspect, cor.method = 'pearson')
#AFFECT OF SLOPE X ASPECT X ELEVATION 
newdf$slopeXaspectXelevation <- newdf$slopeXaspect * newdf$elevation
cor.test(newdf$PD, newdf$slopeXaspectXelevation, cor.method = 'pearson')

ggplot()+
  #geom_point(aes(newdf$slope, newdf$PD), color = 'blue')+
  #geom_point(aes(newdf$aspect, newdf$PD), color = 'red')+
  geom_point(aes(newdf$elevation, newdf$PD), color = 'green')+ 
  xlab('envt factor')+ 
  ylab('phylogenetic diversity')

#DOWN HERE... I DID THIS TO SEE HOW COR.TEST WAS WORKING 
#all i did was get the number of species per plot from the all_spp_18.csv
#I thought this would have to be pretty strongly correlated with PD (itslike 0.6 when i do cor.test)

# all_spp <- read.csv('../data_processing/OUTPUTS/all_spp_18.csv')
# table(all_spp[,2])
# spp_num <- table(all_spp[,2])
# spp_num <- data.frame(spp_num)
# adf <- NULL
# for (p in marlin_data$Plot){
# plots <- spp_num[match(p, spp_num[,1]),]
# adf <- rbind(adf, plots)
# }
# adf <- na.omit(adf)
# newdf$nummspp<-adf$Freq


