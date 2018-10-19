#canopy analysis 

library(picante)
library(tidyverse)

setwd('~/Documents/morton arb/east_woods_phylogeny/analyses/')
marlin_data <- read.csv('DATA/marlins_data.csv')[1:9]
diversity_metrics <- read.csv('OUTPUTS/diversity_metrics.csv', row.names = 1)

marlin_data$Plot <- gsub('-', '', marlin_data$Plot)
marlin_data[3:8] <- NULL
marlin_data[1] <- NULL

newdf <- NULL
for (p in marlin_data$Plot){
  plots <- diversity_metrics[match(p, row.names(diversity_metrics)),]
  newdf <- rbind(newdf, plots)
}
newdf$canopy <- marlin_data$CanopyOpenness
newdf <- na.omit(newdf) #theres a bunch of NAs because there are a lot of the marlin plots that arent survey plots!! :( 
ggplot() + 
  geom_point(aes(newdf$PD, newdf$canopy)) +
  geom_smooth(method = 'lm') +
  xlab('phylogenetic diversity') +
  ylab('canopy openness')
  
cor.test(newdf$PD, newdf$canopy, method = 'pearson')
x <- lm(PD ~ canopy, newdf)
#################################################################################################
#OKEEE so ive been working on this for a while... 
#I think I like sticking with the pearson correlation for analysis
#however im not getting any relationship between canopy and PD ORRR PBD
#I think maybe I have to scale canopy in some waY? 
#or perhaps there is absolutely no correlation 
#either way...there should be a line showing up when I do geom_smooth but theres not...
#additionally.....this is canopy data from 2007... maybe I should try this on the 2007 spp pool 
#if theres still no relationship then, I think there probably is no correlation between the two var
#################################################################################################

# plots_i_have <- intersect(marlin_data$Plot, row.names(diversity_metrics))
# weirdos <- setdiff(marlin_data$Plot, plots_i_have)

