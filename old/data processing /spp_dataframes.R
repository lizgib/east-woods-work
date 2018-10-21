#make individual dataframes for all herbs, shrubs, and trees from 2007 and 2018

setwd('~/Documents/morton arb/east_woods_phylogeny/data_processing/')

springherbs07 <- read.csv('DATA/spp.herb.2007.csv', as.is = T)
summerherbs07 <- read.csv('DATA/summer.spp.herbs.2007.csv', as.is = T)
colnames(summerherbs07) <- colnames(springherbs07)
summerherbs07 <- summerherbs07[-1,]
herbs18 <- read.csv('DATA/spp.herb.2018.csv', as.is = T)
herbs18 <- herbs18[,3:6]
allherbs07 <- rbind(springherbs07, summerherbs07)
allherbs07 <- allherbs07[,1:4]
colnames(herbs18) <- colnames(allherbs07)
allherbs <- rbind(allherbs07, herbs18)
allherbs$plot <- gsub('-', '', allherbs$plot)
allherbs$species <- gsub(' ', '', allherbs$species)
translation_key <- read.csv('DATA/translation_key_all_AH.csv')
translation_key$Original_name <- gsub(' ', '', translation_key$Original_name)
allherbs$accepted_name <- translation_key$acceptedMOR[match(allherbs$species, translation_key$Original_name)]
write.csv(allherbs, 'OUTPUTS/allherbs.csv')
####################################################################################################################

springshrubs07 <- read.csv('DATA/spp.shrub.2007.csv', as.is = T)
summershrubs07 <- read.csv('DATA/summer.spp.shrubs.2007.csv', as.is = T)
colnames(summershrubs07) <- colnames(springshrubs07)
summershrubs07 <- summershrubs07[-1,]
allshrubs07 <- rbind(springshrubs07, summershrubs07)
allshrubs07 <- allshrubs07[,1:4]

shrubs18 <- read.csv('DATA/spp.shrub.2018.csv', as.is = T)
shrubs18 <- shrubs18[,3:8]
shrubs18 <- shrubs18[,-(4:5)]
colnames(shrubs18) <- colnames(allshrubs07)
allshrubs <- rbind(allshrubs07, shrubs18)
allshrubs$plot <- gsub('-', '', allshrubs$plot)
allshrubs$species <- gsub(' ', '', allshrubs$species)
translation_key <- read.csv('DATA/translation_key_all_AH.csv')
translation_key$Original_name <- gsub(' ', '', translation_key$Original_name)
allshrubs$accepted_name <- translation_key$acceptedMOR[match(allshrubs$species, translation_key$Original_name)]
write.csv(allshrubs, 'OUTPUTS/allshrubs.csv')

####################################################################################################################

springtrees07 <- read.csv('DATA/spp.trees.2007.csv', as.is = T)
summertrees07 <- read.csv('DATA/summer.spp.trees.2007.csv', as.is = T)
colnames(summertrees07) <- colnames(springtrees07)
summertrees07 <- summertrees07[-1,]
alltrees07 <- rbind(springtrees07, summertrees07)
alltrees07 <- alltrees07[,1:4]

trees18 <- read.csv('DATA/spp.trees.2018.csv', as.is = T)
trees18 <- trees18[,3:6]
colnames(trees18) <- colnames(alltrees07)
alltrees <- rbind(alltrees07, trees18)
alltrees$plot <- gsub('-', '', alltrees$plot)
alltrees$species <- gsub(' ', '', alltrees$species)
translation_key <- read.csv('DATA/translation_key_all_AH.csv')
translation_key$Original_name <- gsub(' ', '', translation_key$Original_name)
alltrees$accepted_name <- translation_key$acceptedMOR[match(alltrees$species, translation_key$Original_name)]
write.csv(alltrees, 'OUTPUTS/alltrees.csv')



