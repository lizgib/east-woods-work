setwd("~/Documents/morton arb/east_woods_phylogeny/data_processing/DATA")

#open up those csvs and put them in dataframes
dat.herb <- read.csv(file = "spp.herb.2018.csv", as.is = T)
dat.herb <- data.frame(dat.herb[3:5])
dat.herb$X6.Letter.Code <- NULL
dat.shrub <- read.csv(file = "spp.shrub.2018.csv", as.is = T)
dat.shrub <- data.frame(dat.shrub[3:5])
dat.shrub$X6.Letter.Code <- NULL
dat.tree <- read.csv(file = "spp.trees.2018.csv", as.is = T)
dat.tree <- data.frame(dat.tree[3:5])
dat.tree$X6.Letter.Code <- NULL

dat.all <- rbind.data.frame(dat.herb, dat.shrub)
dat.all <- rbind.data.frame(dat.all, dat.tree)
dat.all$Species_Name <- gsub(' ', '', dat.all$Species_Name, fixed = T)
#translate all the names using the translation key
trans_key <- data.frame(read.csv('../DATA/translation_key_all_AH.csv', as.is = T)[2:4])
trans_key$Original_name <- gsub(' ', '', trans_key$Original_name, fixed = T)
dat.all$Accepted_name <- trans_key$acceptedMOR[match(dat.all$Species_Name,
                                                     trans_key$Original_name)]

write.csv(dat.all, '../OUTPUTS/all_spp_18.csv')

#################################################################################################
#do for 2007 
dat.herb07 <- read.csv(file = "spp.herb.2007.csv", as.is = T)
dat.herb07 <- data.frame(dat.herb07[1:3])
dat.herb07$code <- NULL
dat.shrub07 <- read.csv(file = "spp.shrub.2007.csv", as.is = T)
dat.shrub07 <- data.frame(dat.shrub07[1:3])
dat.shrub07$code <- NULL
dat.tree07 <- read.csv(file = "spp.trees.2007.csv", as.is = T)
dat.tree07 <- data.frame(dat.tree07[1:3])
dat.tree07$code <- NULL

dat.all07 <- rbind.data.frame(dat.herb07, dat.shrub07)
dat.all07 <- rbind.data.frame(dat.all07, dat.tree07)
dat.all07$species <- gsub(' ', '', dat.all07$species, fixed = T)
#translate all the names using the translation key
dat.all07$Accepted_name <- trans_key$acceptedMOR[match(dat.all07$species,
                                                     trans_key$Original_name)]

write.csv(dat.all07, '../OUTPUTS/all_spp_07.csv')
