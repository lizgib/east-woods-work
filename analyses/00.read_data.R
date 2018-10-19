#OPEN DATA FOR ANALYSES 
#opens up the csvs with 
#   1. marlin's data
#   2. allherbs.csv (herbs from both surveys in 2007 and 2018 spring herbs)
#   2. allshrubs.csv (shrubs from both surveys in 2007 and 2018 spring shrubs)
#   2. alltrees.csv (trees from both surveys in 2007 and 2018 spring trees)
#also opens up the latest version of the east woods tree 
library(ggtree)
library(picante)
library(vegan)
source('~/Documents/morton arb/east_woods_phylogeny/data_processing/SCRIPTS/02.cover.R')
setwd('~/Documents/morton arb/east_woods_phylogeny/analyses')
#opens marlin's data
marlin_data <- read.csv('DATA/marlins_data.csv', row.names = 2)
rownames(marlin_data) <- gsub('-', '', rownames(marlin_data))

#opens all the existing survey data
allherbs <- read.csv('~/Documents/morton arb/east_woods_phylogeny/data_processing/OUTPUTS/allherbs.csv', as.is = T)
allshrubs <- read.csv('~/Documents/morton arb/east_woods_phylogeny/data_processing/OUTPUTS/allshrubs.csv', as.is = T)
alltrees <- read.csv('~/Documents/morton arb/east_woods_phylogeny/data_processing/OUTPUTS/alltrees.csv', as.is = T)

#edits marlin data
names(allshrubs) <- names(allherbs)   #this nonsense in here is because some of the marlin plots are
names(alltrees) <- names(allherbs)    #not survey plots so I have to remove them
all <- rbind(allherbs, allshrubs, alltrees)
marlin_data <- marlin_data[which(rownames(marlin_data) %in% dat.all07$plot),]

#reads tree
tr.ewv3 <- read.tree('../phylogeny_generation/OUTPUTS/tr.ewv3.edited')
