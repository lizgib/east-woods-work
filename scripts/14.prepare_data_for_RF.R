# prepare some data for running Random Forest on 

data("plots.env")
dat.mat.all.07 <- read.csv('data/Community_Matrix/2007/dat.mat.all.07.csv', row.names = 1)
dat.mat.all.18 <- read.csv('data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)

# grab onl y the continuous data from plots.env

# NOTE: Im including the community class in 2007 but I may remove it later if it makes things too obvious for the alg to figure out 
# I am NOT including the group membership in 2018 because that's what we're predicting! 
colnames(dat.mat.all.07) <- paste(colnames(dat.mat.all.07), '.2007', sep = '')
colnames(dat.mat.all.18) <- paste(colnames(dat.mat.all.18), '.2018', sep = '')


# have decided I will not include variables TopOrgMat and LowOrgMat because there are a lot of NAs
# also decided to take out the coordinate variables because theyre not really interpretable in the model "x.nad83", "y.nad83", "x.utm16", "y.utm16", "lon", "lat",
plots_ML <- plots.env[,c('ComClass18', 'ComClass07', 'Group1.Mem18', 'Group2.Mem18', 'Group3.Mem18', "elev", "slope", "aspect", "tpi", "TreeCover07",  
                        "BurnCount", "TopDryWgt", "LowDryWgt", "TopWtrContent", "LowWtrContent", "PathDist", "RoadDist", 
                        "RoadPathDist", "SolRad", "SR07", "PD07", "MNTD07", "PSV07")] # "TreeCover18",#,"MNTD18", "PSV18", "SR18", "PD18")] #, "Group1.Mem07", "Group2.Mem07", "Group3.Mem07")]

plots_ML$Change <- ifelse(plots_ML$ComClass07 != plots_ML$ComClass18, 1, 0)
plots_ML$ComClass18<- ifelse(plots_ML$ComClass18 == 'Deep Forest', 1, 
                                ifelse(plots_ML$ComClass18 == 'Edge', 2, 
                                       ifelse(plots_ML$ComClass18 == 'Disturbed', 3, 4)))

# all_ML_data <- cbind(plots_ML, dat.mat.all.07, dat.mat.all.18)
# species_ML <- cbind(dat.mat.all.07, dat.mat.all.18)
all_ML_data <- cbind(plots_ML, dat.mat.all.07)
species_ML <- dat.mat.all.07

# need to impute/drop the NAs
all_ML_data <- na.omit(all_ML_data)
plots_ML <- na.omit(plots_ML)
species_ML <- na.omit(species_ML)

all_ML_data <- all_ML_data[order(rownames(all_ML_data)),]
plots_ML <- plots_ML[order(rownames(plots_ML)),]
species_ML <- species_ML[order(rownames(species_ML)),]

# All Data frames 

all_change <- all_ML_data[!names(all_ML_data) %in% c("ComClass18", "ComClass07", "Group1.Mem18", "Group2.Mem18", "Group3.Mem18")]
all_change <- na.omit(all_change)
write.csv(all_change, 'data/MachineLearning/Matrix/all_change.csv', quote = F)

all_ComClass18 <- all_ML_data[!names(all_ML_data) %in% c("ComClass07", "Group1.Mem18", "Group2.Mem18", "Group3.Mem18", "Change")]
all_ComClass18 <- na.omit(all_ComClass18)
write.csv(all_ComClass18, 'data/MachineLearning/Matrix/all_ComClass18.csv', quote = F)

all_Group1 <- all_ML_data[!names(all_ML_data) %in% c("ComClass18", "ComClass07", "Group2.Mem18", "Group3.Mem18", "Change")]
all_Group1 <- na.omit(all_Group1)
write.csv(all_Group1, 'data/MachineLearning/Matrix/all_Group1.csv', quote = F)

all_Group2 <- all_ML_data[!names(all_ML_data) %in% c("ComClass18", "ComClass07", "Group1.Mem18", "Group3.Mem18", "Change")]
all_Group2 <- na.omit(all_Group2)
write.csv(all_Group2, 'data/MachineLearning/Matrix/all_Group2.csv', quote = F)

all_Group3 <- all_ML_data[!names(all_ML_data) %in% c("ComClass18", "ComClass07", "Group1.Mem18", "Group2.Mem18", "Change")]
all_Group3 <- na.omit(all_Group3)
write.csv(all_Group3, 'data/MachineLearning/Matrix/all_Group3.csv', quote = F)

 # Species Data frames 
species_change <- species_ML
species_change$Change <- plots_ML$Change[match(rownames(species_change), rownames(plots_ML))]
species_change <- na.omit(species_change)
write.csv(species_change, 'data/MachineLearning/Matrix/species_change.csv', quote = F)

species_ComClass18 <- species_ML
species_ComClass18$ComClass18 <- plots_ML$ComClass18[match(rownames(species_ComClass18), rownames(plots_ML))]
species_ComClass18 <- na.omit(species_ComClass18)
write.csv(species_ComClass18, 'data/MachineLearning/Matrix/species_ComClass18.csv', quote = F)

species_Group1 <- species_ML
species_Group1$Group1.Mem18 <- plots.env$Group1.Mem18[match(rownames(species_Group1), rownames(plots.env))]
species_Group1 <- na.omit(species_Group1)
write.csv(species_Group1, 'data/MachineLearning/Matrix/species_Group1.csv', quote = F)

species_Group2 <- species_ML
species_Group2$Group2.Mem18 <- plots.env$Group2.Mem18[match(rownames(species_Group2), rownames(plots.env))]
species_Group2 <- na.omit(species_Group2)
write.csv(species_Group2, 'data/MachineLearning/Matrix/species_Group2.csv', quote = F)

species_Group3 <- species_ML
species_Group3$Group3.Mem18 <- plots.env$Group3.Mem18[match(rownames(species_Group3), rownames(plots.env))]
species_Group3 <- na.omit(species_Group3)
write.csv(species_Group3, 'data/MachineLearning/Matrix/species_Group3.csv', quote = F)


# Plot Data frames 
plots_change <-plots_ML[!names(plots_ML) %in% c("ComClass18", "ComClass07", "Group1.Mem18", "Group2.Mem18", "Group3.Mem18")]
plots_change <- na.omit(plots_change)
write.csv(plots_change, 'data/MachineLearning/Matrix/plots_change.csv', quote = F)

plots_ComClass18 <- plots_ML[!names(plots_ML) %in% c("ComClass07", "Group1.Mem18", "Group2.Mem18", "Group3.Mem18", "Change")]
plots_ComClass18 <- na.omit(plots_ComClass18)
write.csv(plots_ComClass18, 'data/MachineLearning/Matrix/plots_ComClass18.csv', quote = F)

plots_Group1 <- plots_ML[!names(plots_ML) %in% c("ComClass18", "ComClass07", "Group2.Mem18", "Group3.Mem18", "Change")]
plots_Group1 <- na.omit(plots_Group1)
write.csv(plots_Group1, 'data/MachineLearning/Matrix/plots_Group1.csv', quote = F)

plots_Group2 <- plots_ML[!names(plots_ML) %in% c("ComClass18", "ComClass07", "Group1.Mem18", "Group3.Mem18", "Change")]
plots_Group2 <- na.omit(plots_Group2)
write.csv(plots_Group2, 'data/MachineLearning/Matrix/plots_Group2.csv', quote = F)

plots_Group3 <- plots_ML[!names(plots_ML) %in% c("ComClass18", "ComClass07", "Group1.Mem18", "Group2.Mem18", "Change")]
plots_Group3 <- na.omit(plots_Group3)
write.csv(plots_Group3, 'data/MachineLearning/Matrix/plots_Group3.csv', quote = F)





