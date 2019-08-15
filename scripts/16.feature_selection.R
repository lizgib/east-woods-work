
most_important_features <- function(fs_outputs){
  
  fs <- read.csv(fs_outputs, header = F)
  newfeatures <- fs$V1[which(fs$V2 > 0)]
  return(newfeatures)
}

new_species <- most_important_features('data/MachineLearning/FeatureImportance/Change_species.csv')
ML_species <- read.csv('data/MachineLearning/Matrix/species_change.csv', row.names = 1)
ML_species <- ML_species[which(names(ML_species) %in% new_species),]
write.csv(ML_species, 'data/MachineLearning/Matrix/species_change.csv')
