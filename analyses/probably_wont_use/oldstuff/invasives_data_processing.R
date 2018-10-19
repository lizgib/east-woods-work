#processing invasives list 

#need to format the list from online in a way I can use translation key with 

invasives <- read.delim('DATA/invasives.csv', header = T, sep = '\t')
tranlsation_key <- read.csv('../data_processing/DATA/collection_priority.csv')
invasives$inv_spp <- paste(invasives$genus, invasives$species)
invasives$inv_spp <- gsub(' ', '', invasives$inv_spp)
tranlsation_key$Original_name <- gsub(' ', '', tranlsation_key$Original_name)
invasives_present <- tranlsation_key$acceptedMOR[match(invasives$inv_spp, tranlsation_key$Original_name)]
invasives_present <- na.omit(invasives_present)
invasives_present <- gsub(' ', '_', invasives_present)
write.csv(invasives_present, 'OUTPUTS/processed_invasives_list.csv')

