# take the groups output from EcoStructure and grab the plots that 
# have predominant membership in a specific community 
# then make a new community matrix for each of these new subsetted communities

# this script will only work for k = 3

k3_2007.omega <- read.csv('figures/Summer 2019/EcoStructure/Omega/2007/k3_dat_mat_all_07.csv')
k3_2018.omega <- read.csv('figures/Summer 2019/EcoStructure/Omega/2018/k3_dat_mat_all_18.csv')
data('plots.env')

change_analysis <- cbind(k3_2007.omega, k3_2018.omega)
rownames(change_analysis) <- change_analysis$X
change_analysis$X <- NULL
change_analysis$X <- NULL
names(change_analysis) <- c('Group1.2007', 'Group2.2007', 'Group3.2007', 'Group1.2018', 'Group2.2018', 'Group3.2018')

# Change 
change_analysis$ChangeGroup1 <- abs(change_analysis$Group1.2018 - change_analysis$Group1.2007) 
change_analysis$ChangeGroup2 <- abs(change_analysis$Group2.2018 - change_analysis$Group2.2007)
change_analysis$ChangeGroup3 <- abs(change_analysis$Group3.2018 - change_analysis$Group3.2007)

# this is not an accurate way of looking at change. the groups are not consistent between t2007 and 2018

change_analysis$OverallChange <- change_analysis$ChangeGroup1 + change_analysis$ChangeGroup2 + change_analysis$ChangeGroup3

# Definining Group Membership 
change_analysis$Group07 <- ifelse(change_analysis$Group1.2007 >= 0.667, 'Group1', 
                                     ifelse(change_analysis$Group2.2007 >= 0.667, 'Group2', 
                                            ifelse(change_analysis$Group3.2007 >= 0.667, 'Group3', 'Mixed')))


change_analysis$Group18 <- ifelse(change_analysis$Group1.2018 >= 0.667, 'Group1', 
                                     ifelse(change_analysis$Group2.2018 >= 0.667, 'Group2', 
                                            ifelse(change_analysis$Group3.2018 >= 0.667, 'Group3', 'Mixed')))
group1_plots07 <- rownames(change_analysis)[which(change_analysis$Group07 == 'Group1')]
group2_plots07 <- rownames(change_analysis)[which(change_analysis$Group07 == 'Group2')]
group3_plots07 <- rownames(change_analysis)[which(change_analysis$Group07 == 'Group3')]

group1_plots18 <- rownames(change_analysis)[which(change_analysis$Group18 == 'Group1')]
group2_plots18 <- rownames(change_analysis)[which(change_analysis$Group18 == 'Group2')]
group3_plots18 <- rownames(change_analysis)[which(change_analysis$Group18 == 'Group3')]

save(group1_plots18, group2_plots18, group3_plots18, group1_plots07, group2_plots07, group3_plots07, file = 'data/ecostructure_groups.RData')

plots.env$Group1.Mem18 <- change_analysis$Group1.2018[match(rownames(plots.env), rownames(change_analysis))]
plots.env$Group2.Mem18 <- change_analysis$Group2.2018[match(rownames(plots.env), rownames(change_analysis))]
plots.env$Group3.Mem18 <- change_analysis$Group3.2018[match(rownames(plots.env), rownames(change_analysis))]

plots.env$Group1.Mem07 <- change_analysis$Group1.2007[match(rownames(plots.env), rownames(change_analysis))]
plots.env$Group2.Mem07 <- change_analysis$Group2.2007[match(rownames(plots.env), rownames(change_analysis))]
plots.env$Group3.Mem07 <- change_analysis$Group3.2007[match(rownames(plots.env), rownames(change_analysis))]

plots.env$EcoStrucGroup18 <- ifelse(rownames(plots.env)%in%group1_plots18, 'Group1', 
                                    ifelse(rownames(plots.env)%in%group2_plots18, 'Group2', 
                                           ifelse(rownames(plots.env)%in%group3_plots18, 'Group3', 'Mixed')))
plots.env$ComClass18 <- ifelse(plots.env$EcoStrucGroup18 == 'Group1', 'Deep Forest', 
                               ifelse(plots.env$EcoStrucGroup18 == 'Group2', 'Edge', 
                                      ifelse(plots.env$EcoStrucGroup18 == 'Group3', 'Disturbed', 'Mixed')))

plots.env$EcoStrucGroup07 <- ifelse(rownames(plots.env)%in%group1_plots07, 'Group1', 
                                    ifelse(rownames(plots.env)%in%group2_plots07, 'Group2', 
                                           ifelse(rownames(plots.env)%in%group3_plots07, 'Group3', 'Mixed')))

plots.env$ComClass07 <- ifelse(plots.env$EcoStrucGroup07 == 'Group1', 'Deep Forest', 
                               ifelse(plots.env$EcoStrucGroup07 == 'Group2', 'Disturbed', 
                                      ifelse(plots.env$EcoStrucGroup07 == 'Group3', 'Edge', 'Mixed')))
save(plots.env, file = 'data/plots.env.RData')
write.csv(plots.env, 'data/plots.env.csv')

# grabbing the community matrix data for these two group 1s 
commat07 <- read.csv('data/Community_Matrix/2007/dat.mat.all.07.csv', row.names = 1)
commat18 <- read.csv('data/Community_Matrix/2018/dat.mat.all.18.csv', row.names = 1)

group1.07.mat <- commat07[which(rownames(commat07) %in% group1_plots07),]
group1.18.mat <- commat18[which(rownames(commat18) %in% group1_plots18),]

write.csv(group1.07.mat, 'data/Community_Matrix/2007/group1.07.mat.csv')
write.csv(group1.18.mat, 'data/Community_Matrix/2018/group1.18.mat.csv')

group2.07.mat <- commat07[which(rownames(commat07) %in% group2_plots07),]
group2.18.mat <- commat18[which(rownames(commat18) %in% group2_plots18),]

write.csv(group2.07.mat, 'data/Community_Matrix/2007/group2.07.mat.csv')
write.csv(group2.18.mat, 'data/Community_Matrix/2018/group2.18.mat.csv')

group3.07.mat <- commat07[which(rownames(commat07) %in% group3_plots07),]
group3.18.mat <- commat18[which(rownames(commat18) %in% group3_plots18),]

write.csv(group3.07.mat, 'data/Community_Matrix/2007/group3.07.mat.csv')
write.csv(group3.18.mat, 'data/Community_Matrix/2018/group3.18.mat.csv')

