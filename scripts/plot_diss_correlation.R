
# like plot_correlations.R except now looking at the pairwise differences between plots in 
# terms of diversity and each environmental variable 

liz_data <- read.csv('~/Documents/GitHub/east_woods_work/data/liz_data.csv')
source('~/Documents/GitHub/east_woods_work/scripts/02.falltranslationkey.R')
source('~/Documents/GitHub/east_woods_work/scripts/10.phylo_diss_matrices.R')

# 2007
inv.07.pd <- lm(beta_Dnn_all.07 ~ invasives07)
summary(inv.07.pd)
inv.07.sr <- lm(all_jaccard_07 ~ invasives07)
summary(inv.07.sr)

canopy.07.pd <- lm(beta_Dnn_all.07 ~ canopy07)
summary(canopy.07.pd)
canopy.07.sr <- lm(all_jaccard_07 ~ canopy07)
summary(canopy.07.sr)

# soil.07.pd <- lm(phylo_all_07$PD ~ liz_data$soil_index)
# soil.07.sr <- lm(phylo_all_07$SR ~ liz_data$soil_index)

elevation.07.pd <- lm(beta_Dnn_all.07 ~ elevation)
summary(elevation.07.pd)
elevation.07.sr <- lm(all_jaccard_07 ~ elevation)
summary(elevation.07.sr)

slope.07.pd <- lm(beta_Dnn_all.07 ~ slope)
summary(slope.07.pd)
slope.07.sr <- lm(all_jaccard_07 ~ slope)
summary(slope.07.sr)

aspect.07.pd <- lm(beta_Dnn_all.07 ~ aspect)
summary(aspect.07.pd)
aspect.07.sr <- lm(all_jaccard_07 ~ aspect)
summary(aspect.07.sr)

# 2018 
inv.18.pd <- lm(beta_Dnn_all.18 ~ invasives18)
summary(inv.18.pd)
inv.18.sr <- lm(all_jaccard_18 ~ invasives18)
summary(inv.18.sr)

canopy.18.pd <- lm(beta_Dnn_all.18 ~ canopy18)
summary(canopy.18.pd)
canopy.18.sr <- lm(all_jaccard_18 ~ canopy18)
summary(canopy.18.sr)

# soil.18.pd <- lm(phylo_all_18$PD ~ liz_data$soil_index)
# soil.18.sr <- lm(phylo_all_18$SR ~ liz_data$soil_index)

elevation.18.pd <- lm(beta_Dnn_all.18 ~ elevation)
summary(elevation.18.pd)
elevation.18.sr <- lm(all_jaccard_18 ~ elevation)
summary(elevation.18.sr)

slope.18.pd <- lm(beta_Dnn_all.18 ~ slope)
summary(slope.18.pd)
slope.18.sr <- lm(all_jaccard_18 ~ slope)
summary(slope.18.sr)

aspect.18.pd <- lm(beta_Dnn_all.18 ~ aspect)
summary(aspect.18.pd)
aspect.18.sr <- lm(all_jaccard_18 ~ aspect)
summary(aspect.18.sr)




