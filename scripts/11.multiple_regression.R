
# just get the R2 between each var and diversity 

liz_data <- read.csv('~/Documents/GitHub/east_woods_work/data/liz_data.csv')
source('~/Documents/GitHub/east_woods_work/scripts/02.falltranslationkey.R')
source('~/Documents/GitHub/east_woods_work/scripts/phylo_metrics.R')

# 2007 
inv.07.pd <- lm(phylo_all_07$PD ~ liz_data$plot_invasive_cover_07)
inv.07.sr <- lm(phylo_all_07$SR ~ liz_data$plot_invasive_cover_07)

canopy.07.pd <- lm(phylo_all_07$PD ~ liz_data$canopy_07)
canopy.07.sr <- lm(phylo_all_07$SR ~ liz_data$canopy_07)

soil.07.pd <- lm(phylo_all_07$PD ~ liz_data$soil_index)
soil.07.sr <- lm(phylo_all_07$SR ~ liz_data$soil_index)

elevation.07.pd <- lm(phylo_all_07$PD ~ liz_data$elevation)
elevation.07.sr <- lm(phylo_all_07$SR ~ liz_data$elevation)

slope.07.pd <- lm(phylo_all_07$PD ~ liz_data$slope)
slope.07.sr <- lm(phylo_all_07$SR ~ liz_data$slope)

aspect.07.pd <- lm(phylo_all_07$PD ~ liz_data$aspect)
aspect.07.sr <- lm(phylo_all_07$SR ~ liz_data$aspect)


# 2018 
inv.18.pd <- lm(phylo_all_18$PD ~ liz_data$plot_invasive_cover_18)
summary(inv.18.pd)
inv.18.sr <- lm(phylo_all_18$SR ~ liz_data$plot_invasive_cover_18)
summary(inv.18.sr)
canopy.18.pd <- lm(phylo_all_18$PD ~ liz_data$canopy_18)
summary(canopy.18.pd)
canopy.18.sr <- lm(phylo_all_18$SR ~ liz_data$canopy_18)
summary(canopy.18.sr)

soil.18.pd <- lm(phylo_all_18$PD ~ liz_data$soil_index)
soil.18.sr <- lm(phylo_all_18$SR ~ liz_data$soil_index)

elevation.18.pd <- lm(phylo_all_18$PD ~ liz_data$elevation)
summary(elevation.18.pd)
elevation.18.sr <- lm(phylo_all_18$SR ~ liz_data$elevation)
summary(elevation.18.sr)

slope.18.pd <- lm(phylo_all_18$PD ~ liz_data$slope)
summary(slope.18.pd)
slope.18.sr <- lm(phylo_all_18$SR ~ liz_data$slope)
summary(slope.18.sr)

aspect.18.pd <- lm(phylo_all_18$PD ~ liz_data$aspect)
summary(aspect.18.pd)
aspect.18.sr <- lm(phylo_all_18$SR ~ liz_data$aspect)
summary(aspect.18.sr)


