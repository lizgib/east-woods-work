source('~/Documents/morton arb/east_woods_phylogeny/SCRIPTS/15.falltranslationkey.R')

spp07 <- sort(unique(dat.07$accepted_name))
spp18 <- sort(unique(dat.18$accepted_name))
allspp <- sort(unique(dat.all$accepted_name))


length(spp07)   # 285 species reported in 2007
length(spp18)   # 347 species reported in 2018 
length(allspp)  # 438 species between years 

both_07_and_18 <- sort(intersect(spp07, spp18))
in_07_not_18 <- sort(setdiff(spp07, spp18))
in_18_not_07 <- sort(setdiff(spp18, spp07))

both <- c()
only_07 <- c()
only_18 <- c()
whatschanged <- data.frame(allspp) 
for (sp in whatschanged$allspp){
  ifelse(sp %in% both_07_and_18, 
         both <-c(both, 1), both <- c(both, 0))
  ifelse(sp %in% in_07_not_18, 
         only_07 <- c(only_07, 1), only_07 <- c(only_07, 0))
  ifelse(sp %in% in_18_not_07, 
         only_18 <- c(only_18, 1), only_18 <- c(only_18, 0))
}

whatschanged$both_07_and_18 <- both
whatschanged$only_in_07 <- only_07
whatschanged$only_in_18 <- only_18
