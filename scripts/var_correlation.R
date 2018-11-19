#
#Doing spp corr this way bc I dont know how else to 
#11/15

#
source('~/Documents/GitHub/east_woods_work/scripts/09.envt_data.R')
source('~/Documents/GitHub/east_woods_work/scripts/11.analyses.R')

aspect <- liz_data$aspect
slope <- liz_data$slope
elevation <- liz_data$elevation
burn_count <- liz_data$burn_count
marlin_canopy <- liz_data$marlin_canopy
invasives_18 <- liz_data$invasive_ratio_18
invasives_07 <- liz_data$invasive_ratio_07
canopy18 <- liz_data$canopy_18
canopy07 <-liz_data$canopy_07

panel.fit<-function(x, y, ...) {
  ll <- loess(y~x)
  points(x,y, ...)
  nx<-seq(min(x), max(x), length.out=150)
  lines(nx, predict(ll, nx), col="blue",lwd=2)
  abline(a = lm(y ~ x)$coefficients[1] , b = lm(y ~ x)$coefficients[2] ,col="red",lwd=2, ...)
}

panel.cor <- function(x, y, digits=2, prefix="",cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 1/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor*abs(r))
}
pairs(liz_data[4:16],#using the first four variables in the iris data set
 #     pch = c(0,1,2)[unclass(liz_data$plots)], #give each species in the dataset a different symbol when plotted
      upper.panel=panel.cor,
      lower.panel=panel.fit)





