
# dat = 'data/ord_mat/ord_07_all.csv' # NAs need to be removed from this before passing in 
# 
# setwd('~/Documents/GitHub/east_woods_work/')
# dat = read.csv(dat, row.names = 1)


do_kmeans <- function(dat, n){
  dat = as.matrix(dat)
  n = as.numeric(n) # number of ks
  c1 <- kmeans(dat, n, iter.max = 100, nstart = 50)
  return(as.data.frame(c1$cluster))
}

n2 <- do_kmeans(dat, 2) # like woodland grassland?
n3 <- do_kmeans(dat, 3) # like management units
n5 <- do_kmeans(dat, 5)
n10 <- do_kmeans(dat, 10) # closest one to the number of com classes
n20 <- do_kmeans(dat, 20)

oup <- as.data.frame(dat)
oup$k2 <- n2$`c1$cluster`
oup$k3 <- n3$`c1$cluster`
oup$k5 <- n5$`c1$cluster`
oup$k10 <- n10$`c1$cluster`
oup$k20 <- n20$`c1$cluster`

# n <- 20
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# colvec = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[oup$k2])
# title('K2 2007')
# #ordiellipse(ord_ew, oup$k2, col = 'black', lwd = 2, label= F)
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[oup$k3])
# title('K3 2007')
# #ordiellipse(ord_ew, oup$k3, col = 'black', lwd = 2, label= F)
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[oup$k5])
# title('K5 2007')
# #ordiellipse(ord_ew, oup$k5, col = 'black', lwd = 2, label= F)
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[oup$k10])
# title('K10 2007')
# #ordiellipse(ord_ew, oup$k10, col = 'black', lwd = 2, label= F)
# 
# plot(ord_ew, type = 'n')
# points(ord_ew, display = 'sites', cex = 0.8, pch = 21, col = colvec[oup$k20])
# title('K20 2007')
# #ordiellipse(ord_ew, oup$k20, col = 'darkgray', lwd = 2, label= F)

write.csv(oup, 'outputs/clust_07.csv', quote = F) # save the cluster IDs to file






