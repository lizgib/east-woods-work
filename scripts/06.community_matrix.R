com_mat <- function(dat, tr.ewv4){
  dat$accepted_name <- trimws(dat$accepted_name)
  vects <- list(plots = unique(sort(dat$plot)),
                sp = unique(sort(dat$species)),
                accname = unique(sort(dat$accepted_name)))
  vects$accname <- vects$accname[which(!vects$accname %in% c('', ' ', '0'))]
  dat.mat <- matrix(0, length(vects$plots), length(vects$accname),
                    dimnames = list(vects$plots, vects$accname))
  
  for(p in unique(dat$plot)){
    spp_occur <- as.data.frame(table(dat$accepted_name[which(dat$plot == p)]))
    for (spp in spp_occur$Var1){
      if(!spp %in% dimnames(dat.mat)[[2]]) next
      dat.mat[p, spp] <- 1
    }
  }
  colnames(dat.mat) <- gsub('[-, ]', '_', colnames(dat.mat))
  names_in_tree <- intersect(tr.ewv4$tip.label, colnames(dat.mat))
  dat.mat.out <- dat.mat[,which(colnames(dat.mat) %in% names_in_tree)]
  return(dat.mat.out) #
}

# not currently using this function. Intended for treating the seedlings as herbs instead of trees 
# problem I'm having with this is incorporating into the phylogenetic metrics. things break when I 
# try to analyze a species pool with names not on the tree. 
seedlings <- function(tree.mat, herb.mat){
  vec <- c()
  for (sp in colnames(herb.mat)){
    print(sp)
    if(sp %in% colnames(herb.mat)){
      vec <- c(vec, paste(sp, 'Seedling', sep = '_'))
    }
    else{
      vec <- c(vec, sp)
    }
  }
  colnames(herb.mat) <- vec
  return(herb.mat)
}
