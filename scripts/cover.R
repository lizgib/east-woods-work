
# FUNCTIONS for calculating plot cover estimates from species cover estimates 

  # function to convert tree cover from DBH to BA (but I am currently handling this when I read the data in so really not necessary anymore)
  # have one to calculate total plot cover (all the cover of everything added up)
  # have another one to calculate the invasive cover based on a few species (how much of these species is present in plot)

# both of these i am just setting up to take dat.all, not individually masked dataframes like before

get_BA <- function(dat){
  BA <- c() #calculate the basal area (just the area of the trunk)
  for (c in dat$cover){
      c <- c * 0.0328084 #converting the cm DBH into feet  (so the final area is in sq feet)
    basal_area <- pi * (c/2)**2
    BA <- c(BA, basal_area)
  }
  dat$cover <- BA # im replacing the cover column so I can use the other function on it (wouldnt work if I 
                  # renamed BA)
  return(dat)
  
}


get_plot_cover <- function(dat){
  dat <- dat[which(!is.na(dat$cover)),]
  dat <- dat[which(dat$cover != 0),]
  dat$cover <- as.numeric(dat$cover)
  total_cover <- 
    data.frame(
      cov = sapply(unique(dat$plot), function(x){
        sum(dat$cover[which(dat$plot == x)], na.rm = T)
      }
    ),
    row.names = unique(dat$plot)
  )
  total_cover$cov <- as.numeric(as.character(total_cover$cov))
  dat$plot_cover <- total_cover$cov[match(dat$plot, row.names(total_cover))]
  dat$spp_percent_total_cover <- dat$cover/dat$plot_cover
  return(dat)
}

get_invasive_cover <- function(plots.env, dat){
  # ------
  # returns a new dataframe with the invasive cover of each plot
  # -----
  dat <- get_plot_cover(dat)
  
  invasives <- dat[grep('Alliaria', dat$accepted_name),]
  invasives <- rbind(invasives, dat[grep('Lonicera', dat$accepted_name),])
  invasives <- rbind(invasives, dat[grep('Rhamnus', dat$accepted_name),]) # nothing showing up here I think its being included in tree matrix...
  invasives <- rbind(invasives, dat[grep('Ligustrum', dat$accepted_name),])                          
  invasives <- rbind(invasives, dat[grep('Euonymus',  dat$accepted_name),])    
  invasives <- rbind(invasives, dat[grep('Rosa multiflora', dat$accepted_name),])    
  invasives <- rbind(invasives, dat[grep('Lythrum salicaria', dat$accepted_name),])    
  invasives <- rbind(invasives, dat[grep('Dipsacus', dat$accepted_name),])    
  invasives <- rbind(invasives, dat[grep('Celastrus orbiculatus', dat$accepted_name),])    
  invasives <- rbind(invasives, dat[grep('Ficaria verna', dat$accepted_name),])
  
  plot_invasive_cover <- c()
  for (p in row.names(plots.env)){
    if(p %in% invasives$plot){
      plot_invasive_cover <- c(plot_invasive_cover, sum(invasives$spp_percent_total_cover[which(invasives$plot == p)]))
    }
    else{
      plot_invasive_cover <- c(plot_invasive_cover, 0)
    }
  }
  plots.env$InvCover <- plot_invasive_cover
  newdf <- plots.env['InvCover']
  return(newdf)
}

get_dominant_tree_group <- function(tree.dat){
  # -----------
  # returns a genus name for whichever tree species occupies the most plot cover
  # -----------
  tree.dat$genus <- gsub(' .*', '', tree.dat$accepted_name)
  
  tree.dat$cover <- as.numeric(tree.dat$cover)
  tree.dat <- get_plot_cover(tree.dat)
  
  tree_type_plots <- data.frame(unique(tree.dat$plot))
  dom_genus <- c()
  for (plt in unique(tree.dat$plot)){
    this_plot <- tree.dat[which(tree.dat$plot == plt),]
    temp <- as.data.frame(aggregate(this_plot$spp_percent_total_cover, by = list(this_plot$genus), FUN = sum))
    temp$x <- as.numeric(temp$x)
    temp <- temp[order(-temp$x),]
    dom_genus <- c(dom_genus, temp[1, 'Group.1'])
  }
  tree_type_plots$tree_type <- as.factor(dom_genus)
  return(tree_type_plots)
}


