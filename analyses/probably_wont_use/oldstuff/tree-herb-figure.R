#herb tree PD plot

source('SCRIPTS/newstuff/01.2.community_data_matrices_cover.R')

tree_pd <- pd(dat.mat.trees.c, tr.ewv3)
herbs_pd <-pd(dat.mat.herbs.c, tr.ewv3)

temp_herbs <- herbs_pd[intersect(rownames(herbs_pd), rownames(tree_pd)),]
temp_trees <- tree_pd[intersect(rownames(herbs_pd), rownames(tree_pd)),]

ggplot()+
  geom_point(aes(temp_trees$PD, temp_herbs$PD), color = 'darkgreen') + 
  xlab(' ') + 
  ylab(' ')

ggplot()+
  geom_point(aes(temp_trees$SR, temp_herbs$SR), color = 'darkgreen') + 
  xlab(' ') + 
  ylab(' ')

