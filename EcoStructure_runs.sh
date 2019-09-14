# making a shell script to keep track of which EcoStructure runs are performed 

# ARGS: 
# Rscript ecostructure.R {community matrix 2007} {community matrix 2018} {k} {WGS shapefile path} 

# Rscript scripts/ecostructure.R dat.mat.all.07 dat.mat.all.18 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/09.ecostructure.R dat.mat.all.07 dat.mat.all.18 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R dat.mat.all.07 dat.mat.all.18 4 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R dat.mat.all.07 dat.mat.all.18 5 ~/Documents/GIS/WGS/WGS_merged_roads.shp

# Rscript scripts/ecostructure.R dat.mat.understory.07 dat.mat.understory.18 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/09.ecostructure.R dat.mat.understory.07 dat.mat.understory.18 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R dat.mat.trees.07 dat.mat.trees.18 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/ecostructure.R dat.mat.trees.07 dat.mat.trees.18 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp

# Rscript scripts/ecostructure.R summer.07 summer.18 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/09.ecostructure.R summer.07 summer.18 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R summer.07 summer.18 4 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R summer.07 summer.18 5 ~/Documents/GIS/WGS/WGS_merged_roads.shp

# also going to break up these communities based on Hidden Lake and East Woods plots

# Rscript scripts/ecostructure.R hl.07 hl.18 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/09.ecostructure.R hl.07 hl.18 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R ew.07 ew.18 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/09.ecostructure.R ew.07 ew.18 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp

# I could also throw in comparing the East Woods understory vs Hidden Lake understory or EW trees vs HL trees, but
# I think that is a little complicated at this time for my schedule

# break up the communites that emerged in other ecostructure runs into more communities

# Rscript scripts/ecostructure.R group1.07.mat group1.18.mat 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R group1.07.mat group1.18.mat 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R group2.07.mat group2.18.mat 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R group2.07.mat group2.18.mat 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R group3.07.mat group3.18.mat 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
# Rscript scripts/ecostructure.R group3.07.mat group3.18.mat 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp

