# making a shell script to keep track of which EcoStructure runs are performed 

# ARGS: 
# Rscript ecostructure.R {community matrix 2007} {community matrix 2018} {k} {WGS shapefile path} 

#Rscript scripts/ecostructure.R dat.mat.all.07 dat.mat.all.18 2 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp 
#Rscript scripts/ecostructure.R dat.mat.all.07 dat.mat.all.18 3 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R dat.mat.all.07 dat.mat.all.18 4 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R dat.mat.all.07 dat.mat.all.18 5 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp

#Rscript scripts/ecostructure.R dat.mat.understory.07 dat.mat.understory.18 2 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R dat.mat.understory.07 dat.mat.understory.18 3 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R dat.mat.understory.07 dat.mat.understory.18 4 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R dat.mat.understory.07 dat.mat.understory.18 5 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp

#Rscript scripts/ecostructure.R summer.07 summer.18 2 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R summer.07 summer.18 3 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R summer.07 summer.18 4 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R summer.07 summer.18 5 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp

#Rscript scripts/ecostructure.R summer.under.07 summer.under.18 2 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R summer.under.07 summer.under.18 3 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R summer.under.07 summer.under.18 4 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp
#Rscript scripts/ecostructure.R summer.under.07 summer.under.18 5 ~/Documents/GIS/WGS/WGS_MortArb_boundary_line.shp


# break up the communites that emerged in other ecostructure runs into more communities

Rscript scripts/ecostructure.R group1.07.mat group1.18.mat 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/ecostructure.R group1.07.mat group1.18.mat 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/ecostructure.R group2.07.mat group2.18.mat 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/ecostructure.R group2.07.mat group2.18.mat 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/ecostructure.R group3.07.mat group3.18.mat 2 ~/Documents/GIS/WGS/WGS_merged_roads.shp
Rscript scripts/ecostructure.R group3.07.mat group3.18.mat 3 ~/Documents/GIS/WGS/WGS_merged_roads.shp

