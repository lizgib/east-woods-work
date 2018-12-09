# east-woods-work
Work done over summer 2018 URF program at the Morton Arboretum. Includes survey data processing, phylogenetic metrics, and community analyses



SCRIPT DESCRIPTIONS: 
Morton Project: June 2018 - December 2018


01.combine_spp_pools.R

	Input: Reads in all the survey data from 2007 (spring and summer surveys) and 2018 (also spring 
	and summer surveys)
	Outputs: Generates 3 dataframes: dat.07, dat.18. and dat.all. These include all tree, shrub, and
	herb species reported in the data as well as the following: 
	
	plot = ID for survey plot 
	code = first three letters of genus, first three letters of species 
	species = reported genus and species ID
	cover = differs for each vegetative type. 
		herbs = percent cover (out of 100% plot cover)
		trees = DBH
		shrubs = # live stems
	datset = keeps track of which survey dataset the species was recorded in (survey broken into 
	three datasets T = tree layer, H = herb layer, S = shrub layer) 
	year = 2007 or 2018 
	
	"plot"    "code"       "species"    "cover"    "datset"   "year"  
	A107	  QUEALB     Quercus alba      54.6 	   T 	   2007 
	
	
02.falltranslationkey.R	

	Input: Reads in the dat.all object
	Output: Returns dat.all with new column "accepted_name"
	- Finds the TNRS accepted name for all species ID in survey data 
	

03.compare_years.R 
	
	Input: Accepts all three dataframes: dat.07, dat.18, and dat.all
	Output: Table of all unique species from both years and record of which were present in 
	2007 only, 2018 only, and those present in both 2007 and 2018
	- compares the which species were identified in 2007 vs 2018 (gets a count for each) 
	  across the whole East Woods
	
04.natives.R

	Input: dat.all dataframe with all species recorded in 2007 and 2018, USDA plants 
	record of invasive and native species in Illinois, csv file of remaining species 
	missing native ID which have been manually assigned native/invasive 
	Output: new column added to dat.all for nativestatus (n = native, i = invasive, 
	x = exotic, NA = no ID)
	- gives each species a native/nonnative identifier based on USDA plants data and manual 
	  identification

05.cover.R 

	Input: dat.all 
	Output: understory.all.csv, trees.all.csv
	- computes total plot cover and the relative percents of total cover for each species
	  within a plot. Also converts tree cover from DBH to Basal Area 
	  
06.envt_data.R 
	
	Input: understory.all.csv, trees.all.csv, various GIS files from Morton GIS drive 
	Output: liz_data.csv dataframe 
	-extracts environmental variables from each source and does a few calculations to get
	 items like invasive ratio, soil index, drainage, and canopy cover. Full list of variables
	 incorporated: 
	 	1. Latitude 
	 	2. Longitude
	 	3. Soil Texture (Categorical)
	 	4. Soil Drainage (Categorical)
	 	5. Canopy07 (Basal Area) 
	 	6. Canopy18 (Basal Area) 
	 	7. Plot Burn Frequency 
	 	8. Slope
	 	9. Aspect 
	 	10. Elevation 
	 	11. Invasive Ratio 07 
	 	12. Invasive Ratio 18 
	 	13. Dominant Tree Genus 
	 	14. Soil Index (Calculated)
	 	15. Plot Drainage (Calculated) 
	 	16. Area Name (East Woods or Hidden Lake) 
	 	17. Community Class (type of community) 
	 	*18. Plan to incorporate a %ACM/ECM trees ratio soon 
	 	
07.tree_generation.R

	Input: dat.all.csv
	Output: tr.ewv4 
	-uses a quick and dirty tree generation approach by slicing and appending tips from 
	 a large pregenerated tree (phylo.zanne.tre) to create phylogeny for East Woods species 
	 from both 2007 and 2018 species pools
	 
08.community_data_matrix.pres_abs.R

	Input: dat.all.csv
	Output: dat.mat.07, dat.mat.18
	-generates two community matrices for 2007 and 2018 on a presence absence basis (binary)
	 *may later also generate matrices for different layers (dat.mat.understory.18 and 
	 dat.mat.trees.18) 
	 
09.phylo_metrics.R 

	Input: community matrices, tr.ewv4 
	Output: appends 4 more columns to liz_data (SR07, PD07, SR18, PD18) 
	-calculates plot level phylogenetic diversity using picante and ape 
		ses.pd and ses.mntd --> gives us phylogenetic diversity, number of taxa, and 
		phylo-betadiversity

10.phylo_diss_matrices.R 

	Input: 
	Output: 
	-calculates the plot dissimilarity for each environmental variable and diversity metric.
	 Uses Jaccard for species richness, comdist and comdistnt for phylogenetic diversity, 
	 and euclidean distances for environmental variables 
	 
11.analyzing_data.R

	Input: liz_data.csv
	Output: 
	-generates some preliminary plots and analyses looking at the distributions of various 
	 environmental factors and their immediate effects on diversity 
	 
12.multiple_regression.R 
	
	Input: liz_data.csv
	Output: Rsq table
	
13.mantel.R

	Input: dissimilarity matrices 
	Output: partial Rsq table 
	-performs a Multiple Mantel test on all plot dissimilarities to evaluate 
	 partial R2 of each 
	 
14.spp_correlation.R
 	
 	Input: community matrices, liz_data.csv
 	Output: heatmap 
 	-determines the correlations between the presence of each species and the presence of 
 	 each environmetal factor. Generates a heatmap for visualizing phylogenetic signal of 
 	 these correlations (does one clade respond more strongly to a particular variable than
 	 other clades?) 
 	 
15.make_plots.R 
	
	Input: liz_data.csv
	Output: plots 
	-generates plots from each of the regression analyses
 	
 
 	 
	 	
	
	
	

			
										

