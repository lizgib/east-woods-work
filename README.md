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
	A107			QUEALB			Quercus alba	 54.6 			   T 			 2007 
	
	
02.falltranslationkey.R	

	Input: Reads in the dat.all object
	Output: Returns dat.all with new column "accepted_name"
	

03.compare_years.R 
	
	Input: Accepts all three dataframes: dat.07, dat.18, and dat.all
	Output: Table of all unique species from both years and record of which were present in 
	2007 only, 2018 only, and those present in both 2007 and 2018
	

			
										

