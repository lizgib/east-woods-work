# east-woods-work
## OVERVIEW: 
This repository contains primary scripts used to perform analyses on East Woods survey data to assess community structure and changes over the last 11 years. Scripts include some data cleaning, diversity calculations, and community analyses (which make use of EcoStructure package found here: https://github.com/kkdey/ecostructure. 
### Contact:
Elizabeth Gibbons | gibbon70@msu.edu | gibbonsliz7@yahoo.com 


## WORKFLOW:
00.combine_spp_pools.R
	- combines all survey data from spring and summer surveys in 2007 and 2018 into compatible format
	
01.falltranslationkey.R	
	- Finds TNRS accepted name for species ID from raw survey data
	
02.compare_years.R

03.generate_phylogeny.R 
	- generates phylogeny using supertree approach, pruning tips from supertree from Zanne et al	

04.cover.R 
	- function to calculates percent of total cover for each species reported in plot. This function also calculates an estimate for how much cover there is in a plot (summing all cover estimates collected in survey data
	- additional function to get estimate of which tree genus is dominant in plot (based on which tree species has most abundant cover)

05.envt_data.R 
	- gathers collection of plot level environmental variable from various sources on the East Woods google drive. Includes topography based variables and hillshade extracted from GIS rasters as well as some management and 
		
06.community_matrix.R
    - generates presence absence based community matrix
	 
07.diversity_metrics.R
    - calculates plot level alpha and beta diversity using species richness, and phylogenetic diversity metrics (PD, MPD, MNTD, PSV)
	 
08.spp_correlation.R 
    - look for phylogenetic signal by determining correlation of each species with selected continuous environmental variables and map the R2 on phylogeny 

09.ecostructure.R 
    - apply the EcoStructure model

10.define_ecostructure_communities.R
    - assign each plot to a community based on its membership in each cluster and investigate environmental variables in plots from each of these groups. 
	
11.ordination.R 
    - another look at the difference between plots using NMDS
	
12.what_is_difference_bt_communities.R
    - Determine if there is a significant difference between groups for any variables
	 
13.difference_bt_years.R
    - determine which plots have changed community class between years 

14.what_predicts_change.R
    - investigate what environmental variables may be associated with that changing community class in plots between 2007 and 2018


 	
 
 	 
	 	
	
	
	

			
										

