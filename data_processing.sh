# run all data processing scripts 
# 07-10-2019

# Includes (in this order) the following operations
# 	1. Read in survey data and combine into one dataframe (dat.all) 
# 	2. Clean the species names of this dataframe using TNRS 
# 	3. Generate the phylogeny for diversity metrics using clean species names 
# 	4. Process environmental data for all plots 
#	5. Calculate plot cover (total cover and tree cover) add to environmental data matrix
#	6. Generate the community matrix (presence absence)

Rscript scripts/00.combine_spp_pools.R 
echo 'dat.all generated' 
Rscript scripts/01.clean_spp_names.R
echo 'dat.all cleaned'
Rscript scripts/02.generate_phylogeny.R
echo 'phylogeny generated'
Rscript scripts/04.envt_data.R
echo 'plots.env generated'
Rscript scripts/05.make_community_matrix.R
echo 'community matrix has been made'
echo 'DONE!'
