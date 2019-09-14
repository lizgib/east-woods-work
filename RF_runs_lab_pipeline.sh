# don't really trust some of my output so gonna try running it wiht 
# the Shiu Lab pipeline to make sure i did everything ok 

python ../ML-Pipeline/ML_classification.py -df data/MachineLearning/Matrix/species_change.csv -alg RF -sep ',' -y_name Change  -cm t -plots t


