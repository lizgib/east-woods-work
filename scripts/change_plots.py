# doing this in python because my first thought was dictionaries

import pandas as pd

clust_07 = pd.read_csv('outputs/clust_07.csv')
clust_18 = pd.read_csv('outputs/clust_18.csv')

for i in clust_07:
    print(clust_07.iloc[:,i])
