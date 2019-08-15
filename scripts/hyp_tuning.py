# '''
# Script for tuning hyperparameters for Random Forest Classifier
# '''
#
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.model_selection import train_test_split
# from sklearn.model_selection import cross_val_score
# from sklearn.metrics import classification_report, confusion_matrix
# from sklearn.model_selection import RandomizedSearchCV
# from sklearn import metrics
# import pandas as pd
# import sys, os
# import numpy as np
#
# df = sys.argv[1]
# pred_val = sys.argv[2]
# outfile = sys.argv[3]
#
# # Get the data ready
# X = pd.read_csv(df, index_col = 0, header = 0)
# y = X[pred_val]
# X = X.drop([pred_val], axis = 1)
#
# if pred_val == 'ComClass18':
#     balanced = {1:[], 2:[], 3:[], 4:[]}
#     for p in range(len(y)):
#         if y[p] ==1:
#             balanced[1].append(y.index.values[p])
#         elif y[p] == 2:
#             balanced[2].append(y.index[p])
#         elif y[p] == 3:
#             balanced[3].append(y.index[p])
#         elif y[p] == 4:
#             balanced[4].append(y.index[p])
#
#     grab_plots = []
#     for k in balanced.keys():
#         if len(balanced[k]) >= 50: # chose 50 because there should be 47 disturbed plots this should jsut grab all of them (they are smallest group)
#             chosen_ones = np.random.choice(balanced[k], 50)
#         else:
#             chosen_ones = balanced[k]
#             print('not enough plots in ', k, ' just grabbing all of them')
#         grab_plots.extend(chosen_ones)
# else:
#         balanced = {0:[], 1:[]}
#         for p in range(len(y)):
#             if y[p] == 0:
#                 balanced[0].append(y.index.values[p])
#             elif y[p] == 1:
#                 balanced[1].append(y.index[p])
#         grab_plots = []
#         for k in balanced.keys():
#             if len(balanced[k]) >= 50: # chose 50 because there should be 47 disturbed plots this should jsut grab all of them (they are smallest group)
#                 chosen_ones = np.random.choice(balanced[k], 50)
#             else:
#                 chosen_ones = balanced[k]
#                 print('not enough plots in ', k, ' just grabbing all of them')
#             grab_plots.extend(chosen_ones)
#
# X_bal = X.loc[grab_plots]
# y_bal = y.loc[grab_plots]
#
# X_train, X_test, y_train, y_test = train_test_split(X_bal, y_bal, test_size = 0.3, random_state = 17)
#
# ###########################
# # Hyperparameter tuning
# ###########################
# # number of trees in random forest
# n_estimators = [int(x) for x in np.linspace(start = 200, stop = 2000, num = 10)]
# # number of features at every split
# max_features = ['auto', 'sqrt']
#
# # max depth
# max_depth = [int(x) for x in np.linspace(100, 500, num = 11)]
# max_depth.append(None)
# # create random grid
# random_grid = {
#  'n_estimators': n_estimators,
#  'max_features': max_features,
#  'max_depth': max_depth
#  }
# # Random search of parameters
# clf_random = RandomizedSearchCV(estimator = clf, param_distributions = random_grid, n_iter = 100, cv = 3, verbose=2, random_state=42, n_jobs = -1)
# # Fit the model
# clf_random.fit(X_train, y_train)
# # print results
# print(clf_random.best_params_)
