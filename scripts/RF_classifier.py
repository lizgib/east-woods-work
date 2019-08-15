'''
writing the machine learning script using sklearn since I don't have a lot
of time left and it is more familiar to me here
'''

from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.metrics import classification_report, confusion_matrix
from sklearn.model_selection import RandomizedSearchCV
from sklearn import metrics
import pandas as pd
import sys, os
import numpy as np
from sklearn import tree
import graphviz

df = sys.argv[1]
pred_val = sys.argv[2]
outfile = sys.argv[3]

# Get the data ready
X = pd.read_csv(df, index_col = 0, header = 0)
y = X[pred_val]
X = X.drop([pred_val], axis = 1)

if pred_val == 'ComClass18':
    balanced = {1:[], 2:[], 3:[], 4:[]}
    for p in range(len(y)):
        if y[p] ==1:
            balanced[1].append(y.index.values[p])
        elif y[p] == 2:
            balanced[2].append(y.index[p])
        elif y[p] == 3:
            balanced[3].append(y.index[p])
        elif y[p] == 4:
            balanced[4].append(y.index[p])

    grab_plots = []
    for k in balanced.keys():
        if len(balanced[k]) >= 50: # chose 50 because there should be 47 disturbed plots this should jsut grab all of them (they are smallest group)
            chosen_ones = np.random.choice(balanced[k], 50)
        else:
            chosen_ones = balanced[k]
            print('not enough plots in ', k, ' just grabbing all of them')
        grab_plots.extend(chosen_ones)
else:
        balanced = {0:[], 1:[]}
        for p in range(len(y)):
            if y[p] == 0:
                balanced[0].append(y.index.values[p])
            elif y[p] == 1:
                balanced[1].append(y.index[p])
        grab_plots = []
        for k in balanced.keys():
            if len(balanced[k]) >= 50: # chose 50 because there should be 47 disturbed plots this should jsut grab all of them (they are smallest group)
                chosen_ones = np.random.choice(balanced[k], 50)
            else:
                chosen_ones = balanced[k]
                print('not enough plots in ', k, ' just grabbing all of them')
            grab_plots.extend(chosen_ones)

X_bal = X.loc[grab_plots]
y_bal = y.loc[grab_plots]

X_train, X_test, y_train, y_test = train_test_split(X_bal, y_bal, test_size = 0.3, random_state = 17)

###########################
# Hyperparameter tuning
###########################
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


# for i in range(len(balanced_ids)):
#     print("  Round %s of %s"%(j+1,len(balanced_ids)))
#     clf = RandomForestClassifier(args.n_estimators, args.max_depth, args.max_features)
# 	if args.test!='':
# 		result,current_scores,result_test = ML.fun.BuildModel_Apply_Performance(df1, clf, args.cv_num, df_notSel, apply_unk, df_unknowns, test_df, classes, args.pos, NEG, j, args.alg,args.threshold_test)
# 		results_test.append(result_test)
# 	else:
# 		result,current_scores = ML.fun.BuildModel_Apply_Performance(df1, clf, args.cv_num, df_notSel, apply_unk, df_unknowns, test_df, classes, args.pos, NEG, j, args.alg,args.threshold_test)
#
# 	results.append(result)


clf = RandomForestClassifier(n_estimators = 1200, max_features = 'auto', max_depth = 180, random_state = 17)
clf.fit(X_train, y_train)
y_pred = clf.predict(X_test)


rfc_cv_score = cross_val_score(clf, X_bal, y_bal, cv=10, scoring= 'roc_auc')
print("=== Confusion Matrix ===")
print(confusion_matrix(y_test, y_pred))
print('\n')
print("=== Classification Report ===")
print(classification_report(y_test, y_pred))
print('\n')
print("=== All AUC Scores ===")
print(rfc_cv_score)
print('\n')
print("=== Mean AUC Score ===")
print("Mean AUC Score - Random Forest: ", rfc_cv_score.mean())



# Feature Importance
feature_imp = feature_imp = pd.Series(clf.feature_importances_,index=X_train.columns.values).sort_values(ascending=False)
print(feature_imp)
feature_imp.to_csv(outfile)
