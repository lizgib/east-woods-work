'''
writing the machine learning script using sklearn since I don't have a lot
of time left and it is more familiar to  me here
'''

from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import cross_val_score, GridSearchCV
from sklearn.model_selection import train_test_split
from sklearn import metrics
import pandas as pd
import sys, os


df = sys.argv[1]
pred_val = sys.argv[2]
outfile = sys.argv[3]

# Get the data ready
X = pd.read_csv(df, index_col = 0, header = 0)
y = pd.read_csv('data/MachineLearning/Pred/pred_values.csv', index_col = 0, header = 0)
y = y[pred_val]

for i in y.index.values:
    if i not in X.index.values:
        y = y.drop([i], axis = 0)

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
    if len(balanced[k]) >= 47: # chose 47 because there should be 47 disturbed plots this should jsut grab all of them (they are smallest group)
        chosen_ones = np.random.choice(balanced[k], 47)
    else:
        chosen_ones = balanced[k]
        print('not enough genes in ', k, ' just grabbing all of them')
    grab_plots.extend(chosen_ones)

X_bal = X.loc[grab_plots]

X_train, X_test, y_train, y_test = train_test_split(X_bal, y, test_size = 0.3)
print(X_train.head())
rgr = RandomForestRegressor(n_estimators = 100)

rgr.fit(X_train, y_train)
y_pred = rgr.predict(X_test)

print(y_pred)
print("Accuracy:",metrics.accuracy_score(y_test, y_pred))

# Feature Importance
feature_imp = feature_imp = pd.Series(clf.feature_importances_,index=X_train.columns.values).sort_values(ascending=False)
print(feature_imp)
feature_imp.to_csv(outfile)
