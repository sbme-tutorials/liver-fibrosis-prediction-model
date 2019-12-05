<h1> Liver Fibrosis Prediction Model</h1>

**File Breakdown:**

- `EDA.R`: R file for exploratory data analysis.
- `discretize.R`: Contains functions for data discretization and saving the discretized data in /data/processed/hcv-data-dis.csv
- `confusion_matrix.R`: Contains functions responisable for calculation of sensitivity, specificity and accuracy. (**src file for all following files**)
- `knn.R`: KNN Model with cross validation (k-folds = 10)
- `stratified_knn.R`: KNN model with stratified k-folds.
- `naive_bayes.R`: Naive Bayes model with cross validation (k-folds = 10)
- `stratified_naive.R`: Naive Bayes model with stratified k-folds.

This project is done as a requirement for SBE304 course.
Please refer to [this link](https://github.com/aligamalelgaml/Team15) to view the repo for our [website](https://aligamalelgaml.github.io/Team15/).
