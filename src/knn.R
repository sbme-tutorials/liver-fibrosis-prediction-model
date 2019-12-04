# Load Processed dataset
hcv_data_dis <- read.csv("./data/processed/hcv-data-dis.csv")

# Load Libraries and functions
library(naivebayes)
source("./src/confusion_matrix.R")

# Randomly shuffle the data
hcv_data_dis <- hcv_data_dis[sample(nrow(hcv_data_dis)),]

# Create 10 equally size folds
folds <-
  cut(seq(1, nrow(hcv_data_dis)), breaks = 10, labels = FALSE)

# Perform 10 fold cross validation

# knn model
total_micro_accuracy <-0
total_accuracy <- 0
for (i in 1:10) {
  # Segement data by fold
  testIndexes <- which(folds == i, arr.ind = TRUE)
  hcv_test <- hcv_data_dis[testIndexes, ]
  hcv_train <- hcv_data_dis[-testIndexes, ]
  # Use the test and train data partitions
  knn_model <-
    class::knn(hcv_train,
               hcv_test,
               cl = hcv_train$Baselinehistological.staging,
               k = 37)
  # Get Accuracy
  acc <- confusion_matrix(knn_model,hcv_test$Baselinehistological.staging)
  total_accuracy <-
    total_accuracy + acc[1]
  total_micro_accuracy <-    total_accuracy + acc[2]
    print("----------------------------")

}
print(paste("Average Acuuracy of knn = ", total_accuracy / 10))
print(paste("Average Micro Acuuracy of knn = ", total_micro_accuracy / 10))
