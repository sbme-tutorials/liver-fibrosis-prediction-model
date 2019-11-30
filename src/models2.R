source("./src/confusion_matrix.R")
# Load Processed dataset
hcv_data_dis <- read.csv("./data/processed/hcv-data-dis.csv")
library(naivebayes)

# Randomly shuffle the data
hcv_data_dis <- hcv_data_dis[sample(nrow(hcv_data_dis)),]

# Create 10 equally size folds
folds <-
  cut(seq(1, nrow(hcv_data_dis)), breaks = 10, labels = FALSE)

# Perform 10 fold cross validation
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
               k = 10)
  accuracy <- confusion_matrix(knn_model,hcv_test$Baselinehistological.staging)
  total__accuracy <-
    total_accuracy + accuracy
}

print(paste("Average Acuuracy of knn = ", total_accuracy / 10))
print("-------------------------------")

# naive bayes model
total_accuracy <- 0
for (i in 1:10) {
  # Segement your data by fold
  testIndexes <- which(folds == i, arr.ind = TRUE)
  hcv_test <- hcv_data_dis[testIndexes, ]
  hcv_train <- hcv_data_dis[-testIndexes, ]
  # Use the test and train data partitions
  
  naive_model <-
    naive_bayes(
      x = hcv_train,
      y = as.factor(hcv_train$Baselinehistological.staging),
      laplace = 1
    )
  naive_predicted <- predict(naive_model , hcv_test)
  accuracy <- confusion_matrix(naive_predicted,hcv_test$Baselinehistological.staging)
  total_accuracy <-
    total_accuracy + accuracy
}
print(paste("Average Acuuracy  of naive = ", total_accuracy / 10))
warnings()
