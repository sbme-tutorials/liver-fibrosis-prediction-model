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

# Naive bayes model
total_accuracy <- 0
for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  hcv_test <- hcv_data_dis[testIndexes, ]
  hcv_train <- hcv_data_dis[-testIndexes, ]
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
print(paste("Average Acuuracy of naive = ", total_accuracy / 10))
# warnings()

