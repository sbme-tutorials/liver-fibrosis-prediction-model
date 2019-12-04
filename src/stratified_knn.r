library(plyr)
# Load Processed dataset
hcv_data_dis <- read.csv("./data/processed/hcv-data-dis.csv")

# Load Libraries and functions
library(naivebayes)
source("./src/confusion_matrix.R")



# Perform 10 fold cross validation
createFolds <- function(x,k){
    n <- nrow(x)
    x$folds <- rep(1:k,length.out = n)[sample(n,n)]
    x
}

str_data <- ddply(hcv_data_dis,.(hcv_data_dis$Baselinehistological.staging),createFolds,k = 10 )
# knn model
total_accuracy <- 0
for (i in 1:10) {
  # Segement data by fold
  testIndexes <- which(str_data$folds == i, arr.ind = TRUE)
  hcv_test <- str_data[testIndexes, ]
  hcv_train <- str_data[-testIndexes, ]
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
}
print(paste("Average Acuuracy of knn = ", total_accuracy / 10))
print(paste("Average Micro Acuuracy of knn = ", total_micro_accuracy / 10))