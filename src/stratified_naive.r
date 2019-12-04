library(plyr)
# Load Processed dataset
hcv_data_dis <- read.csv("./data/processed/hcv-data-dis.csv")

# Load Libraries and functions
library(naivebayes)
source("./src/confusion_matrix.R")



# Perform 10 fold-stratified cross validation
createFolds <- function(x,k){
    n <- nrow(x)
    x$folds <- rep(1:k,length.out = n)[sample(n,n)]
    x
}

str_data <- ddply(hcv_data_dis,.(hcv_data_dis$Baselinehistological.staging),createFolds,k = 10 )
# Naive bayes model
total_micro_accuracy <- 0
total_accuracy <- 0
for (i in 1:10) {
  testIndexes <- which(str_data$folds == i, arr.ind = TRUE)
  hcv_test <- str_data[testIndexes, ]
  hcv_train <- str_data[-testIndexes, ]
  naive_model <-
    naive_bayes(
      x = hcv_train,
      y = as.factor(hcv_train$Baselinehistological.staging),
      laplace = 1
    )
  naive_predicted <- predict(naive_model , hcv_test)
  acc <- confusion_matrix(naive_predicted,hcv_test$Baselinehistological.staging)
  total_accuracy <-
    total_accuracy + acc[1]
  total_micro_accuracy <-    total_micro_accuracy + acc[2]
}
print(paste("Average Acuuracy of knn = ", total_accuracy / 10))
print(paste("Average Micro Acuuracy of knn = ", total_micro_accuracy / 10))
# warnings()

