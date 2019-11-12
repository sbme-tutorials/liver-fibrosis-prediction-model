hcv_data_dis <- read.csv("./data/processed/hcv-data-dis.csv")

# split train and test data in an 80/20 proportion
#set.seed(100)
#hcv_data_dis[, "train"] <- ifelse(runif(nrow(hcv_data_dis))<= 0.2, 1,
#                                 ifelse(runif(nrow(hcv_data_dis))<= 0.4 & runif(nrow(hcv_data_dis)) >0.2,2,
#                                       ifelse(runif(nrow(hcv_data_dis))<= 0.6 & runif(nrow(hcv_data_dis)) >0.4,3,
#                                             ifelse(runif(nrow(hcv_data_dis))<= 0.8 & runif(nrow(hcv_data_dis)) >0.6,4,5))))

#Randomly shuffle the data
hcv_data_dis <- hcv_data_dis[sample(nrow(hcv_data_dis)), ]

#Create 10 equally size folds
folds <- cut(seq(1, nrow(hcv_data_dis)), breaks = 10, labels = FALSE)
folds

knn_accuracy <- 0
#Perform 10 fold cross validation
for (i in 1:10) {
  #Segement your data by fold using the which() function
  testIndexes <- which(folds == i, arr.ind = TRUE)
  hcv_test <- hcv_data_dis[testIndexes,]
  hcv_train <- hcv_data_dis[-testIndexes,]
  #Use the test and train data partitions however you desire...
  
  knn_model <-
    class::knn(hcv_train,
               hcv_test,
               cl = hcv_train$Baselinehistological.staging,
               k = 10)
  print(mean(knn_model == hcv_test$Baselinehistological))
  knn_accuracy <-
    knn_accuracy + mean(knn_model == hcv_test$Baselinehistological)
}
accuracy <- knn_accuracy / 10
