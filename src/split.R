hcv_data_dis <- read.csv("./data/processed/hcv-data-dis.csv")
library(naivebayes)
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

# get the confusion matrix    
 confusion_knn <- table(as.factor(knn_model) , as.factor(hcv_test$Baselinehistological))
 predicted_true <- confusion_knn[1,1]+confusion_knn[2,2]+confusion_knn[3,3]+confusion_knn[4,4]
confusion_acuur_knn <- predicted_true/sum(confusion_knn)
print(paste("Accuracy=" , confusion_acuur_knn))
  knn_accuracy <-
    knn_accuracy + confusion_acuur_knn
}

print(paste("Average Acuuracy of knn = ", knn_accuracy/10))


# naive bayes model 
naive_accuracy <- 0
for (i in 1:10) {
  #Segement your data by fold using the which() function
  testIndexes <- which(folds == i, arr.ind = TRUE)
  hcv_test <- hcv_data_dis[testIndexes,]
  hcv_train <- hcv_data_dis[-testIndexes,]
  #Use the test and train data partitions however you desire...
  
naive_model <- naive_bayes(x= hcv_train, y=as.factor(hcv_train$Baselinehistological), laplace=1)
#naive_model <- naive_bayes(as.factor(hcv_train$Baselinehistological.staging) ~ ., data = hcv_train , laplace=1)
naive_predicted <- predict(naive_model , hcv_test)

# get the confusion matrix    
 confusion_naive <- table(as.factor(naive_predicted) , as.factor(hcv_test$Baselinehistological))
 confusion_naive
 predicted_true <- confusion_naive[1,1]+confusion_naive[2,2]+confusion_naive[3,3]+confusion_naive[4,4]
confusion_acuur_naive <- predicted_true/sum(confusion_naive)
print(paste("Accuracy=" , confusion_acuur_naive))
  naive_accuracy <-
    naive_accuracy + confusion_acuur_naive
}
print(paste("Average Acuuracy  of naive = ", naive_accuracy/10))
warnings()
