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
 confusion_knn <- table(as.factor(knn_model) , as.factor(hcv_test$Baselinehistological.staging))
 #calculate accuracy 
 predicted_true <- confusion_knn[1,1]+confusion_knn[2,2]+confusion_knn[3,3]+confusion_knn[4,4]
confusion_acuur_knn <- predicted_true/sum(confusion_knn)
#calculate SP and SV
T1 <- confusion_knn[1,1]
T2 <- confusion_knn[2,2]
T3 <- confusion_knn[3,3]
T4 <- confusion_knn[4,4]

F1 <-  sum(confusion_knn[1,2:4])
F2 <- confusion_knn[2,1] + confusion_knn[2,3] + confusion_knn[2,4]
F3 <- confusion_knn[3,1] +  confusion_knn[3,2] + confusion_knn[3,4]
F4 <- confusion_knn[4,1] +  confusion_knn[4,2] + confusion_knn[4,3]

F1_n <-  confusion_knn[2,1] + confusion_knn[3,1] +  confusion_knn[4,1]
F2_n <- confusion_knn[1,2] + confusion_knn[3,2] +  confusion_knn[4,2]
F3_n <- confusion_knn[1,3] + confusion_knn[2,3] +  confusion_knn[4,3]
F4_n <- confusion_knn[1,4] + confusion_knn[3,4] +  confusion_knn[2,4]

T1_n <- confusion_knn[2,2]+confusion_knn[3,3]+confusion_knn[4,4]
T2_n <- confusion_knn[1,1]+confusion_knn[3,3]+confusion_knn[4,4]
T3_n<- confusion_knn[1,1]+confusion_knn[2,2]+confusion_knn[4,4]
T4_n <- confusion_knn[1,1]+confusion_knn[2,2]+confusion_knn[3,3]

# for class 1 
SV1 = T1/(T1+F1_n)
SP1 = T1_n/(T1_n+F1)
# for class 2
SV2 = T2/(T2+F2_n)
SP2 = T2_n/(T2_n+F2)
# for class 3 
SV3 = T3/(T3+F3_n)
SP3 = T3_n/(T3_n+F3)
# for class 4 
SV4 = T4/(T4+F4_n)
SP4 = T4_n/(T4_n+F4)
print(SP1)
print(SV1)

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
  
naive_model <- naive_bayes(x= hcv_train, y=as.factor(hcv_train$Baselinehistological.staging), laplace=1)
#naive_model <- naive_bayes(as.factor(hcv_train$Baselinehistological.staging) ~ ., data = hcv_train , laplace=1)
naive_predicted <- predict(naive_model , hcv_test)

# get the confusion matrix    
 confusion_naive <- table(as.factor(naive_predicted) , as.factor(hcv_test$Baselinehistological.staging))

# calculate SV  and  SP 
T1 <- confusion_naive[1,1]
T2 <- confusion_naive[2,2]
T3 <- confusion_naive[3,3]
T4 <- confusion_naive[4,4]

F1 <-  sum(confusion_naive[1,2:4])
F2 <- confusion_naive[2,1] + confusion_naive[2,3] + confusion_naive[2,4]
F3 <- confusion_naive[3,1] +  confusion_naive[3,2] + confusion_naive[3,4]
F4 <- confusion_naive[4,1] +  confusion_naive[4,2] + confusion_naive[4,3]

F1_n <-  confusion_naive[2,1] + confusion_naive[3,1] +  confusion_naive[4,1]
F2_n <- confusion_naive[1,2] + confusion_naive[3,2] +  confusion_naive[4,2]
F3_n <- confusion_naive[1,3] + confusion_naive[2,3] +  confusion_naive[4,3]
F4_n <- confusion_naive[1,4] + confusion_naive[3,4] +  confusion_naive[2,4]

T1_n <- confusion_naive[2,2]+confusion_naive[3,3]+confusion_naive[4,4]
T2_n <- confusion_naive[1,1]+confusion_naive[3,3]+confusion_naive[4,4]
T3_n<- confusion_naive[1,1]+confusion_naive[2,2]+confusion_naive[4,4]
T4_n <- confusion_naive[1,1]+confusion_naive[2,2]+confusion_naive[3,3]

# for class 1 
SV1 = T1/(T1+F1_n)
SP1 = T1_n/(T1_n+F1)
# for class 2
SV2 = T2/(T2+F2_n)
SP2 = T2_n/(T2_n+F2)
# for class 3 
SV3 = T3/(T3+F3_n)
SP3 = T3_n/(T3_n+F3)
# for class 4 
SV4 = T4/(T4+F4_n)
SP4 = T4_n/(T4_n+F4)
print(SP1)
print(SV1)

# calculate accuracy 
 predicted_true <- confusion_naive[1,1]+confusion_naive[2,2]+confusion_naive[3,3]+confusion_naive[4,4]
confusion_acuur_naive <- predicted_true/sum(confusion_naive)
print(paste("Accuracy=" , confusion_acuur_naive))
  naive_accuracy <-
    naive_accuracy + confusion_acuur_naive
}
print(paste("Average Acuuracy  of naive = ", naive_accuracy/10))
warnings()
