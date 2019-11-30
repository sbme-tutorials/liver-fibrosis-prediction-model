confusion_matrix <- function(prediction,true){
  # get the confusion matrix
  cm <-
    table(as.factor(prediction) ,
          as.factor(true))
  #calculate accuracy
  predicted_true <-
    cm[1, 1] + cm[2, 2] + cm[3, 3] + cm[4, 4]
  accuracy <- predicted_true / sum(cm)
  #calculate SP and SV
  T1 <- cm[1, 1]
  T2 <- cm[2, 2]
  T3 <- cm[3, 3]
  T4 <- cm[4, 4]
  
  F1 <-  sum(cm[1, 2:4])
  F2 <- cm[2, 1] + cm[2, 3] + cm[2, 4]
  F3 <- cm[3, 1] +  cm[3, 2] + cm[3, 4]
  F4 <- cm[4, 1] +  cm[4, 2] + cm[4, 3]
  
  F1_n <-
    cm[2, 1] + cm[3, 1] +  cm[4, 1]
  F2_n <-
    cm[1, 2] + cm[3, 2] +  cm[4, 2]
  F3_n <-
    cm[1, 3] + cm[2, 3] +  cm[4, 3]
  F4_n <-
    cm[1, 4] + cm[3, 4] +  cm[2, 4]
  
  T1_n <- cm[2, 2] + cm[3, 3] + cm[4, 4]
  T2_n <- cm[1, 1] + cm[3, 3] + cm[4, 4]
  T3_n <- cm[1, 1] + cm[2, 2] + cm[4, 4]
  T4_n <- cm[1, 1] + cm[2, 2] + cm[3, 3]
  
  # for class 1
  SV1 = T1 / (T1 + F1_n)
  SP1 = T1_n / (T1_n + F1)
  # for class 2
  SV2 = T2 / (T2 + F2_n)
  SP2 = T2_n / (T2_n + F2)
  # for class 3
  SV3 = T3 / (T3 + F3_n)
  SP3 = T3_n / (T3_n + F3)
  # for class 4
  SV4 = T4 / (T4 + F4_n)
  SP4 = T4_n / (T4_n + F4)
  print(SP1)
  print(SV1)
  
  print(paste("Accuracy=" , accuracy))
  return(accuracy)
}
