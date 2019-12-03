source("./src/discretize.R")
source("./src/knn.R")
source("./src/naive_bayes.R")
























































































# # Converting numeric variables to factors
# hcv_data_dis[sapply(hcv_data_dis, is.numeric)] <-
#   lapply(hcv_data_dis[sapply(hcv_data_dis, is.numeric)], as.factor)

# # Check types for each attribute
# sapply(hcv_data_dis, class)

# # List the levels for the class
# levels(hcv_data_dis$Baselinehistological.staging)



# # Missing Values
# sum(is.na(hcv_data_dis))
# mean(is.na(hcv_data_dis))
# missing_values <- hcv_data_dis %>% dplyr::filter_all(any_vars(is.na(.)))
# hcv_data_dis <- na.omit(hcv_data_dis)

# # Splitting the data
# ran <- sample(1:nrow(hcv_data_dis), 0.8 * nrow(hcv_data_dis)) 
# hcv_train <- hcv_data_dis[ran,] 
# hcv_test <- hcv_data_dis[-ran,]

# # knn Model
# knn_model <- class::knn(hcv_train,hcv_test,cl=hcv_train$Baselinehistological.staging,k=10)

# # knn Model with cv
# indx <- sapply(hcv_data_dis, is.factor)
# hcv_data_dis[indx] <- lapply(hcv_data_dis[indx], function(x) as.numeric(as.character(x)))
# knn_cv_model <- FNN::knn.cv(hcv_data_dis[,-29],hcv_data_dis[,29],k=10)

# # Evaluation
# knn_confusion_matrix <- table(knn_model,hcv_test$Baselinehistological)
# knn_confusion_matrix
# knn_accuracy<- mean(knn_model == hcv_test$Baselinehistological)
# knn_accuracy

# knn_cv_confusion_matrix <- table(knn_cv_model,hcv_data_dis[,29])
# knn_cv_confusion_matrix
# knn_cv_accuracy<- mean(knn_cv_model == hcv_data_dis[,29])
# knn_cv_accuracy