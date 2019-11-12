# Load and inspect datset
install.packages("data.table")
hcv_data <- read.csv("./data/raw/HCV-Egy-Data.csv")
summary(hcv_data)

head(hcv_data)
colnames(hcv_data)

# Discretization
source("./src/discretize.R")
discretize(hcv_data, "WBC", c(0, 4000, 11000, 12101), c(1, 2, 3))
discretize(hcv_data,
           "Age",
           c(0, 32, 37, 42, 47, 52, 57, 62),
           c(1, 2, 3, 4, 5, 6, 7))
discretize(hcv_data, "BMI", c(0, 18.5, 25, 30, 35, 40), c(1, 2, 3, 4, 5))
discretize(hcv_data, "RBC", c(0, 3000000, 5000000, 5018451), c(1, 2, 3))
discretize(hcv_data, "Plat", c(93013, 100000, 255000, 226465), c(1, 2, 3))
discretize(hcv_data, "AST.1", c(0, 20, 40, 128), c(1, 2, 3))
discretize(hcv_data, "ALT4", c(0, 20, 40, 128), c(1, 2, 3))
discretize(hcv_data, "ALT.1", c(0, 20, 40, 128), c(1, 2, 3))
discretize(hcv_data, "ALT.12", c(0, 20, 40, 128), c(1, 2, 3))
discretize(hcv_data, "ALT.24", c(0, 20, 40, 128), c(1, 2, 3))
discretize(hcv_data, "ALT.36", c(0, 20, 40, 128), c(1, 2, 3))
discretize(hcv_data, "ALT.48", c(0, 20, 40, 128), c(1, 2, 3))
discretize(hcv_data, "ALT.after.24.w", c(0, 20, 40, 128), c(1, 2, 3))
discretize(hcv_data, "RNA.Base", c(0, 5, 1201086), c(1, 2))
discretize(hcv_data, "RNA.4", c(0, 5, 1201715), c(1, 2))
discretize(hcv_data, "RNA.12", c(0, 5, 3731527), c(1, 2))
discretize(hcv_data, "RNA.EOT", c(0, 5, 808450), c(1, 2))
discretize(hcv_data, "RNA.EF", c(0, 5, 808450), c(1, 2))
if (hcv_data$Gender == 1) {
  discretize(hcv_data, "HGB", c(0, 14, 17.5, 20), c(1, 2, 3))
} else if (hcv_data$Gender == 2)  {
  discretize(hcv_data, "HGB", c(0, 12.3, 15.3, 20), c(1, 2, 3))
}

# Save discretized data
write.table(hcv_data_dis, file = "./data/processed/hcv-data-dis.csv",
            sep = ",", row.names = F)

# Converting numeric variables to factors
hcv_data_dis[sapply(hcv_data_dis, is.numeric)] <-
  lapply(hcv_data_dis[sapply(hcv_data_dis, is.numeric)], as.factor)

# Check types for each attribute
sapply(hcv_data_dis, class)

# List the levels for the class
levels(hcv_data_dis$Baselinehistological.staging)

# Summarize the Baselinehistological.staging class distribution
percentage <- prop.table(table(hcv_data_dis$Baselinehistological.staging)) * 100
cbind(freq=table(hcv_data_dis$Baselinehistological.staging), percentage=percentage)

# Missing Values
sum(is.na(hcv_data_dis))
mean(is.na(hcv_data_dis))
missing_values <- hcv_data_dis %>% dplyr::filter_all(any_vars(is.na(.)))
hcv_data_dis <- na.omit(hcv_data_dis)

# Splitting the data
ran <- sample(1:nrow(hcv_data_dis), 0.8 * nrow(hcv_data_dis)) 
hcv_train <- hcv_data_dis[ran,] 
hcv_test <- hcv_data_dis[-ran,]

# knn Model
knn_model <- class::knn(hcv_train,hcv_test,cl=hcv_train$Baselinehistological.staging,k=10)

# knn Model with cv
indx <- sapply(hcv_data_dis, is.factor)
hcv_data_dis[indx] <- lapply(hcv_data_dis[indx], function(x) as.numeric(as.character(x)))
knn_cv_model <- FNN::knn.cv(hcv_data_dis[,-29],hcv_data_dis[,29],k=10)

# Evaluation
knn_confusion_matrix <- table(knn_model,hcv_test$Baselinehistological)
knn_confusion_matrix
knn_accuracy<- mean(knn_model == hcv_test$Baselinehistological)
knn_accuracy

knn_cv_confusion_matrix <- table(knn_cv_model,hcv_data_dis[,29])
knn_cv_confusion_matrix
knn_cv_accuracy<- mean(knn_cv_model == hcv_data_dis[,29])
knn_cv_accuracy