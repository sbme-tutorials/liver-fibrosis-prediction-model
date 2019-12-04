library(ggplot2)
library(praznik)
hcv_data <- read.csv("./data/raw/HCV-Egy-Data.csv")
par(mfrow = c(3:2))
boxplot(ALT.12~factor(Baselinehistological.staging) , data=hcv_data ,  xlab="Baseline.histological.Staging", ylab="ALT.12")
boxplot(ALT.24~factor(Baselinehistological.staging) , data=hcv_data ,  xlab="Baseline.histological.Staging", ylab="ALT.24")
boxplot(ALT.36~factor(Baselinehistological.staging) , data=hcv_data ,  xlab="Baseline.histological.Staging", ylab="ALT.36")
boxplot(ALT.48~factor(Baselinehistological.staging) , data=hcv_data ,  xlab="Baseline.histological.Staging", ylab="ALT.48")
boxplot(ALT.after.24.w~factor(Baselinehistological.staging) , data=hcv_data ,  xlab="Baseline.histological.Staging", ylab="ALT.after.24.w")
boxplot(AST.1~factor(Baselinehistological.staging) , data=hcv_data ,  xlab="Baseline.histological.Staging", ylab="AST.1")
boxplot(ALT4~factor(Baselinehistological.staging) , data=hcv_data ,  xlab="Baseline.histological.Staging", ylab="ALT4")

# Summarize the Baselinehistological.staging class distribution
percentage <- prop.table(table(hcv_data$Baselinehistological.staging)) * 100
cbind(freq=table(hcv_data$Baselinehistological.staging), percentage=percentage)


#First Feature Selection 
data_set <- read.csv("./data/processed/hcv-data-dis.csv")


F_test <- MRMR(data_set[-29] , data_set$Baselinehistological.staging , 28)
max(F_test$score)
which(F_test$score == max(F_test$score), arr.ind = TRUE)

# Second feature selection by deleting the first selected 
F_test <- MRMR(data_set[-29][-28] , data_set$Baselinehistological.staging , 27)
max(F_test$score)
which(F_test$score == max(F_test$score), arr.ind = TRUE)

#Upon Results the 2 features selected are Baseline.histological.Grading and Age 
Staging <- as.factor(data_set$Baselinehistological.staging)
  ggplot(data_set, aes(x=Baseline.histological.Grading, y=Age , color= Staging))+
  geom_jitter(width=0.25 , alpha= 0.5)
 
# qplot(Baseline.histological.Grading, Age, data=data_set, color=as.factor(Baselinehistological.staging))
  ggplot(data_set, aes(x=Baseline.histological.Grading, y=Age))+
  geom_jitter(width=0.25 , alpha = 0.5)+
  facet_wrap(~Baselinehistological.staging)



# #First Feature Selection 

# F_test <- MRMR(hcv_data[-29] , hcv_data$Baselinehistological.staging , 28)
# max(F_test$score)
# which(F_test$score == max(F_test$score), arr.ind = TRUE)

# # Second feature selection by deleting the first selected 
# F_test <- MRMR(hcv_data[-29][-23] , hcv_data$Baselinehistological.staging , 27)
# max(F_test$score)
# which(F_test$score == max(F_test$score), arr.ind = TRUE)

# #Upon Results the 2 features selected are Baseline.histological.Grading and Age 
# qplot(RNA.Base , RNA.4 , data=hcv_data, color=as.factor(Baselinehistological.staging))
# qplot(Baseline.histological.Grading, Age, data=hcv_data, color=as.factor(Baselinehistological.staging))
# ggplot(hcv_data, aes(x=Baseline.histological.Grading, y=Age))+
#   geom_point()+
#   facet_wrap(~Baselinehistological.staging)
#   ggplot(hcv_data, aes(x=RNA.Base, y=RNA.4))+
#   geom_point()+
#   facet_wrap(~Baselinehistological.staging)
