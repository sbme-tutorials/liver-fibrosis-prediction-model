# Load and inspect datset
hcv_data <- read.csv("./dataset/HCV-Egy-Data.csv")
summary(hcv_data)
head(hcv_data)

# Discretization
# WBC
library(dplyr)
hcv_dis<- mutate (hcv_data,WBC = ifelse (WBC <= 4000, 0, 
                  ifelse(WBC > 4000 & WBC <= 11000, 1,2)))
