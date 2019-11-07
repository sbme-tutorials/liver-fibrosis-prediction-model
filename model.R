# Load and inspect datset
hcv_data <- read.csv("./dataset/HCV-Egy-Data.csv")
summary(hcv_data)
head(hcv_data)

# Load libraries
library(dplyr)
library(arules)

# Discretization
# WBC
hcv_dis <- hcv_data %>% mutate (WBC =
                                  cut(
                                    WBC,
                                    breaks = c(0, 4000, 11000, 12101),
                                    labels = c(1, 2, 3)
                                  ))