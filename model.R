# Load and inspect datset
hcv_data <- read.csv("./dataset/HCV-Egy-Data.csv")
summary(hcv_data)
head(hcv_data)

# Load libraries
library(dplyr)
library(arules)
library(rlang) # for sym()


# Discretization

discretize <- function(feature,column_name,A,B) {
  column_sym <- sym(column_name)
  hcv_dis <<- hcv_dis %>% mutate(!!column_sym :=
                                     cut(
                                       feature,
                                       breaks = c(A),
                                       labels = c(B)
                                     ))
  return(hcv_dis)
}


discretize(hcv_data$WBC,"WBC",c(0, 4000, 11000, 12101),c(1, 2, 3))
discretize(hcv_data$Age,"Age",c(0, 32, 37, 42,47,52,57,62),c(1, 2, 3,4,5,6,7))
