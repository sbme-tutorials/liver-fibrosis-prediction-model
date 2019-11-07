# Load and inspect datset
hcv_data <- read.csv("./dataset/HCV-Egy-Data.csv")
summary(hcv_data)
head(hcv_data)

# Load libraries
library(dplyr)
library(arules)
library(rlang) # for sym()


# Discretization

discretize <- function(data, column_name, A, B) {
  column_sym <- sym(column_name)
  hcv_dis <<- hcv_dis %>% mutate(!!column_sym :=
                                   cut(data[[column_name]],
                                       breaks = c(A),
                                       labels = c(B)))
  return(hcv_dis)
}


discretize(hcv_data, "WBC", c(0, 4000, 11000, 12101), c(1, 2, 3))
discretize(hcv_data,
           "Age",
           c(0, 32, 37, 42, 47, 52, 57, 62),
           c(1, 2, 3, 4, 5, 6, 7))

discretize(hcv_data,"BMI",c(0,18.5,25,30,35,40), c(1,2,3,4,5))
discretize(hcv_data,"RBC",c(0,3000000,5000000,5018451),c(1,2,3))
discretize(hcv_data,"Plat",c(93013, 100000,255000,226465),c(1,2,3))
discretize(hvc_data,"AST1",c(0,20,40,128),c(1,2,3))
discretize(hvc_data,"ALT1",c(0,20,40,128),c(1,2,3))
discretize(hvc_data,"ALT4",c(0,20,40,128),c(1,2,3))
discretize(hvc_data,"ALT12",c(0,20,40,128),c(1,2,3))
discretize(hvc_data,"ALT24",c(0,20,40,128),c(1,2,3))
discretize(hvc_data,"ALT36",c(0,20,40,128),c(1,2,3))
discretize(hvc_data,"ALT48",c(0,20,40,128),c(1,2,3))
