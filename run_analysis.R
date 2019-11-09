# Load and inspect datset
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
