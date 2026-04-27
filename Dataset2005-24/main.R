# ============================================================
# MAIN.R - Wheat Production Forecasting Pipeline
# Dataset: 2005-2024 | Target: ProdPerBusiness
# Split: 80% Train (2005-2021) / 20% Test (2022-2024)
# ============================================================

# Install packages if needed (run once):
# install.packages(c("readxl","dplyr","ggplot2","corrplot",
#                    "glmnet","rpart","rpart.plot",
#                    "randomForest","Metrics"))

setwd("C:/Users/Usuario/mi-proyecto-ml/Dataset2005-24")

source("01_load_data.R")
#source("02_clean_data.R")
#source("03_correlation.R")
#source("04_linear_reg.R")
#source("05_lasso.R")
#source("06_decision_tree.R")
#source("06b_decision_tree_top_features.R")
#source("07_random_forest.R")
