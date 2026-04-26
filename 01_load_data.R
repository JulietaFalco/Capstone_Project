# ══════════════════════════════════════════════════════════════
# STEP 1 — Load Data
# ══════════════════════════════════════════════════════════════

if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr",  quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("skimr",  quietly = TRUE)) install.packages("skimr")

library(readxl)
library(dplyr)
library(skimr)

# Load datasets
Dataset_Train_v1 <- read_excel("C:/Users/Usuario/mi-proyecto-ml/Dataset_Train_v1.xlsx")
Dataset_Test_v1  <- read_excel("C:/Users/Usuario/mi-proyecto-ml/Dataset_Test_v1.xlsx")

cat("✓ Step 1 complete — Data loaded\n")
cat("  Train:", nrow(Dataset_Train_v1), "rows x", ncol(Dataset_Train_v1), "cols\n")
cat("  Test: ", nrow(Dataset_Test_v1),  "rows x", ncol(Dataset_Test_v1),  "cols\n")