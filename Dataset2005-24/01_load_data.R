# ============================================================
# 01_LOAD_DATA.R
# Wheat Production Forecasting | 2005-2024
# Split: 80% Train (2005-2021) / 20% Test (2022-2024)
# ============================================================

library(readxl)
library(dplyr)

# Load dataset
df_raw <- read_excel("C:/Users/Usuario/mi-proyecto-ml/Dataset2005-24/Dataset_2005-24.xlsx")

cat("Dataset loaded:", nrow(df_raw), "rows x", ncol(df_raw), "columns\n")
cat("Years:", sort(unique(df_raw$YEAR)), "\n")
cat("Stations:", length(unique(df_raw$station_code)), "\n")
cat("\nTarget - ProdPerBusiness summary:\n")
print(summary(df_raw$ProdPerBusiness))
cat("\nPrice column names:\n")
print(names(df_raw)[grep("Price|Nitrogen|Wheat", names(df_raw))])
