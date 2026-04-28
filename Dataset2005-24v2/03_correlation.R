# ============================================================
# 03_CORRELATION.R
# Correlation of ALL features with ProdPerBusiness
# ============================================================

library(corrplot)
library(dplyr)

# Correlation of ALL features with target
cor_data   <- train
cor_matrix <- cor(cor_data, use = "complete.obs")

# Correlation with ProdPerBusiness only - sorted
cor_target <- sort(cor_matrix[,"ProdPerBusiness"], decreasing = TRUE)
cor_target <- cor_target[names(cor_target) != "ProdPerBusiness"]

cat("=== Correlation of ALL features with ProdPerBusiness ===\n")
print(round(cor_target, 3))

cat("\n=== Top 30 POSITIVE correlations ===\n")
print(round(head(cor_target, 30), 3))

cat("\n=== Top 30 NEGATIVE correlations ===\n")
print(round(tail(cor_target, 30), 3))

# Correlation of key variables plot
key_vars <- c(
  "LATITUDE", "LONGITUDE",
  "Rainfall_growing_season_lag1", "Rainfall_annual_lag1",
  "Nitrogen_price", "Wheat_PriceAvgAprDec",
  "AirTempAvg_month_8", "AirTempAvg_month_9", "AirTempAvg_month_3",
  "WindAvg_month_10", "WindAvg_month_9",
  "Rainfall_month_7", "Rainfall_month_8",
  "ProdPerBusiness"
)

cor_data_key   <- train %>% select(all_of(key_vars))
cor_matrix_key <- cor(cor_data_key, use = "complete.obs")

corrplot(cor_matrix_key,
         method      = "color",
         type        = "upper",
         tl.cex      = 0.7,
         tl.col      = "black",
         addCoef.col = "black",
         number.cex  = 0.6,
         title       = "Correlation Matrix - Key Variables",
         mar         = c(0,0,2,0))
