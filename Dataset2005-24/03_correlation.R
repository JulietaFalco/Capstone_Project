# ============================================================
# 03_CORRELATION.R
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
