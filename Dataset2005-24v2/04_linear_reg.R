# ============================================================
# 04_LINEAR_REG.R
# Linear Regression with Z-score normalisation
# Dataset: 2005-2024 v2
# ============================================================

library(Metrics)
library(dplyr)

# ── Normalisation (Z-score) ───────────────────────────────────
# Using TRAIN mean and sd only to avoid data leakage
train_means <- colMeans(X_train)
train_sds   <- apply(X_train, 2, sd)

X_train_scaled <- as.data.frame(scale(X_train, center = train_means, scale = train_sds))
X_test_scaled  <- as.data.frame(scale(X_test,  center = train_means, scale = train_sds))

train_scaled <- X_train_scaled
train_scaled$ProdPerBusiness <- y_train

cat("=== Normalisation complete ===\n")
cat("Mean of first feature (should be ~0):", round(mean(X_train_scaled[,1]), 4), "\n")
cat("SD of first feature (should be ~1):  ", round(sd(X_train_scaled[,1]), 4), "\n\n")

# ── Train model ───────────────────────────────────────────────
lm_model <- lm(ProdPerBusiness ~ ., data = train_scaled)

# ── AIC ───────────────────────────────────────────────────────
lm_aic <- AIC(lm_model)

# ── Predictions ───────────────────────────────────────────────
pred_train_lm <- predict(lm_model, newdata = X_train_scaled)
pred_test_lm  <- predict(lm_model, newdata = X_test_scaled)

# ── Metrics ───────────────────────────────────────────────────
cat("==============================\n")
cat("  LINEAR REGRESSION RESULTS\n")
cat("==============================\n")
cat("AIC:       ", round(lm_aic, 2), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_lm), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_lm)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_lm), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_lm), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_lm)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_lm), 2), "\n")

# ── Feature importance (p-values) ────────────────────────────
lm_summary <- summary(lm_model)
lm_coef    <- as.data.frame(lm_summary$coefficients)
lm_coef$Feature <- rownames(lm_coef)
lm_coef <- lm_coef[lm_coef$Feature != "(Intercept)", ]
lm_coef <- lm_coef[order(lm_coef$`Pr(>|t|)`), ]

cat("\nTop 15 significant features (by p-value):\n")
print(round(head(lm_coef[, c("Estimate", "Pr(>|t|)")], 15), 4))

cat("\nRainfall lag and price features:\n")
lag_price <- c("Rainfall_growing_season_lag1", "Rainfall_annual_lag1",
               "Nitrogen_price", "Wheat_PriceAvgAprDec")
print(round(lm_coef[lm_coef$Feature %in% lag_price,
                    c("Estimate", "Pr(>|t|)")], 4))

# ── Actual vs Predicted plot ──────────────────────────────────
plot(y_test, pred_test_lm,
     main = "Linear Regression - Actual vs Predicted (Test)",
     xlab = "Actual ProdPerBusiness",
     ylab = "Predicted ProdPerBusiness",
     pch  = 16, col = "steelblue")
abline(0, 1, col = "red", lwd = 2)

# ── Store AIC for comparison ──────────────────────────────────
model_comparison <- data.frame(
  Model     = "Linear Regression",
  AIC       = round(lm_aic, 2),
  Train_R2  = round(cor(y_train, pred_train_lm)^2, 4),
  Test_RMSE = round(rmse(y_test, pred_test_lm), 2),
  Test_R2   = round(cor(y_test, pred_test_lm)^2, 4)
)

cat("\n=== MODEL COMPARISON (so far) ===\n")
print(model_comparison, row.names = FALSE)
