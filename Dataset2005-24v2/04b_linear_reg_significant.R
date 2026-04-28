# ============================================================
# 04b_LINEAR_REG_SIGNIFICANT.R
# Linear Regression with significant features only (p < 0.05)
# ============================================================

library(Metrics)
library(dplyr)

# ── Significant features (p < 0.05) from 04_linear_reg.R ─────
sig_features <- c(
  "WindAvg_month_7",
  "Rainfall_month_4",
  "Evaporation_month_1",
  "AirTempAvg_month_5",
  "Radiation_month_8",
  "WindAvg_month_10",
  "Radiation_month_1",
  "Rainfall_month_11",
  "Radiation_month_6",
  "Evaporation_month_8",
  "AirTempAvg_month_9",
  "WindAvg_month_9",
  "RelHumAvg_month_9",
  "AirTempAvg_month_8",
  "RelHumAvg_month_7"
)

cat("Features used:", length(sig_features), "\n")
print(sig_features)

# ── Prepare data ──────────────────────────────────────────────
X_train_sig <- X_train %>% select(all_of(sig_features))
X_test_sig  <- X_test  %>% select(all_of(sig_features))

# ── Normalisation ─────────────────────────────────────────────
train_means_s <- colMeans(X_train_sig)
train_sds_s   <- apply(X_train_sig, 2, sd)

X_train_scaled_s <- as.data.frame(scale(X_train_sig, center = train_means_s, scale = train_sds_s))
X_test_scaled_s  <- as.data.frame(scale(X_test_sig,  center = train_means_s, scale = train_sds_s))

train_scaled_s <- X_train_scaled_s
train_scaled_s$ProdPerBusiness <- y_train

# ── Train model ───────────────────────────────────────────────
lm_sig_model <- lm(ProdPerBusiness ~ ., data = train_scaled_s)

# ── AIC ───────────────────────────────────────────────────────
lm_sig_aic <- AIC(lm_sig_model)

# ── Predictions ───────────────────────────────────────────────
pred_train_lm_s <- predict(lm_sig_model, newdata = X_train_scaled_s)
pred_test_lm_s  <- predict(lm_sig_model, newdata = X_test_scaled_s)

# ── Metrics ───────────────────────────────────────────────────
cat("\n==============================\n")
cat("  LINEAR REGRESSION - SIG FEATURES\n")
cat("  (p < 0.05 | 15 features)\n")
cat("==============================\n")
cat("AIC:       ", round(lm_sig_aic, 2), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_lm_s), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_lm_s)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_lm_s), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_lm_s), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_lm_s)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_lm_s), 2), "\n")

# ── Feature importance ────────────────────────────────────────
lm_sig_summary <- summary(lm_sig_model)
lm_sig_coef    <- as.data.frame(lm_sig_summary$coefficients)
lm_sig_coef$Feature <- rownames(lm_sig_coef)
lm_sig_coef <- lm_sig_coef[lm_sig_coef$Feature != "(Intercept)", ]
lm_sig_coef <- lm_sig_coef[order(lm_sig_coef$`Pr(>|t|)`), ]

cat("\nAll features (sorted by p-value):\n")
print(round(lm_sig_coef[, c("Estimate", "Pr(>|t|)")], 4))

# ── Actual vs Predicted plot ──────────────────────────────────
plot(y_test, pred_test_lm_s,
     main = "LM Significant Features - Actual vs Predicted (Test)",
     xlab = "Actual ProdPerBusiness",
     ylab = "Predicted ProdPerBusiness",
     pch  = 16, col = "steelblue")
abline(0, 1, col = "red", lwd = 2)

# ── Update model comparison ───────────────────────────────────
model_comparison <- rbind(model_comparison, data.frame(
  Model     = "LM Significant (15 feat)",
  AIC       = round(lm_sig_aic, 2),
  Train_R2  = round(cor(y_train, pred_train_lm_s)^2, 4),
  Test_RMSE = round(rmse(y_test, pred_test_lm_s), 2),
  Test_R2   = round(cor(y_test, pred_test_lm_s)^2, 4)
))

cat("\n=== MODEL COMPARISON (so far) ===\n")
print(model_comparison, row.names = FALSE)
