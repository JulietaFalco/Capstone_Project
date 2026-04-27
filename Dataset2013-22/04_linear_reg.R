# ══════════════════════════════════════════════════════════════
# STEP 4 — Baseline Linear Regression
# ══════════════════════════════════════════════════════════════

if (!requireNamespace("caret",   quietly = TRUE)) install.packages("caret")
if (!requireNamespace("Metrics", quietly = TRUE)) install.packages("Metrics")

library(caret)
library(Metrics)
library(ggplot2)

# ── 1. Normalize features (fit on TRAIN only) ─────────────────
scaler <- preProcess(
  Dataset_Train_v2 %>% select(-ProdPerBusiness),
  method = c("center", "scale")
)

Train_scaled <- predict(scaler, Dataset_Train_v2)
Test_scaled  <- predict(scaler, Dataset_Test_v2)

cat("✓ Normalization complete\n")

# ── 2. Fit baseline linear regression ─────────────────────────
lm_model <- lm(ProdPerBusiness ~ ., data = Train_scaled)

# ── 3. Model summary ──────────────────────────────────────────
cat("\n=== LINEAR REGRESSION SUMMARY ===\n")
print(summary(lm_model))

# ── 4. AIC ────────────────────────────────────────────────────
cat("\n=== AIC ===\n")
cat("AIC:", AIC(lm_model), "\n")

# ── 5. Train performance ──────────────────────────────────────
train_pred <- predict(lm_model, Train_scaled)
train_rmse <- rmse(Train_scaled$ProdPerBusiness, train_pred)
train_mae  <- mae(Train_scaled$ProdPerBusiness,  train_pred)
train_r2   <- cor(Train_scaled$ProdPerBusiness,  train_pred)^2

# ── 6. Test performance ───────────────────────────────────────
test_pred <- predict(lm_model, Test_scaled)
test_rmse <- rmse(Test_scaled$ProdPerBusiness,  test_pred)
test_mae  <- mae(Test_scaled$ProdPerBusiness,   test_pred)
test_r2   <- cor(Test_scaled$ProdPerBusiness,   test_pred)^2

# ── 7. Train vs Test comparison ───────────────────────────────
cat("\n=== TRAIN vs TEST PERFORMANCE ===\n")
comparison <- data.frame(
  Dataset = c("Train", "Test"),
  RMSE    = c(round(train_rmse, 2), round(test_rmse, 2)),
  MAE     = c(round(train_mae,  2), round(test_mae,  2)),
  R2      = c(round(train_r2,   4), round(test_r2,   4))
)
print(comparison)

# ── 8. Actual vs Predicted plot ───────────────────────────────
plot_df <- data.frame(
  Actual    = Test_scaled$ProdPerBusiness,
  Predicted = test_pred
)

ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Linear Regression — Actual vs Predicted (Test Set)",
       x     = "Actual ProdPerBusiness",
       y     = "Predicted ProdPerBusiness") +
  theme_minimal()

cat("\n✓ Step 4 complete — Baseline Linear Regression done\n")