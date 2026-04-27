# ══════════════════════════════════════════════════════════════
# STEP 7 — Random Forest
# ══════════════════════════════════════════════════════════════

if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("Metrics",      quietly = TRUE)) install.packages("Metrics")
if (!requireNamespace("ggplot2",      quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr",        quietly = TRUE)) install.packages("dplyr")

library(randomForest)
library(Metrics)
library(ggplot2)
library(dplyr)

# ── 1. Train Random Forest ────────────────────────────────────
set.seed(123)

# Calculate default mtry for regression = p/3
n_features   <- ncol(Train_scaled) - 1   # exclude target
default_mtry <- max(1, floor(n_features / 3))

cat("=== RANDOM FOREST SETUP ===\n")
cat("Number of features:", n_features, "\n")
cat("Default mtry (p/3):", default_mtry, "\n")

rf_model <- randomForest(
  ProdPerBusiness ~ .,
  data        = Train_scaled,
  ntree       = 500,
  mtry        = default_mtry,
  importance  = TRUE,
  keep.forest = TRUE
)

cat("\n=== RANDOM FOREST MODEL SUMMARY ===\n")
print(rf_model)

# ── 2. OOB error plot ─────────────────────────────────────────
oob_df <- data.frame(
  trees = 1:rf_model$ntree,
  oob   = rf_model$mse
)

ggplot(oob_df, aes(x = trees, y = sqrt(oob))) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Random Forest — OOB RMSE vs Number of Trees",
       x     = "Number of Trees",
       y     = "OOB RMSE") +
  theme_minimal()

# ── 3. Feature importance ─────────────────────────────────────
importance_rf <- data.frame(
  feature         = rownames(importance(rf_model)),
  IncMSE          = importance(rf_model)[, "%IncMSE"],
  IncNodePurity   = importance(rf_model)[, "IncNodePurity"]
) %>%
  arrange(desc(IncMSE))

cat("\n=== TOP 20 FEATURE IMPORTANCE (%IncMSE) ===\n")
print(head(importance_rf, 20))

# Plot top 20 by %IncMSE
importance_rf %>%
  head(20) %>%
  ggplot(aes(x = reorder(feature, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  coord_flip() +
  labs(title = "Random Forest — Top 20 Feature Importance (%IncMSE)",
       x     = "Feature",
       y     = "% Increase in MSE") +
  theme_minimal()

# Plot top 20 by IncNodePurity
importance_rf %>%
  arrange(desc(IncNodePurity)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(feature, IncNodePurity), y = IncNodePurity)) +
  geom_bar(stat = "identity", fill = "darkorange", color = "white") +
  coord_flip() +
  labs(title = "Random Forest — Top 20 Feature Importance (Node Purity)",
       x     = "Feature",
       y     = "Increase in Node Purity") +
  theme_minimal()

# ── 4. Tune mtry ──────────────────────────────────────────────
cat("\n=== TUNING mtry ===\n")
set.seed(123)

mtry_values  <- c(3, 5, 10, 15, 20, 25)
mtry_results <- data.frame()

for(m in mtry_values){
  rf_m <- randomForest(
    ProdPerBusiness ~ .,
    data        = Train_scaled,
    ntree       = 500,
    mtry        = m,
    importance  = FALSE
  )

  # OOB RMSE
  oob_rmse <- sqrt(rf_m$mse[500])
  
  # Test RMSE
  test_pred_m  <- predict(rf_m, Test_scaled)
  test_rmse_m  <- rmse(Test_scaled$ProdPerBusiness, test_pred_m)
  test_r2_m    <- cor(Test_scaled$ProdPerBusiness,  test_pred_m)^2
  
  mtry_results <- rbind(mtry_results, data.frame(
    mtry      = m,
    OOB_RMSE  = round(oob_rmse,   2),
    Test_RMSE = round(test_rmse_m, 2),
    Test_R2   = round(test_r2_m,   4)
  ))
  
  cat("mtry =", m, "| OOB RMSE:", round(oob_rmse, 2), 
      "| Test RMSE:", round(test_rmse_m, 2),
      "| Test R²:", round(test_r2_m, 4), "\n")
}

cat("\n=== MTRY TUNING RESULTS ===\n")
print(mtry_results)

# Plot mtry tuning
mtry_results %>%
  tidyr::pivot_longer(
    cols      = c(OOB_RMSE, Test_RMSE),
    names_to  = "Metric",
    values_to = "RMSE"
  ) %>%
  ggplot(aes(x = mtry, y = RMSE, color = Metric, group = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  scale_color_manual(values = c("OOB_RMSE"  = "steelblue",
                                "Test_RMSE" = "tomato")) +
  labs(title = "Random Forest — OOB vs Test RMSE by mtry",
       x     = "mtry (features per split)",
       y     = "RMSE") +
  theme_minimal()

# ── 5. Best model with optimal mtry ───────────────────────────
best_mtry <- mtry_results$mtry[which.min(mtry_results$Test_RMSE)]
cat("\n=== BEST mtry ===\n")
cat("Best mtry:", best_mtry, "\n")

set.seed(123)
rf_best <- randomForest(
  ProdPerBusiness ~ .,
  data        = Train_scaled,
  ntree       = 500,
  mtry        = best_mtry,
  importance  = TRUE
)

# ── 6. Train performance ──────────────────────────────────────
train_pred_rf <- predict(rf_best, Train_scaled)
train_rmse_rf <- rmse(Train_scaled$ProdPerBusiness, train_pred_rf)
train_mae_rf  <- mae(Train_scaled$ProdPerBusiness,  train_pred_rf)
train_r2_rf   <- cor(Train_scaled$ProdPerBusiness,  train_pred_rf)^2

# ── 7. Test performance ───────────────────────────────────────
test_pred_rf <- predict(rf_best, Test_scaled)
test_rmse_rf <- rmse(Test_scaled$ProdPerBusiness,  test_pred_rf)
test_mae_rf  <- mae(Test_scaled$ProdPerBusiness,   test_pred_rf)
test_r2_rf   <- cor(Test_scaled$ProdPerBusiness,   test_pred_rf)^2

# ── 8. Train vs Test comparison ───────────────────────────────
cat("\n=== TRAIN vs TEST PERFORMANCE (Best RF) ===\n")
comparison_rf <- data.frame(
  Dataset = c("Train", "Test"),
  RMSE    = c(round(train_rmse_rf, 2), round(test_rmse_rf, 2)),
  MAE     = c(round(train_mae_rf,  2), round(test_mae_rf,  2)),
  R2      = c(round(train_r2_rf,   4), round(test_r2_rf,   4))
)
print(comparison_rf)

# ── 9. Full model comparison ──────────────────────────────────
cat("\n=== FULL MODEL COMPARISON ===\n")
full_comparison <- data.frame(
  Model     = c("Linear Regression", "Lasso", 
                "Decision Tree (cp=0.045)", 
                "Decision Tree (cp=0.01)",
                "Random Forest"),
  Test_RMSE = c(1833.00, 1705.54, 1338.01, 1347.46,
                round(test_rmse_rf, 2)),
  Test_MAE  = c(1303.63, 1233.54, 999.45,  999.45,
                round(test_mae_rf,  2)),
  Test_R2   = c(0.1682,  0.1672,  0.6528,  0.7321,
                round(test_r2_rf,   4))
)
print(full_comparison)

# ── 10. Actual vs Predicted plot ──────────────────────────────
plot_df_rf <- data.frame(
  Actual    = Test_scaled$ProdPerBusiness,
  Predicted = test_pred_rf
)

ggplot(plot_df_rf, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Random Forest — Actual vs Predicted (Test Set)",
       x     = "Actual ProdPerBusiness",
       y     = "Predicted ProdPerBusiness") +
  theme_minimal()

cat("\n✓ Step 7 complete — Random Forest done\n")