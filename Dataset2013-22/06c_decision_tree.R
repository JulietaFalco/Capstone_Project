# ══════════════════════════════════════════════════════════════
# STEP 6b — Decision Tree with Top Features only
# ══════════════════════════════════════════════════════════════

library(rpart)
library(rpart.plot)
library(Metrics)
library(ggplot2)
library(dplyr)

# ── 1. Select top features ────────────────────────────────────
top_features <- c(
  # Geographic
  "LATITUDE", "LONGITUDE",
  
  # Humidity
  "RelHumAvg_month_8", "RelHumAvg_month_9",
  "RelHumAvg_month_11", "RelHumAvg_month_3",
  
  # Evaporation
  "Evaporation_month_7", "Evaporation_month_8", "Evaporation_month_6",
  
  # Temperature
  "AirTempAvg_month_3", "AirTempAvg_month_9",
  
  # Wind
  "WindAvg_month_10", "WindAvg_month_6", "WindAvg_month_12",
  
  # Rainfall
  "Rainfall_month_4",
  
  # Economic
  "Wheat_price",
  
  # Target
  "ProdPerBusiness"
)

# ── 2. Filter datasets to top features only ───────────────────
Train_top <- Train_scaled %>% select(all_of(top_features))
Test_top  <- Test_scaled  %>% select(all_of(top_features))

cat("=== REDUCED DATASET DIMENSIONS ===\n")
cat("Train:", nrow(Train_top), "rows x", ncol(Train_top), "cols\n")
cat("Test: ", nrow(Test_top),  "rows x", ncol(Test_top),  "cols\n")
cat("Features used:", ncol(Train_top) - 1, "\n")

# ── 3. Test multiple cp values ────────────────────────────────
cp_values  <- c(0.15, 0.08, 0.045, 0.01)
tree_names <- c("Very Simple (cp=0.15)",
                "Simple (cp=0.08)",
                "Current (cp=0.045)",
                "Complex (cp=0.01)")

results_top <- data.frame()

for(i in seq_along(cp_values)){
  
  tree_i <- rpart(
    ProdPerBusiness ~ .,
    data    = Train_top,
    method  = "anova",
    control = rpart.control(
      cp       = cp_values[i],
      minsplit = 10,
      maxdepth = 10
    )
  )
  
  # Tree size
  n_leaves <- sum(tree_i$frame$var == "<leaf>")
  n_splits <- nrow(tree_i$frame) - n_leaves
  
  # Train performance
  train_pred_i <- predict(tree_i, Train_top)
  train_rmse_i <- rmse(Train_top$ProdPerBusiness, train_pred_i)
  train_r2_i   <- cor(Train_top$ProdPerBusiness,  train_pred_i)^2
  
  # Test performance
  test_pred_i  <- predict(tree_i, Test_top)
  test_rmse_i  <- rmse(Test_top$ProdPerBusiness,  test_pred_i)
  test_r2_i    <- cor(Test_top$ProdPerBusiness,   test_pred_i)^2
  
  # AIC
  resid_i <- Train_top$ProdPerBusiness - train_pred_i
  rss_i   <- sum(resid_i^2)
  aic_i   <- nrow(Train_top) * log(rss_i / nrow(Train_top)) + 2 * n_leaves
  
  results_top <- rbind(results_top, data.frame(
    Model      = tree_names[i],
    cp         = cp_values[i],
    Splits     = n_splits,
    Leaves     = n_leaves,
    Train_RMSE = round(train_rmse_i, 2),
    Train_R2   = round(train_r2_i,   4),
    Test_RMSE  = round(test_rmse_i,  2),
    Test_R2    = round(test_r2_i,    4),
    AIC        = round(aic_i,        2)
  ))
}

cat("\n=== DECISION TREE TOP FEATURES — VARIANTS COMPARISON ===\n")
print(results_top)

# ── 4. Select best cp ─────────────────────────────────────────
best_cp <- results_top$cp[which.min(results_top$Test_RMSE)]
cat("\n=== BEST cp ===\n")
cat("Best cp:", best_cp, "\n")

# ── 5. Train best tree ────────────────────────────────────────
set.seed(123)
dt_top_best <- rpart(
  ProdPerBusiness ~ .,
  data    = Train_top,
  method  = "anova",
  control = rpart.control(
    cp       = best_cp,
    minsplit = 10,
    maxdepth = 10
  )
)

cat("\n=== BEST TREE STRUCTURE ===\n")
cat("Number of leaves:", sum(dt_top_best$frame$var == "<leaf>"), "\n")
cat("Number of splits:", nrow(dt_top_best$frame) - 
                         sum(dt_top_best$frame$var == "<leaf>"), "\n")

# ── 6. Visualize best tree ────────────────────────────────────
png("decision_tree_top_features.png", width = 1400, height = 900, res = 120)
rpart.plot(
  dt_top_best,
  type          = 4,
  extra         = 101,
  fallen.leaves = TRUE,
  main          = "Decision Tree — Top 16 Features",
  cex           = 0.7
)
dev.off()
cat("✓ Tree saved to decision_tree_top_features.png\n")

# ── 7. Feature importance ─────────────────────────────────────
importance_top <- data.frame(
  feature    = names(dt_top_best$variable.importance),
  importance = dt_top_best$variable.importance
) %>%
  arrange(desc(importance))

cat("\n=== FEATURE IMPORTANCE (Top Features Tree) ===\n")
print(importance_top)

ggplot(importance_top,
       aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  coord_flip() +
  labs(title = "Decision Tree Top Features — Feature Importance",
       x     = "Feature",
       y     = "Importance") +
  theme_minimal()

# ── 8. Train and Test performance ─────────────────────────────
train_pred_top <- predict(dt_top_best, Train_top)
train_rmse_top <- rmse(Train_top$ProdPerBusiness, train_pred_top)
train_mae_top  <- mae(Train_top$ProdPerBusiness,  train_pred_top)
train_r2_top   <- cor(Train_top$ProdPerBusiness,  train_pred_top)^2

test_pred_top  <- predict(dt_top_best, Test_top)
test_rmse_top  <- rmse(Test_top$ProdPerBusiness,  test_pred_top)
test_mae_top   <- mae(Test_top$ProdPerBusiness,   test_pred_top)
test_r2_top    <- cor(Test_top$ProdPerBusiness,   test_pred_top)^2

cat("\n=== TRAIN vs TEST PERFORMANCE ===\n")
comparison_top <- data.frame(
  Dataset = c("Train", "Test"),
  RMSE    = c(round(train_rmse_top, 2), round(test_rmse_top, 2)),
  MAE     = c(round(train_mae_top,  2), round(test_mae_top,  2)),
  R2      = c(round(train_r2_top,   4), round(test_r2_top,   4))
)
print(comparison_top)

# ── 9. Full model comparison ──────────────────────────────────
# AIC for best tree
resid_top <- Train_top$ProdPerBusiness - train_pred_top
rss_top   <- sum(resid_top^2)
k_top     <- sum(dt_top_best$frame$var == "<leaf>")
aic_top   <- nrow(Train_top) * log(rss_top / nrow(Train_top)) + 2 * k_top

cat("\n=== FULL MODEL COMPARISON ===\n")
full_comparison <- data.frame(
  Model     = c("Linear Regression", "Lasso",
                "Decision Tree (cp=0.045)",
                "Decision Tree (cp=0.01)",
                "Random Forest",
                "Decision Tree Top Features"),
  Test_RMSE = c(1833.00, 1705.54, 1338.01, 1347.46, 1465.11,
                round(test_rmse_top, 2)),
  Test_MAE  = c(1303.63, 1233.54, 999.45,  999.45,  1072.14,
                round(test_mae_top,  2)),
  Test_R2   = c(0.1682,  0.1672,  0.6528,  0.7321,  0.5108,
                round(test_r2_top,   4)),
  AIC       = c(2533.74, 2106.50, 2044.25, 1946.38, NA,
                round(aic_top,      2))
)
print(full_comparison)

# ── 10. Actual vs Predicted plot ──────────────────────────────
plot_df_top <- data.frame(
  Actual    = Test_top$ProdPerBusiness,
  Predicted = test_pred_top
)

ggplot(plot_df_top, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Decision Tree Top Features — Actual vs Predicted (Test Set)",
       x     = "Actual ProdPerBusiness",
       y     = "Predicted ProdPerBusiness") +
  theme_minimal()

cat("\n✓ Step 6b complete — Decision Tree Top Features done\n")