# ══════════════════════════════════════════════════════════════
# STEP 6 — Decision Tree
# ══════════════════════════════════════════════════════════════

if (!requireNamespace("rpart",       quietly = TRUE)) install.packages("rpart")
if (!requireNamespace("rpart.plot",  quietly = TRUE)) install.packages("rpart.plot")
if (!requireNamespace("Metrics",     quietly = TRUE)) install.packages("Metrics")
if (!requireNamespace("ggplot2",     quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("caret",       quietly = TRUE)) install.packages("caret")

library(rpart)
library(rpart.plot)
library(Metrics)
library(ggplot2)
library(caret)

# ── 1. Train Decision Tree with cross-validation ──────────────
set.seed(123)

# Use cross-validation to find optimal tree complexity (cp)
dt_model <- rpart(
  ProdPerBusiness ~ .,
  data    = Train_scaled,
  method  = "anova",        # regression tree
  control = rpart.control(
    cp       = 0.001,       # minimum complexity parameter
    minsplit = 10,          # minimum obs to attempt a split
    maxdepth = 10           # maximum tree depth
  )
)

# ── 2. Plot cross-validation error vs complexity ──────────────
plotcp(dt_model)
title("Decision Tree — Cross Validation Error vs Complexity", line = 3)

# Find optimal cp (lowest cross-validation error)
optimal_cp <- dt_model$cptable[
  which.min(dt_model$cptable[, "xerror"]), "CP"
]

cat("=== OPTIMAL COMPLEXITY PARAMETER ===\n")
cat("Optimal cp:", round(optimal_cp, 6), "\n")
cat("Tree size (splits):", 
    dt_model$cptable[which.min(dt_model$cptable[, "xerror"]), "nsplit"], "\n")

# ── 3. Prune tree to optimal cp ───────────────────────────────
dt_pruned <- prune(dt_model, cp = optimal_cp)

cat("\n=== PRUNED TREE STRUCTURE ===\n")
cat("Number of leaves:", sum(dt_pruned$frame$var == "<leaf>"), "\n")
cat("Number of splits:", nrow(dt_pruned$frame) - 
                         sum(dt_pruned$frame$var == "<leaf>"), "\n")

# ── 4. Visualize the tree ─────────────────────────────────────
rpart.plot(
  dt_pruned,
  type    = 4,
  extra   = 101,
  fallen.leaves = TRUE,
  main    = "Decision Tree — ProdPerBusiness",
  cex     = 0.7
)

# ── 5. Feature importance ─────────────────────────────────────
importance_dt <- data.frame(
  feature    = names(dt_pruned$variable.importance),
  importance = dt_pruned$variable.importance
) %>%
  arrange(desc(importance)) %>%
  head(20)

cat("\n=== TOP 20 FEATURE IMPORTANCE ===\n")
print(importance_dt)

# Plot feature importance
ggplot(importance_dt, 
       aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  coord_flip() +
  labs(title = "Decision Tree — Top 20 Feature Importance",
       x     = "Feature",
       y     = "Importance") +
  theme_minimal()

# ── 6. Train performance ──────────────────────────────────────
train_pred_dt <- predict(dt_pruned, Train_scaled)
train_rmse_dt <- rmse(Train_scaled$ProdPerBusiness, train_pred_dt)
train_mae_dt  <- mae(Train_scaled$ProdPerBusiness,  train_pred_dt)
train_r2_dt   <- cor(Train_scaled$ProdPerBusiness,  train_pred_dt)^2

# ── 7. Test performance ───────────────────────────────────────
test_pred_dt <- predict(dt_pruned, Test_scaled)
test_rmse_dt <- rmse(Test_scaled$ProdPerBusiness,  test_pred_dt)
test_mae_dt  <- mae(Test_scaled$ProdPerBusiness,   test_pred_dt)
test_r2_dt   <- cor(Test_scaled$ProdPerBusiness,   test_pred_dt)^2

# ── 8. Train vs Test comparison ───────────────────────────────
cat("\n=== TRAIN vs TEST PERFORMANCE ===\n")
comparison_dt <- data.frame(
  Dataset = c("Train", "Test"),
  RMSE    = c(round(train_rmse_dt, 2), round(test_rmse_dt, 2)),
  MAE     = c(round(train_mae_dt,  2), round(test_mae_dt,  2)),
  R2      = c(round(train_r2_dt,   4), round(test_r2_dt,   4))
)
print(comparison_dt)

# ── 9. AIC for Decision Tree ──────────────────────────────────
n_dt       <- nrow(Train_scaled)
k_dt       <- sum(dt_pruned$frame$var == "<leaf>")
resid_dt   <- Train_scaled$ProdPerBusiness - train_pred_dt
rss_dt     <- sum(resid_dt^2)
aic_dt     <- n_dt * log(rss_dt / n_dt) + 2 * k_dt

cat("\n=== AIC COMPARISON ===\n")
model_comparison <- data.frame(
  Model = c("Linear Regression", "Lasso", "Decision Tree"),
  RMSE  = c(1833.00, 1705.54, round(test_rmse_dt, 2)),
  MAE   = c(1303.63, 1233.54, round(test_mae_dt,  2)),
  R2    = c(0.1682,  0.1672,  round(test_r2_dt,   4)),
  AIC   = c(2533.74, 2106.50, round(aic_dt,       2))
)
print(model_comparison)

# ── 10. Actual vs Predicted plot ──────────────────────────────
plot_df_dt <- data.frame(
  Actual    = Test_scaled$ProdPerBusiness,
  Predicted = test_pred_dt
)

ggplot(plot_df_dt, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Decision Tree — Actual vs Predicted (Test Set)",
       x     = "Actual ProdPerBusiness",
       y     = "Predicted ProdPerBusiness") +
  theme_minimal()

cat("\n✓ Step 6 complete — Decision Tree done\n")


# Save tree plot as PNG
png("decision_tree_plot.png", width = 1200, height = 800, res = 120)
rpart.plot(
  dt_pruned,
  type          = 4,
  extra         = 101,
  fallen.leaves = TRUE,
  main          = "Decision Tree — ProdPerBusiness",
  cex           = 0.7
)
dev.off()

cat("✓ Tree saved to:", 
    file.path(getwd(), "decision_tree_plot.png"), "\n")

# Get original mean and sd of LATITUDE from scaler
lat_mean <- scaler$mean["LATITUDE"]
lat_sd   <- scaler$std["LATITUDE"]

# Convert standardized threshold back to real latitude
threshold_z   <- 0.0098
threshold_real <- (threshold_z * lat_sd) + lat_mean

cat("=== LATITUDE THRESHOLD ===\n")
cat("Standardized value:", threshold_z, "\n")
cat("Mean LATITUDE (train):", round(lat_mean, 4), "\n")
cat("SD LATITUDE (train):  ", round(lat_sd,   4), "\n")
cat("Real LATITUDE threshold:", round(threshold_real, 4), "\n")