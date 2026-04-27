# ══════════════════════════════════════════════════════════════
# STEP 5 — Lasso Regression
# ══════════════════════════════════════════════════════════════

if (!requireNamespace("glmnet",  quietly = TRUE)) install.packages("glmnet")
if (!requireNamespace("Metrics", quietly = TRUE)) install.packages("Metrics")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(glmnet)
library(Metrics)
library(ggplot2)

# ── 1. Prepare matrices (glmnet requires matrix input) ────────
X_train <- as.matrix(Train_scaled %>% select(-ProdPerBusiness))
y_train <- Train_scaled$ProdPerBusiness

X_test  <- as.matrix(Test_scaled %>% select(-ProdPerBusiness))
y_test  <- Test_scaled$ProdPerBusiness

# ── 2. Find optimal lambda via cross-validation ───────────────
set.seed(123)
cv_lasso <- cv.glmnet(
  x          = X_train,
  y          = y_train,
  alpha      = 1,          # alpha = 1 means Lasso
  nfolds     = 10,         # 10-fold cross-validation
  standardize = FALSE      # already standardized
)

# Plot cross-validation results
plot(cv_lasso)
title("Lasso — Cross Validation (Lambda Selection)", line = 3)

cat("=== OPTIMAL LAMBDA ===\n")
cat("Lambda min  (lowest CV error):", round(cv_lasso$lambda.min, 4), "\n")
cat("Lambda 1se  (simplest model within 1 SE):", round(cv_lasso$lambda.1se, 4), "\n")

# ── 3. Fit Lasso with optimal lambda ──────────────────────────
lasso_model <- glmnet(
  x      = X_train,
  y      = y_train,
  alpha  = 1,
  lambda = cv_lasso$lambda.min
)

# ── 4. Non-zero coefficients (selected features) ──────────────
lasso_coef <- coef(lasso_model)
lasso_coef_df <- data.frame(
  feature     = rownames(lasso_coef),
  coefficient = as.vector(lasso_coef)
) %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient)))

cat("\n=== SELECTED FEATURES (non-zero coefficients) ===\n")
cat("Number of features selected:", nrow(lasso_coef_df) - 1, "\n")
print(lasso_coef_df)

# ── 5. Train performance ──────────────────────────────────────
train_pred_lasso <- predict(lasso_model, X_train)
train_rmse_lasso <- rmse(y_train, train_pred_lasso)
train_mae_lasso  <- mae(y_train,  train_pred_lasso)
train_r2_lasso   <- cor(y_train,  train_pred_lasso)^2

# ── 6. Test performance ───────────────────────────────────────
test_pred_lasso <- predict(lasso_model, X_test)
test_rmse_lasso <- rmse(y_test, test_pred_lasso)
test_mae_lasso  <- mae(y_test,  test_pred_lasso)
test_r2_lasso   <- cor(y_test,  test_pred_lasso)^2

# ── 7. Train vs Test comparison ───────────────────────────────
cat("\n=== TRAIN vs TEST PERFORMANCE ===\n")
comparison_lasso <- data.frame(
  Dataset = c("Train", "Test"),
  RMSE    = c(round(train_rmse_lasso, 2), round(test_rmse_lasso, 2)),
  MAE     = c(round(train_mae_lasso,  2), round(test_mae_lasso,  2)),
  R2      = c(round(train_r2_lasso,   4), round(test_r2_lasso,   4))
)
print(comparison_lasso)

# ── 8. AIC for Lasso ──────────────────────────────────────────
# Calculate AIC manually for Lasso
n         <- length(y_train)
k         <- sum(lasso_coef_df$coefficient != 0)
residuals <- y_train - train_pred_lasso
rss       <- sum(residuals^2)
aic_lasso <- n * log(rss / n) + 2 * k

cat("\n=== AIC COMPARISON ===\n")
cat("Linear Regression AIC:", round(2533.74,   2), "\n")
cat("Lasso AIC:            ", round(aic_lasso, 2), "\n")
cat("Improvement:          ", round(2533.74 - aic_lasso, 2), "\n")

# ── 9. Compare Linear Regression vs Lasso ─────────────────────
cat("\n=== LINEAR REGRESSION vs LASSO (TEST SET) ===\n")
model_comparison <- data.frame(
  Model = c("Linear Regression", "Lasso"),
  RMSE  = c(1833.00, round(test_rmse_lasso, 2)),
  MAE   = c(1303.63, round(test_mae_lasso,  2)),
  R2    = c(0.1682,  round(test_r2_lasso,   4)),
  AIC   = c(2533.74, round(aic_lasso,       2))
)
print(model_comparison)

# ── 10. Actual vs Predicted plot ──────────────────────────────
plot_df_lasso <- data.frame(
  Actual    = y_test,
  Predicted = as.vector(test_pred_lasso)
)

ggplot(plot_df_lasso, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Lasso — Actual vs Predicted (Test Set)",
       x     = "Actual ProdPerBusiness",
       y     = "Predicted ProdPerBusiness") +
  theme_minimal()

# ── 11. Feature importance plot ───────────────────────────────
lasso_coef_df %>%
  filter(feature != "(Intercept)") %>%
  ggplot(aes(x = reorder(feature, abs(coefficient)),
             y = coefficient,
             fill = coefficient > 0)) +
  geom_bar(stat = "identity", color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    labels = c("Negative", "Positive"),
                    name   = "Direction") +
  labs(title = "Lasso — Selected Features and Coefficients",
       x     = "Feature",
       y     = "Coefficient") +
  theme_minimal()

cat("\n✓ Step 5 complete — Lasso Regression done\n")