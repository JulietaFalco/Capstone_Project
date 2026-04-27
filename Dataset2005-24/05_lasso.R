# ============================================================
# 05_LASSO.R
# ============================================================

library(glmnet)
library(Metrics)

# ── Prepare matrices ─────────────────────────────────────────
X_train_mat <- as.matrix(X_train)
X_test_mat  <- as.matrix(X_test)

# ── Cross-validation to find best lambda ─────────────────────
set.seed(123)
cv_lasso    <- cv.glmnet(X_train_mat, y_train, alpha = 1, nfolds = 5)
best_lambda <- cv_lasso$lambda.min
cat("Best lambda:", round(best_lambda, 4), "\n")

# ── Train model ───────────────────────────────────────────────
lasso_model <- glmnet(X_train_mat, y_train, alpha = 1, lambda = best_lambda)

# ── AIC ───────────────────────────────────────────────────────
# AIC for LASSO: 2*df + n*log(RSS/n)
pred_train_lasso <- as.vector(predict(lasso_model, X_train_mat))
rss    <- sum((y_train - pred_train_lasso)^2)
df     <- lasso_model$df
n      <- length(y_train)
lasso_aic <- n * log(rss/n) + 2 * df
cat("AIC:", round(lasso_aic, 2), "\n")

# ── Predictions ───────────────────────────────────────────────
pred_test_lasso <- as.vector(predict(lasso_model, X_test_mat))

# ── Metrics ───────────────────────────────────────────────────
cat("==============================\n")
cat("       LASSO RESULTS\n")
cat("==============================\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_lasso), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_lasso)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_lasso), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_lasso), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_lasso)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_lasso), 2), "\n")

# ── Features kept by LASSO (non-zero coefficients) ───────────
lasso_coef    <- coef(lasso_model)
lasso_coef_df <- data.frame(
  Feature     = rownames(lasso_coef),
  Coefficient = as.vector(lasso_coef)
)
lasso_coef_df <- lasso_coef_df[lasso_coef_df$Feature != "(Intercept)" & 
                                lasso_coef_df$Coefficient != 0, ]
lasso_coef_df <- lasso_coef_df[order(abs(lasso_coef_df$Coefficient), decreasing = TRUE), ]

cat("\nFeatures kept by LASSO:", nrow(lasso_coef_df), "out of", length(features), "\n")
cat("\nAll features selected by LASSO (sorted by importance):\n")
print(lasso_coef_df)

# ── Features dropped by LASSO ─────────────────────────────────
dropped <- setdiff(features, lasso_coef_df$Feature)
cat("\nFeatures dropped by LASSO:", length(dropped), "\n")
print(dropped)

# ── Plot lambda path ──────────────────────────────────────────
plot(cv_lasso, main = "LASSO - Cross Validation Lambda")
