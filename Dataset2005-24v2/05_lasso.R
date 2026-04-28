# ============================================================
# 05_LASSO.R
# Step 1: Baseline model (all features)
# Step 2: Feature importance
# Step 3: Model with top 15 features
# ============================================================

library(glmnet)
library(Metrics)
library(dplyr)

# ============================================================
# STEP 1 - BASELINE (all features)
# ============================================================

X_train_mat <- as.matrix(X_train)
X_test_mat  <- as.matrix(X_test)

set.seed(123)
cv_lasso    <- cv.glmnet(X_train_mat, y_train, alpha = 1, nfolds = 5)
best_lambda <- cv_lasso$lambda.min
cat("Best lambda:", round(best_lambda, 4), "\n")

lasso_model <- glmnet(X_train_mat, y_train, alpha = 1, lambda = best_lambda)

pred_train_lasso <- as.vector(predict(lasso_model, X_train_mat))
pred_test_lasso  <- as.vector(predict(lasso_model, X_test_mat))

rss_l    <- sum((y_train - pred_train_lasso)^2)
n_l      <- length(y_train)
lasso_aic <- n_l * log(rss_l/n_l) + 2 * lasso_model$df

cat("\n==============================\n")
cat("   LASSO - BASELINE\n")
cat("   (All features)\n")
cat("==============================\n")
cat("AIC:       ", round(lasso_aic, 2), "\n")
cat("Features kept:", lasso_model$df, "out of", length(features), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_lasso), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_lasso)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_lasso), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_lasso), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_lasso)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_lasso), 2), "\n")

# ============================================================
# STEP 2 - FEATURE IMPORTANCE
# ============================================================

lasso_coef    <- coef(lasso_model)
lasso_coef_df <- data.frame(
  Feature     = rownames(lasso_coef),
  Coefficient = as.vector(lasso_coef)
)
lasso_coef_df <- lasso_coef_df[lasso_coef_df$Feature != "(Intercept)" &
                                lasso_coef_df$Coefficient != 0, ]
lasso_coef_df <- lasso_coef_df[order(abs(lasso_coef_df$Coefficient),
                                     decreasing = TRUE), ]

cat("\n=== FEATURES KEPT BY LASSO ===\n")
cat("Total kept:", nrow(lasso_coef_df), "| Dropped:",
    length(features) - nrow(lasso_coef_df), "\n\n")
print(lasso_coef_df, row.names = FALSE)

cat("\n=== FEATURES DROPPED BY LASSO ===\n")
dropped <- setdiff(features, lasso_coef_df$Feature)
print(dropped)

# ============================================================
# STEP 3 - TOP 15 FEATURES
# ============================================================

top15_lasso <- head(lasso_coef_df$Feature, 15)
cat("\nTop 15 features:\n")
print(top15_lasso)

X_train_top15 <- as.matrix(X_train[, top15_lasso])
X_test_top15  <- as.matrix(X_test[,  top15_lasso])

set.seed(123)
cv_lasso_top  <- cv.glmnet(X_train_top15, y_train, alpha = 1, nfolds = 5)
lasso_top     <- glmnet(X_train_top15, y_train, alpha = 1,
                        lambda = cv_lasso_top$lambda.min)

pred_train_top <- as.vector(predict(lasso_top, X_train_top15))
pred_test_top  <- as.vector(predict(lasso_top, X_test_top15))

rss_top   <- sum((y_train - pred_train_top)^2)
aic_top   <- n_l * log(rss_top/n_l) + 2 * lasso_top$df

cat("\n==============================\n")
cat("   LASSO - TOP 15 FEATURES\n")
cat("==============================\n")
cat("AIC:       ", round(aic_top, 2), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_top), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_top)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_top), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_top), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_top)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_top), 2), "\n")

# ── Plot lambda path ──────────────────────────────────────────
plot(cv_lasso, main = "LASSO - Cross Validation Lambda")

# ── Update model comparison ───────────────────────────────────
model_comparison <- rbind(model_comparison,
  data.frame(Model     = "LASSO Baseline",
             AIC       = round(lasso_aic, 2),
             Train_R2  = round(cor(y_train, pred_train_lasso)^2, 4),
             Test_RMSE = round(rmse(y_test, pred_test_lasso), 2),
             Test_R2   = round(cor(y_test, pred_test_lasso)^2, 4)),
  data.frame(Model     = "LASSO Top 15",
             AIC       = round(aic_top, 2),
             Train_R2  = round(cor(y_train, pred_train_top)^2, 4),
             Test_RMSE = round(rmse(y_test, pred_test_top), 2),
             Test_R2   = round(cor(y_test, pred_test_top)^2, 4))
)

cat("\n=== MODEL COMPARISON (so far) ===\n")
print(model_comparison, row.names = FALSE)
