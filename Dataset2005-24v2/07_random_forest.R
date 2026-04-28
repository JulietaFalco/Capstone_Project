# ============================================================
# 07_RANDOM_FOREST.R
# Step 1: Run RF with ALL features to get feature importance
# Step 2: Run RF with top features selected by RF itself
# ============================================================

library(randomForest)
library(Metrics)
library(ggplot2)
library(dplyr)

# ============================================================
# STEP 1 - RF WITH ALL FEATURES (to get importance)
# ============================================================

set.seed(123)
rf_full <- randomForest(ProdPerBusiness ~ .,
                        data       = train,
                        ntree      = 500,
                        mtry       = floor(sqrt(ncol(X_train))),
                        importance = TRUE)

pred_train_full <- predict(rf_full, newdata = X_train)
pred_test_full  <- predict(rf_full, newdata = X_test)

cat("==============================\n")
cat("   RF - ALL FEATURES\n")
cat("   (to determine importance)\n")
cat("==============================\n")
cat("TRAIN R2: ", round(cor(y_train, pred_train_full)^2, 4), "\n")
cat("TEST  R2: ", round(cor(y_test,  pred_test_full)^2, 4), "\n")
cat("TEST RMSE:", round(rmse(y_test, pred_test_full), 2), "\n")
cat("Overfit:  ", round(cor(y_train, pred_train_full)^2 -
                        cor(y_test,  pred_test_full)^2, 4), "\n")

# ── Feature importance ────────────────────────────────────────
rf_imp <- data.frame(
  Feature = rownames(importance(rf_full)),
  IncMSE  = importance(rf_full)[, "%IncMSE"]
)
rf_imp <- rf_imp[order(rf_imp$IncMSE, decreasing = TRUE), ]

cat("\nTop 20 features by %IncMSE:\n")
print(head(rf_imp, 20), row.names = FALSE)

# ── Plot importance ───────────────────────────────────────────
ggplot(head(rf_imp, 20), aes(x = reorder(Feature, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "RF All Features - Top 20 Feature Importance (%IncMSE)",
       x = "Feature", y = "%IncMSE") +
  theme_minimal()

# ============================================================
# STEP 2 - RF WITH TOP FEATURES (RF selects itself)
# ============================================================

# Test different cutoffs: top 5, 10, 15, 20
cutoffs <- c(5, 10, 15, 20)
results <- data.frame()

for (n in cutoffs) {
  top_feats  <- head(rf_imp$Feature, n)
  train_top  <- train[, c(top_feats, "ProdPerBusiness")]
  X_test_top <- X_test[, top_feats]

  set.seed(123)
  rf_top <- randomForest(ProdPerBusiness ~ .,
                         data      = train_top,
                         ntree     = 500,
                         mtry      = floor(sqrt(n)),
                         importance = FALSE)

  pred_tr <- predict(rf_top, newdata = train_top)
  pred_te <- predict(rf_top, newdata = X_test_top)
  rss     <- sum((y_train - pred_tr)^2)
  aic     <- length(y_train) * log(rss/length(y_train)) + 2 * rf_top$mtry
  gap     <- round(cor(y_train, pred_tr)^2 - cor(y_test, pred_te)^2, 4)

  results <- rbind(results, data.frame(
    Top_N       = n,
    AIC         = round(aic, 2),
    Train_R2    = round(cor(y_train, pred_tr)^2, 4),
    Test_RMSE   = round(rmse(y_test, pred_te), 2),
    Test_R2     = round(cor(y_test, pred_te)^2, 4),
    Test_MAE    = round(mae(y_test, pred_te), 2),
    Overfit_gap = gap
  ))
}

results <- results[order(results$Test_R2, decreasing = TRUE), ]

cat("\n=== RF TOP FEATURES RESULTS ===\n")
print(results, row.names = FALSE)

cat("\n=== BEST by Test R2 ===\n")
best_n <- results$Top_N[1]
print(results[1, ], row.names = FALSE)

cat("\nTop", best_n, "features selected by RF:\n")
print(head(rf_imp$Feature, best_n))

# ── Final plot - actual vs predicted ─────────────────────────
top_feats  <- head(rf_imp$Feature, best_n)
train_best <- train[, c(top_feats, "ProdPerBusiness")]
X_test_best <- X_test[, top_feats]

set.seed(123)
rf_best <- randomForest(ProdPerBusiness ~ .,
                        data      = train_best,
                        ntree     = 500,
                        mtry      = floor(sqrt(best_n)),
                        importance = FALSE)

pred_test_best <- predict(rf_best, newdata = X_test_best)

plot(y_test, pred_test_best,
     main = paste0("RF Top ", best_n, " Features - Actual vs Predicted"),
     xlab = "Actual ProdPerBusiness",
     ylab = "Predicted ProdPerBusiness",
     pch  = 16, col = "steelblue")
abline(0, 1, col = "red", lwd = 2)

# ── Update model comparison ───────────────────────────────────
best_row <- results[1, ]
model_comparison <- rbind(model_comparison,
  data.frame(Model     = paste0("RF Top ", best_n, " (RF-selected)"),
             AIC       = best_row$AIC,
             Train_R2  = best_row$Train_R2,
             Test_RMSE = best_row$Test_RMSE,
             Test_R2   = best_row$Test_R2))

cat("\n=== MODEL COMPARISON (so far) ===\n")
print(model_comparison, row.names = FALSE)
