# ============================================================
# 07d_RANDOM_FOREST_TUNING.R
# Grid search: mtry x nodesize
# ============================================================

library(randomForest)
library(Metrics)

# ── Grid of parameters ────────────────────────────────────────
mtry_values    <- c(3, 5, 9, 15, 20)
nodesize_values <- c(5, 10, 15, 20, 30)

results <- data.frame()

cat("Running grid search...\n")

for (mt in mtry_values) {
  for (ns in nodesize_values) {
    
    set.seed(123)
    model <- randomForest(ProdPerBusiness ~ .,
                          data      = train,
                          ntree     = 500,
                          mtry      = mt,
                          nodesize  = ns,
                          importance = FALSE)
    
    pred_train <- predict(model, newdata = X_train)
    pred_test  <- predict(model, newdata = X_test)
    
    rss  <- sum((y_train - pred_train)^2)
    n    <- length(y_train)
    aic  <- n * log(rss/n) + 2 * mt
    gap  <- round(cor(y_train, pred_train)^2 - cor(y_test, pred_test)^2, 4)
    
    results <- rbind(results, data.frame(
      mtry       = mt,
      nodesize   = ns,
      AIC        = round(aic, 2),
      Train_R2   = round(cor(y_train, pred_train)^2, 4),
      Test_RMSE  = round(rmse(y_test, pred_test), 2),
      Test_R2    = round(cor(y_test, pred_test)^2, 4),
      Test_MAE   = round(mae(y_test, pred_test), 2),
      Overfit_gap = gap
    ))
  }
}

# ── Sort by Test R2 ───────────────────────────────────────────
results_r2   <- results[order(results$Test_R2, decreasing = TRUE), ]
results_rmse <- results[order(results$Test_RMSE), ]
results_gap  <- results[order(results$Overfit_gap), ]

cat("\n=== TUNING RESULTS - Sorted by Test R2 ===\n")
print(results_r2)

cat("\n=== BEST by Test R2 ===\n")
print(results_r2[1, ])

cat("\n=== BEST by Test RMSE ===\n")
print(results_rmse[1, ])

cat("\n=== LEAST OVERFITTING (smallest Train-Test gap) ===\n")
print(results_gap[1, ])
