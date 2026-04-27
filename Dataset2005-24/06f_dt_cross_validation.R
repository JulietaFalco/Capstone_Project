# ============================================================
# 06f_DT_CROSS_VALIDATION.R
# K-fold cross validation for Decision Tree
# Features: Geo + AirTemp + Wind (best combination)
# ============================================================

library(rpart)
library(Metrics)
library(caret)

# ── Feature set ───────────────────────────────────────────────
geo     <- c("LATITUDE", "LONGITUDE")
airtemp <- paste0("AirTempAvg_month_", 1:12)
windavg <- paste0("WindAvg_month_",    1:12)

best_feats <- c(geo, airtemp, windavg)

# Use full dataset for cross-validation (not just train)
df_cv <- df_model[, c(best_feats, "ProdPerBusiness")]

cat("Total rows for CV:", nrow(df_cv), "\n")
cat("Features used:", length(best_feats), "\n\n")

# ── K-fold CV grid ────────────────────────────────────────────
cp_values       <- c(0.0001, 0.001, 0.005, 0.01)
maxdepth_values <- c(4, 5, 6, 7)
k               <- 5

set.seed(123)
folds <- createFolds(df_cv$ProdPerBusiness, k = k, list = TRUE)

results <- data.frame()

for (cp in cp_values) {
  for (md in maxdepth_values) {
    
    fold_rmse <- c()
    fold_r2   <- c()
    fold_mae  <- c()
    
    for (fold in folds) {
      val_data   <- df_cv[fold, ]
      train_data <- df_cv[-fold, ]
      
      model <- rpart(ProdPerBusiness ~ .,
                     data    = train_data,
                     method  = "anova",
                     control = rpart.control(cp = cp, maxdepth = md))
      
      pred <- predict(model, newdata = val_data)
      
      fold_rmse <- c(fold_rmse, rmse(val_data$ProdPerBusiness, pred))
      fold_r2   <- c(fold_r2,   cor(val_data$ProdPerBusiness, pred)^2)
      fold_mae  <- c(fold_mae,  mae(val_data$ProdPerBusiness, pred))
    }
    
    results <- rbind(results, data.frame(
      cp          = cp,
      maxdepth    = md,
      CV_RMSE     = round(mean(fold_rmse), 2),
      CV_R2       = round(mean(fold_r2), 4),
      CV_MAE      = round(mean(fold_mae), 2),
      SD_RMSE     = round(sd(fold_rmse), 2)
    ))
  }
}

results <- results[order(results$CV_R2, decreasing = TRUE), ]

cat("=== 5-FOLD CV RESULTS - Sorted by CV R2 ===\n\n")
print(results, row.names = FALSE)

cat("\n=== BEST by CV R2 ===\n")
print(results[1, ], row.names = FALSE)

cat("\n=== BEST by CV RMSE ===\n")
print(results[order(results$CV_RMSE), ][1, ], row.names = FALSE)

# ── Train final model with best params on full train set ──────
best_cp <- results$cp[1]
best_md <- results$maxdepth[1]

cat("\n=== FINAL MODEL with best CV params ===\n")
cat("cp:", best_cp, "| maxdepth:", best_md, "\n\n")

train_best  <- train[, c(best_feats, "ProdPerBusiness")]
X_test_best <- X_test[, best_feats]

final_model <- rpart(ProdPerBusiness ~ .,
                     data    = train_best,
                     method  = "anova",
                     control = rpart.control(cp = best_cp, maxdepth = best_md))

pred_train_final <- predict(final_model, newdata = train_best)
pred_test_final  <- predict(final_model, newdata = X_test_best)

rss_f <- sum((y_train - pred_train_final)^2)
n_f   <- length(y_train)
df_f  <- length(unique(final_model$where))
aic_f <- n_f * log(rss_f/n_f) + 2 * df_f

cat("AIC:      ", round(aic_f, 2), "\n")
cat("TRAIN R2: ", round(cor(y_train, pred_train_final)^2, 4), "\n")
cat("TRAIN RMSE:", round(rmse(y_train, pred_train_final), 2), "\n")
cat("TEST  R2: ", round(cor(y_test, pred_test_final)^2, 4), "\n")
cat("TEST  RMSE:", round(rmse(y_test, pred_test_final), 2), "\n")
cat("TEST  MAE: ", round(mae(y_test, pred_test_final), 2), "\n")

rpart.plot(final_model,
           main = paste0("DT CV Best: cp=", best_cp, " maxdepth=", best_md),
           type = 4, extra = 101)
