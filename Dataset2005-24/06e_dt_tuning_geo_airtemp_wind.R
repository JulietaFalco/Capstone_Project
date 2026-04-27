# ============================================================
# 06e_DT_TUNING_GEO_AIRTEMP_WIND.R
# Tuning cp x maxdepth for Geo+AirTemp+Wind combination
# ============================================================

library(rpart)
library(rpart.plot)
library(Metrics)

# ── Feature set ───────────────────────────────────────────────
geo     <- c("LATITUDE", "LONGITUDE")
airtemp <- paste0("AirTempAvg_month_", 1:12)
windavg <- paste0("WindAvg_month_",    1:12)

best_feats <- c(geo, airtemp, windavg)
cat("Features used:", length(best_feats), "\n")

train_best  <- train[, c(best_feats, "ProdPerBusiness")]
X_test_best <- X_test[, best_feats]

# ── Grid ──────────────────────────────────────────────────────
cp_values       <- c(0.0001, 0.0005, 0.001, 0.005, 0.01)
maxdepth_values <- c(4, 5, 6, 7, 8, 10)

results <- data.frame()

for (cp in cp_values) {
  for (md in maxdepth_values) {
    model      <- rpart(ProdPerBusiness ~ .,
                        data    = train_best,
                        method  = "anova",
                        control = rpart.control(cp = cp, maxdepth = md))
    pred_train <- predict(model, newdata = train_best)
    pred_test  <- predict(model, newdata = X_test_best)
    rss        <- sum((y_train - pred_train)^2)
    n          <- length(y_train)
    df         <- length(unique(model$where))
    aic        <- n * log(rss/n) + 2 * df
    gap        <- round(cor(y_train, pred_train)^2 - cor(y_test, pred_test)^2, 4)

    results <- rbind(results, data.frame(
      cp         = cp,
      maxdepth   = md,
      AIC        = round(aic, 2),
      Train_R2   = round(cor(y_train, pred_train)^2, 4),
      Test_RMSE  = round(rmse(y_test, pred_test), 2),
      Test_R2    = round(cor(y_test, pred_test)^2, 4),
      Test_MAE   = round(mae(y_test, pred_test), 2),
      Overfit_gap = gap,
      N_leaves   = df
    ))
  }
}

results <- results[order(results$Test_R2, decreasing = TRUE), ]

cat("\n=== TUNING RESULTS - Sorted by Test R2 ===\n")
print(results, row.names = FALSE)

cat("\n=== BEST by Test R2 ===\n")
print(results[1, ], row.names = FALSE)

cat("\n=== BEST by AIC ===\n")
print(results[order(results$AIC), ][1, ], row.names = FALSE)

cat("\n=== BEST by Test RMSE ===\n")
print(results[order(results$Test_RMSE), ][1, ], row.names = FALSE)

cat("\n=== BEST Train R2 with Test R2 > 0.70 ===\n")
filtered <- results[results$Test_R2 > 0.70, ]
print(filtered[order(filtered$Train_R2, decreasing = TRUE), ][1, ], row.names = FALSE)

# ── Plot best model ───────────────────────────────────────────
best <- results[1, ]
best_model <- rpart(ProdPerBusiness ~ .,
                    data    = train_best,
                    method  = "anova",
                    control = rpart.control(cp = best$cp, maxdepth = best$maxdepth))

rpart.plot(best_model,
           main = paste0("Best DT: cp=", best$cp, " maxdepth=", best$maxdepth),
           type = 4, extra = 101)
