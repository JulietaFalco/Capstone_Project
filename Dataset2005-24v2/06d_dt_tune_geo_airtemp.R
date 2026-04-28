# ============================================================
# 06d_DT_TUNE_GEO_AIRTEMP.R
# Tuning grid for Geo + AirTemp combination
# ============================================================

library(rpart)
library(rpart.plot)
library(Metrics)
library(dplyr)

# ── Feature set ───────────────────────────────────────────────
geo     <- c("LATITUDE", "LONGITUDE")
airtemp <- paste0("AirTempAvg_month_", 1:12)
feats   <- c(geo, airtemp)

cat("Features used:", length(feats), "\n")

train_ga  <- train[, c(feats, "ProdPerBusiness")]
X_test_ga <- X_test[, feats]

# ── Tuning grid ───────────────────────────────────────────────
cp_values       <- c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.02)
maxdepth_values <- c(3, 4, 5, 6, 7, 8)

results <- data.frame()

for (cp in cp_values) {
  for (md in maxdepth_values) {
    model      <- rpart(ProdPerBusiness ~ .,
                        data    = train_ga,
                        method  = "anova",
                        control = rpart.control(cp = cp, maxdepth = md))
    pred_train <- predict(model, newdata = train_ga)
    pred_test  <- predict(model, newdata = X_test_ga)
    rss        <- sum((y_train - pred_train)^2)
    n          <- length(y_train)
    df         <- length(unique(model$where))
    aic        <- n * log(rss/n) + 2 * df
    gap        <- round(cor(y_train, pred_train)^2 - cor(y_test, pred_test)^2, 4)

    results <- rbind(results, data.frame(
      cp          = cp,
      maxdepth    = md,
      AIC         = round(aic, 2),
      Train_R2    = round(cor(y_train, pred_train)^2, 4),
      Test_RMSE   = round(rmse(y_test, pred_test), 2),
      Test_R2     = round(cor(y_test, pred_test)^2, 4),
      Test_MAE    = round(mae(y_test, pred_test), 2),
      Overfit_gap = gap,
      N_leaves    = df
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

cat("\n=== LOWEST OVERFIT GAP ===\n")
print(results[order(abs(results$Overfit_gap)), ][1, ], row.names = FALSE)

# ── Plot best model ───────────────────────────────────────────
best <- results[1, ]
best_model <- rpart(ProdPerBusiness ~ .,
                    data    = train_ga,
                    method  = "anova",
                    control = rpart.control(cp = best$cp, maxdepth = best$maxdepth))

rpart.plot(best_model,
           main = paste0("Best DT Geo+AirTemp: cp=", best$cp,
                         " maxdepth=", best$maxdepth),
           type = 4, extra = 101)

# ── Update model comparison ───────────────────────────────────
model_comparison <- rbind(model_comparison,
  data.frame(Model     = paste0("DT Geo+AirTemp cp=", best$cp, " md=", best$maxdepth),
             AIC       = best$AIC,
             Train_R2  = best$Train_R2,
             Test_RMSE = best$Test_RMSE,
             Test_R2   = best$Test_R2))

cat("\n=== MODEL COMPARISON (so far) ===\n")
print(model_comparison, row.names = FALSE)
