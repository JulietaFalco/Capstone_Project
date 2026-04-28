# ============================================================
# 06e_DT_AIRTEMP_RAIN_RAD.R
# Decision Tree with AirTemp + Rainfall + Radiation
# No geographic variables
# cp=0.005, maxdepth=4 (best parameters)
# ============================================================

library(rpart)
library(rpart.plot)
library(Metrics)
library(dplyr)

# ── Feature set ───────────────────────────────────────────────
airtemp <- paste0("AirTempAvg_month_",  1:12)
rain    <- paste0("Rainfall_month_",    1:12)
rad     <- paste0("Radiation_month_",   1:12)

# ── Combinations to test ──────────────────────────────────────
combos <- list(
  "AirTemp only"          = airtemp,
  "Rainfall only"         = rain,
  "Radiation only"        = rad,
  "AirTemp + Rainfall"    = c(airtemp, rain),
  "AirTemp + Radiation"   = c(airtemp, rad),
  "Rainfall + Radiation"  = c(rain, rad),
  "AirTemp+Rain+Rad"      = c(airtemp, rain, rad),
  "Geo+AirTemp+Rain+Rad"  = c("LATITUDE","LONGITUDE", airtemp, rain, rad)
)

results <- data.frame()

for (name in names(combos)) {
  feats    <- combos[[name]]
  train_c  <- train[, c(feats, "ProdPerBusiness")]
  X_test_c <- X_test[, feats]

  model <- rpart(ProdPerBusiness ~ .,
                 data    = train_c,
                 method  = "anova",
                 control = rpart.control(cp = 0.005, maxdepth = 4))

  pred_train <- predict(model, newdata = train_c)
  pred_test  <- predict(model, newdata = X_test_c)

  rss <- sum((y_train - pred_train)^2)
  n   <- length(y_train)
  df  <- length(unique(model$where))
  aic <- n * log(rss/n) + 2 * df
  gap <- round(cor(y_train, pred_train)^2 - cor(y_test, pred_test)^2, 4)

  results <- rbind(results, data.frame(
    Combination = name,
    N_features  = length(feats),
    AIC         = round(aic, 2),
    Train_R2    = round(cor(y_train, pred_train)^2, 4),
    Test_RMSE   = round(rmse(y_test, pred_test), 2),
    Test_R2     = round(cor(y_test, pred_test)^2, 4),
    Test_MAE    = round(mae(y_test, pred_test), 2),
    Overfit_gap = gap,
    N_leaves    = df
  ))
}

results <- results[order(results$Test_R2, decreasing = TRUE), ]

cat("=== DT AirTemp + Rainfall + Radiation ===\n")
cat("cp=0.005 | maxdepth=4\n\n")
print(results, row.names = FALSE)

cat("\n=== BEST by Test R2 ===\n")
print(results[1, ], row.names = FALSE)

cat("\n=== BEST by AIC ===\n")
print(results[order(results$AIC), ][1, ], row.names = FALSE)

cat("\n=== BEST by Test RMSE ===\n")
print(results[order(results$Test_RMSE), ][1, ], row.names = FALSE)

# ── Plot best model ───────────────────────────────────────────
best_feats  <- combos[[results$Combination[1]]]
train_best  <- train[, c(best_feats, "ProdPerBusiness")]
X_test_best <- X_test[, best_feats]

best_model <- rpart(ProdPerBusiness ~ .,
                    data    = train_best,
                    method  = "anova",
                    control = rpart.control(cp = 0.005, maxdepth = 4))

windows()
rpart.plot(best_model,
           main = paste("Best DT:", results$Combination[1]),
           type = 4, extra = 101, cex = 0.8)

# ── Feature importance of best model ─────────────────────────
best_imp <- data.frame(
  Feature    = names(best_model$variable.importance),
  Importance = best_model$variable.importance
)
best_imp <- best_imp[order(best_imp$Importance, decreasing = TRUE), ]

cat("\nTop 15 features of best model:\n")
print(head(best_imp, 15), row.names = FALSE)

# ── Update model comparison ───────────────────────────────────
best_row <- results[1, ]
model_comparison <- rbind(model_comparison,
  data.frame(Model     = paste0("DT ", best_row$Combination),
             AIC       = best_row$AIC,
             Train_R2  = best_row$Train_R2,
             Test_RMSE = best_row$Test_RMSE,
             Test_R2   = best_row$Test_R2))

cat("\n=== MODEL COMPARISON (so far) ===\n")
print(model_comparison, row.names = FALSE)
