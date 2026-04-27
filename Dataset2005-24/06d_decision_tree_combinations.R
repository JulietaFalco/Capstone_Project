# ============================================================
# 06d_DECISION_TREE_COMBINATIONS.R
# Tests different feature group combinations
# cp=0.001, maxdepth=5 (best parameters from tuning)
# ============================================================

library(rpart)
library(rpart.plot)
library(Metrics)

# ── Feature groups ────────────────────────────────────────────
geo       <- c("LATITUDE", "LONGITUDE")
airtemp   <- paste0("AirTempAvg_month_", 1:12)
windavg   <- paste0("WindAvg_month_",    1:12)
rainfall  <- paste0("Rainfall_month_",   1:12)
radiation <- paste0("Radiation_month_",  1:12)
evap      <- paste0("Evaporation_month_",1:12)
relhum    <- paste0("RelHumAvg_month_",  1:12)
lag       <- c("Rainfall_growing_season_lag1", "Rainfall_annual_lag1")
economic  <- c("Nitrogen_Price", "Wheat_Price")

# ── Combinations to test ──────────────────────────────────────
combos <- list(
  "Geo+AirTemp+Wind+Lag+Econ"      = c(geo, airtemp, windavg, lag, economic),
  "Geo+AirTemp+Wind+Econ"          = c(geo, airtemp, windavg, economic),
  "Geo+AirTemp+Wind+Lag"           = c(geo, airtemp, windavg, lag),
  "Geo+AirTemp+Wind"               = c(geo, airtemp, windavg),
  "Geo+AirTemp+Wind+Rain+Lag+Econ" = c(geo, airtemp, windavg, rainfall, lag, economic),
  "Geo+AirTemp+Wind+Rad+Lag+Econ"  = c(geo, airtemp, windavg, radiation, lag, economic)
)

results <- data.frame()

for (name in names(combos)) {
  feats <- combos[[name]]
  
  train_c  <- train[, c(feats, "ProdPerBusiness")]
  X_test_c <- X_test[, feats]
  
  model <- rpart(ProdPerBusiness ~ .,
                 data    = train_c,
                 method  = "anova",
                 control = rpart.control(cp = 0.001, maxdepth = 5))
  
  pred_train <- predict(model, newdata = train_c)
  pred_test  <- predict(model, newdata = X_test_c)
  
  rss <- sum((y_train - pred_train)^2)
  n   <- length(y_train)
  df  <- length(unique(model$where))
  aic <- n * log(rss/n) + 2 * df
  
  results <- rbind(results, data.frame(
    Combination = name,
    N_features  = length(feats),
    AIC         = round(aic, 2),
    Train_R2    = round(cor(y_train, pred_train)^2, 4),
    Test_RMSE   = round(rmse(y_test, pred_test), 2),
    Test_R2     = round(cor(y_test, pred_test)^2, 4),
    Test_MAE    = round(mae(y_test, pred_test), 2)
  ))
}

results <- results[order(results$Test_R2, decreasing = TRUE), ]

cat("=== FEATURE COMBINATION RESULTS - Sorted by Test R2 ===\n\n")
print(results, row.names = FALSE)

cat("\n=== BEST by Test R2 ===\n")
print(results[1, ], row.names = FALSE)

cat("\n=== BEST by AIC ===\n")
print(results[order(results$AIC), ][1, ], row.names = FALSE)

cat("\n=== BEST by Test RMSE ===\n")
print(results[order(results$Test_RMSE), ][1, ], row.names = FALSE)

# ── Plot best combination tree ────────────────────────────────
best_feats <- combos[[results$Combination[1]]]
train_best  <- train[, c(best_feats, "ProdPerBusiness")]
X_test_best <- X_test[, best_feats]

best_model <- rpart(ProdPerBusiness ~ .,
                    data    = train_best,
                    method  = "anova",
                    control = rpart.control(cp = 0.001, maxdepth = 5))

rpart.plot(best_model,
           main = paste("Best DT:", results$Combination[1]),
           type = 4, extra = 101)
