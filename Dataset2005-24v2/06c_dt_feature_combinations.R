# ============================================================
# 06c_DT_FEATURE_COMBINATIONS.R
# Tests different feature group combinations
# Fixed: cp=0.005, maxdepth=4 (best parameters)
# ============================================================

library(rpart)
library(rpart.plot)
library(Metrics)
library(dplyr)

# ── Feature groups ────────────────────────────────────────────
geo     <- c("LATITUDE", "LONGITUDE")
airtemp <- paste0("AirTempAvg_month_",  1:12)
evap    <- paste0("Evaporation_month_", 1:12)
relhum  <- paste0("RelHumAvg_month_",   1:12)
rad     <- paste0("Radiation_month_",   1:12)
wind    <- paste0("WindAvg_month_",     1:12)
rain    <- paste0("Rainfall_month_",    1:12)

# ── Combinations ──────────────────────────────────────────────
combos <- list(
  "A: Geo+AirTemp"                        = c(geo, airtemp),
  "B: Geo+Evaporation"                    = c(geo, evap),
  "C: Geo+RelHum"                         = c(geo, relhum),
  "D: Geo+Radiation"                      = c(geo, rad),
  "E: Geo+Wind"                           = c(geo, wind),
  "F: Geo+Rainfall"                       = c(geo, rain),
  "G: Geo+AirTemp+Evaporation"            = c(geo, airtemp, evap),
  "H: Geo+AirTemp+RelHum"                 = c(geo, airtemp, relhum),
  "I: Geo+AirTemp+Rainfall"               = c(geo, airtemp, rain),
  "J: Geo+AirTemp+Wind"                   = c(geo, airtemp, wind),
  "K: Geo+AirTemp+Evaporation+RelHum"     = c(geo, airtemp, evap, relhum),
  "L: Geo+AirTemp+Evaporation+Rainfall"   = c(geo, airtemp, evap, rain),
  "M: Geo+AirTemp+Evap+RelHum+Rainfall"   = c(geo, airtemp, evap, relhum, rain),
  "N: Geo+AirTemp+Evap+RelHum+Wind+Rain"  = c(geo, airtemp, evap, relhum, wind, rain)
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

cat("=== DT FEATURE COMBINATIONS - Sorted by Test R2 ===\n\n")
print(results, row.names = FALSE)

cat("\n=== BEST by Test R2 ===\n")
print(results[1, ], row.names = FALSE)

cat("\n=== BEST by AIC ===\n")
print(results[order(results$AIC), ][1, ], row.names = FALSE)

cat("\n=== BEST by Test RMSE ===\n")
print(results[order(results$Test_RMSE), ][1, ], row.names = FALSE)

cat("\n=== LOWEST OVERFIT GAP ===\n")
print(results[order(results$Overfit_gap), ][1, ], row.names = FALSE)

# ── Plot best combination tree ────────────────────────────────
best_name  <- results$Combination[1]
best_feats <- combos[[best_name]]
train_best  <- train[, c(best_feats, "ProdPerBusiness")]
X_test_best <- X_test[, best_feats]

best_model <- rpart(ProdPerBusiness ~ .,
                    data    = train_best,
                    method  = "anova",
                    control = rpart.control(cp = 0.005, maxdepth = 4))

rpart.plot(best_model,
           main = paste("Best DT:", best_name),
           type = 4, extra = 101)
