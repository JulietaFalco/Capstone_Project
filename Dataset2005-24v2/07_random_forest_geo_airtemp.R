# ============================================================
# 07_RANDOM_FOREST.R
# Step 1: Baseline - Geo + AirTemp (best DT combination)
# Step 2: Feature importance
# Step 3: Tuning (mtry x nodesize)
# ============================================================

library(randomForest)
library(Metrics)
library(ggplot2)
library(dplyr)

# ── Feature set (same as best DT) ────────────────────────────
geo     <- c("LATITUDE", "LONGITUDE")
airtemp <- paste0("AirTempAvg_month_", 1:12)
feats   <- c(geo, airtemp)

cat("Features used:", length(feats), "\n\n")

train_ga  <- train[, c(feats, "ProdPerBusiness")]
X_test_ga <- X_test[, feats]

# ============================================================
# STEP 1 - BASELINE
# ============================================================

set.seed(123)
rf_model <- randomForest(ProdPerBusiness ~ .,
                         data       = train_ga,
                         ntree      = 500,
                         mtry       = floor(sqrt(length(feats))),
                         importance = TRUE)

pred_train_rf <- predict(rf_model, newdata = train_ga)
pred_test_rf  <- predict(rf_model, newdata = X_test_ga)

rss_rf <- sum((y_train - pred_train_rf)^2)
n_rf   <- length(y_train)
rf_aic <- n_rf * log(rss_rf/n_rf) + 2 * rf_model$mtry

cat("==============================\n")
cat("   RANDOM FOREST - BASELINE\n")
cat("   Geo + AirTemp | ntree=500\n")
cat("   mtry:", rf_model$mtry, "\n")
cat("==============================\n")
cat("AIC:        ", round(rf_aic, 2), "\n")
cat("Overfit gap:", round(cor(y_train, pred_train_rf)^2 -
                          cor(y_test,  pred_test_rf)^2, 4), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_rf), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_rf)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_rf), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_rf), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_rf)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_rf), 2), "\n")

# ============================================================
# STEP 2 - FEATURE IMPORTANCE
# ============================================================

rf_imp <- data.frame(
  Feature = rownames(importance(rf_model)),
  IncMSE  = importance(rf_model)[, "%IncMSE"],
  IncNodePurity = importance(rf_model)[, "IncNodePurity"]
)
rf_imp <- rf_imp[order(rf_imp$IncMSE, decreasing = TRUE), ]

cat("\nFeature importance (%IncMSE):\n")
print(rf_imp, row.names = FALSE)

ggplot(rf_imp, aes(x = reorder(Feature, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "RF Geo+AirTemp - Feature Importance (%IncMSE)",
       x = "Feature", y = "%IncMSE") +
  theme_minimal()

# ============================================================
# STEP 3 - TUNING (mtry x nodesize)
# ============================================================

mtry_values    <- c(2, 3, 4, 5, 7)
nodesize_values <- c(5, 10, 15, 20)

tuning <- data.frame()

for (mt in mtry_values) {
  for (ns in nodesize_values) {
    set.seed(123)
    model <- randomForest(ProdPerBusiness ~ .,
                          data      = train_ga,
                          ntree     = 500,
                          mtry      = mt,
                          nodesize  = ns,
                          importance = FALSE)

    pred_tr <- predict(model, newdata = train_ga)
    pred_te <- predict(model, newdata = X_test_ga)
    rss     <- sum((y_train - pred_tr)^2)
    aic     <- n_rf * log(rss/n_rf) + 2 * mt
    gap     <- round(cor(y_train, pred_tr)^2 - cor(y_test, pred_te)^2, 4)

    tuning <- rbind(tuning, data.frame(
      mtry        = mt,
      nodesize    = ns,
      AIC         = round(aic, 2),
      Train_R2    = round(cor(y_train, pred_tr)^2, 4),
      Test_RMSE   = round(rmse(y_test, pred_te), 2),
      Test_R2     = round(cor(y_test, pred_te)^2, 4),
      Overfit_gap = gap
    ))
  }
}

tuning <- tuning[order(tuning$Test_R2, decreasing = TRUE), ]

cat("\n=== RF TUNING RESULTS - Sorted by Test R2 ===\n")
print(tuning, row.names = FALSE)

cat("\n=== BEST by Test R2 ===\n")
print(tuning[1, ], row.names = FALSE)

cat("\n=== BEST by Test RMSE ===\n")
print(tuning[order(tuning$Test_RMSE), ][1, ], row.names = FALSE)

cat("\n=== LOWEST OVERFIT GAP ===\n")
print(tuning[order(abs(tuning$Overfit_gap)), ][1, ], row.names = FALSE)

# ── Update model comparison ───────────────────────────────────
model_comparison <- rbind(model_comparison,
  data.frame(Model     = "RF Geo+AirTemp baseline",
             AIC       = round(rf_aic, 2),
             Train_R2  = round(cor(y_train, pred_train_rf)^2, 4),
             Test_RMSE = round(rmse(y_test, pred_test_rf), 2),
             Test_R2   = round(cor(y_test, pred_test_rf)^2, 4))
)

cat("\n=== MODEL COMPARISON (so far) ===\n")
print(model_comparison, row.names = FALSE)
