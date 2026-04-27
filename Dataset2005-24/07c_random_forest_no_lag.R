# ============================================================
# 07c_RANDOM_FOREST_NO_LAG.R
# Top 15 RF features EXCLUDING rainfall lag variables
# ============================================================

library(randomForest)
library(Metrics)
library(ggplot2)

# ── Top 15 excluding lag features ────────────────────────────
exclude <- c("Rainfall_growing_season_lag1", "Rainfall_annual_lag1")

top15_no_lag <- head(
  rf_importance$Feature[!rf_importance$Feature %in% exclude], 
  15
)

cat("Top 15 features (no lag):\n")
print(top15_no_lag)

train_no_lag  <- train[, c(top15_no_lag, "ProdPerBusiness")]
X_test_no_lag <- X_test[, top15_no_lag]

set.seed(123)
rf_no_lag_model <- randomForest(ProdPerBusiness ~ .,
                                data       = train_no_lag,
                                ntree      = 500,
                                mtry       = floor(sqrt(length(top15_no_lag))),
                                importance = TRUE)

pred_train_no_lag <- predict(rf_no_lag_model, newdata = train_no_lag)
pred_test_no_lag  <- predict(rf_no_lag_model, newdata = X_test_no_lag)

rss_no_lag <- sum((y_train - pred_train_no_lag)^2)
n_no_lag   <- length(y_train)
df_no_lag  <- rf_no_lag_model$mtry
aic_no_lag <- n_no_lag * log(rss_no_lag/n_no_lag) + 2 * df_no_lag

cat("\n==============================\n")
cat("   RF - TOP 15 (NO LAG)\n")
cat("==============================\n")
cat("AIC:", round(aic_no_lag, 2), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_no_lag), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_no_lag)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_no_lag), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_no_lag), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_no_lag)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_no_lag), 2), "\n")

rf_no_lag_imp <- data.frame(
  Feature = rownames(importance(rf_no_lag_model)),
  IncMSE  = importance(rf_no_lag_model)[, "%IncMSE"]
)
rf_no_lag_imp <- rf_no_lag_imp[order(rf_no_lag_imp$IncMSE, decreasing = TRUE), ]

cat("\nFeature importance:\n")
print(rf_no_lag_imp)

ggplot(rf_no_lag_imp,
       aes(x = reorder(Feature, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "RF Top 15 (No Lag) - Feature Importance",
       x = "Feature", y = "%IncMSE") +
  theme_minimal()

plot(y_test, pred_test_no_lag,
     main = "RF Top 15 (No Lag) - Actual vs Predicted (Test)",
     xlab = "Actual ProdPerBusiness",
     ylab = "Predicted ProdPerBusiness",
     pch  = 16, col = "steelblue")
abline(0, 1, col = "red", lwd = 2)
