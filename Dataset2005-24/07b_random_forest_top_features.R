# ============================================================
# 07b_RANDOM_FOREST_TOP_FEATURES.R
# Top 15 features from RF importance
# ============================================================

library(randomForest)
library(Metrics)
library(ggplot2)

# ── Top 15 features from RF ───────────────────────────────────
top15_rf <- head(rf_importance$Feature, 15)
cat("Top 15 features used:\n")
print(top15_rf)

train_top15  <- train[, c(top15_rf, "ProdPerBusiness")]
X_test_top15 <- X_test[, top15_rf]

set.seed(123)
rf_top15_model <- randomForest(ProdPerBusiness ~ .,
                               data       = train_top15,
                               ntree      = 500,
                               mtry       = floor(sqrt(length(top15_rf))),
                               importance = TRUE)

pred_train_rf15 <- predict(rf_top15_model, newdata = train_top15)
pred_test_rf15  <- predict(rf_top15_model, newdata = X_test_top15)

rss_rf15 <- sum((y_train - pred_train_rf15)^2)
n_rf15   <- length(y_train)
df_rf15  <- rf_top15_model$mtry
aic_rf15 <- n_rf15 * log(rss_rf15/n_rf15) + 2 * df_rf15

cat("\n==============================\n")
cat("   RF - TOP 15 FEATURES\n")
cat("==============================\n")
cat("AIC:", round(aic_rf15, 2), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_rf15), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_rf15)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_rf15), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_rf15), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_rf15)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_rf15), 2), "\n")

# ── Feature importance ────────────────────────────────────────
rf15_importance <- data.frame(
  Feature = rownames(importance(rf_top15_model)),
  IncMSE  = importance(rf_top15_model)[, "%IncMSE"]
)
rf15_importance <- rf15_importance[order(rf15_importance$IncMSE, decreasing = TRUE), ]

cat("\nFeature importance:\n")
print(rf15_importance)

# ── Plots ─────────────────────────────────────────────────────
ggplot(rf15_importance,
       aes(x = reorder(Feature, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "RF Top 15 - Feature Importance (%IncMSE)",
       x = "Feature", y = "%IncMSE") +
  theme_minimal()

plot(y_test, pred_test_rf15,
     main = "RF Top 15 - Actual vs Predicted (Test)",
     xlab = "Actual ProdPerBusiness",
     ylab = "Predicted ProdPerBusiness",
     pch  = 16, col = "steelblue")
abline(0, 1, col = "red", lwd = 2)
