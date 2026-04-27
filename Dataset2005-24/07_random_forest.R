# ============================================================
# 07_RANDOM_FOREST.R
# ============================================================

library(randomForest)
library(Metrics)
library(ggplot2)

set.seed(123)

# ── Train model ───────────────────────────────────────────────
rf_model <- randomForest(ProdPerBusiness ~ .,
                         data       = train,
                         ntree      = 500,
                         mtry       = floor(sqrt(ncol(X_train))),
                         importance = TRUE)

pred_train_rf <- predict(rf_model, newdata = X_train)
pred_test_rf  <- predict(rf_model, newdata = X_test)

# ── AIC ───────────────────────────────────────────────────────
rss_rf <- sum((y_train - pred_train_rf)^2)
n_rf   <- length(y_train)
df_rf  <- rf_model$mtry
rf_aic <- n_rf * log(rss_rf/n_rf) + 2 * df_rf

cat("==============================\n")
cat("    RANDOM FOREST RESULTS\n")
cat("==============================\n")
cat("AIC:", round(rf_aic, 2), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_rf), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_rf)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_rf), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_rf), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_rf)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_rf), 2), "\n")

# ── Feature importance ────────────────────────────────────────
rf_importance <- data.frame(
  Feature       = rownames(importance(rf_model)),
  IncMSE        = importance(rf_model)[, "%IncMSE"],
  IncNodePurity = importance(rf_model)[, "IncNodePurity"]
)
rf_importance <- rf_importance[order(rf_importance$IncMSE, decreasing = TRUE), ]

cat("\nTop 20 features by %IncMSE:\n")
print(head(rf_importance, 20))

# ── Plots ─────────────────────────────────────────────────────
ggplot(head(rf_importance, 20),
       aes(x = reorder(Feature, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Random Forest - Top 20 Feature Importance (%IncMSE)",
       x = "Feature", y = "%IncMSE") +
  theme_minimal()

plot(y_test, pred_test_rf,
     main = "Random Forest - Actual vs Predicted (Test)",
     xlab = "Actual ProdPerBusiness",
     ylab = "Predicted ProdPerBusiness",
     pch  = 16, col = "steelblue")
abline(0, 1, col = "red", lwd = 2)
