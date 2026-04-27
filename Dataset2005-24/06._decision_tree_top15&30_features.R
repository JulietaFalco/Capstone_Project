# ============================================================
# 06b_DECISION_TREE_TOP_FEATURES.R
# Best parameters: cp=0.001, maxdepth=5
# Tests Top 15 and Top 30 features from 06_decision_tree.R
# ============================================================

library(rpart)
library(rpart.plot)
library(Metrics)

run_dt_top <- function(n_features, label) {
  top_feat   <- head(dt_importance$Feature, n_features)
  train_top  <- train[, c(top_feat, "ProdPerBusiness")]
  X_test_top <- X_test[, top_feat]

  model <- rpart(ProdPerBusiness ~ .,
                 data    = train_top,
                 method  = "anova",
                 control = rpart.control(cp = 0.001, maxdepth = 5))

  pred_train <- predict(model, newdata = train_top)
  pred_test  <- predict(model, newdata = X_test_top)

  rss <- sum((y_train - pred_train)^2)
  n   <- length(y_train)
  df  <- length(unique(model$where))
  aic <- n * log(rss/n) + 2 * df

  cat("\n==============================\n")
  cat("   DECISION TREE -", label, "\n")
  cat("   cp=0.001 | maxdepth=5\n")
  cat("==============================\n")
  cat("AIC:", round(aic, 2), "\n")
  cat("TRAIN:\n")
  cat("  RMSE:", round(rmse(y_train, pred_train), 2), "\n")
  cat("  R2:  ", round(cor(y_train, pred_train)^2, 4), "\n")
  cat("  MAE: ", round(mae(y_train, pred_train), 2), "\n")
  cat("TEST:\n")
  cat("  RMSE:", round(rmse(y_test, pred_test), 2), "\n")
  cat("  R2:  ", round(cor(y_test, pred_test)^2, 4), "\n")
  cat("  MAE: ", round(mae(y_test, pred_test), 2), "\n")

  rpart.plot(model, main = paste("Decision Tree -", label), type = 4, extra = 101)

  return(model)
}

# ── Run both ──────────────────────────────────────────────────
cat("Top 15 features:\n")
print(head(dt_importance$Feature, 15))
dt_top15_model <- run_dt_top(15, "TOP 15 Features")

cat("\nTop 30 features:\n")
print(head(dt_importance$Feature, 30))
dt_top30_model <- run_dt_top(30, "TOP 30 Features")
