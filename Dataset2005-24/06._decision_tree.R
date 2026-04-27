# ============================================================
# 06_DECISION_TREE.R
# Part 1: Base model (cp=0.01, maxdepth=5) - All features
# Part 2: Tuning - test different cp and maxdepth combinations
# ============================================================

library(rpart)
library(rpart.plot)
library(Metrics)

# ============================================================
# PART 1 - BASE MODEL (All Features)
# ============================================================

dt_model <- rpart(ProdPerBusiness ~ .,
                  data    = train,
                  method  = "anova",
                  control = rpart.control(cp = 0.01, maxdepth = 5))

pred_train_dt <- predict(dt_model, newdata = X_train)
pred_test_dt  <- predict(dt_model, newdata = X_test)

rss_dt <- sum((y_train - pred_train_dt)^2)
n_dt   <- length(y_train)
df_dt  <- length(unique(dt_model$where))
dt_aic <- n_dt * log(rss_dt/n_dt) + 2 * df_dt

cat("==============================\n")
cat("   DECISION TREE - BASE MODEL\n")
cat("   cp=0.01 | maxdepth=5\n")
cat("==============================\n")
cat("AIC:", round(dt_aic, 2), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_dt), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_dt)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_dt), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_dt), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_dt)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_dt), 2), "\n")

dt_importance <- data.frame(
  Feature    = names(dt_model$variable.importance),
  Importance = dt_model$variable.importance
)
dt_importance <- dt_importance[order(dt_importance$Importance, decreasing = TRUE), ]

cat("\nTop 15 features by importance:\n")
print(head(dt_importance, 15))

rpart.plot(dt_model, main = "Decision Tree - Base Model (All Features)", type = 4, extra = 101)

barplot(head(dt_importance$Importance, 15),
        names.arg = head(dt_importance$Feature, 15),
        las = 2, cex.names = 0.6,
        main = "Decision Tree - Top 15 Feature Importance",
        col  = "steelblue", ylab = "Importance")

# ============================================================
# PART 2 - TUNING (cp x maxdepth grid)
# ============================================================

cp_values       <- c(0.001, 0.005, 0.01, 0.02, 0.05)
maxdepth_values <- c(3, 4, 5, 6, 7)

results <- data.frame()

for (cp in cp_values) {
  for (md in maxdepth_values) {
    model      <- rpart(ProdPerBusiness ~ .,
                        data    = train,
                        method  = "anova",
                        control = rpart.control(cp = cp, maxdepth = md))
    pred_train <- predict(model, newdata = X_train)
    pred_test  <- predict(model, newdata = X_test)
    rss        <- sum((y_train - pred_train)^2)
    df         <- length(unique(model$where))
    aic        <- n_dt * log(rss/n_dt) + 2 * df

    results <- rbind(results, data.frame(
      cp         = cp,
      maxdepth   = md,
      AIC        = round(aic, 2),
      Train_RMSE = round(rmse(y_train, pred_train), 2),
      Train_R2   = round(cor(y_train, pred_train)^2, 4),
      Test_RMSE  = round(rmse(y_test, pred_test), 2),
      Test_R2    = round(cor(y_test, pred_test)^2, 4),
      Test_MAE   = round(mae(y_test, pred_test), 2),
      N_leaves   = df
    ))
  }
}

results <- results[order(results$Test_R2, decreasing = TRUE), ]

cat("\n=== TUNING RESULTS - Sorted by Test R2 ===\n")
print(results)

cat("\n=== BEST by Test R2 ===\n")
print(results[1, ])

cat("\n=== BEST by AIC ===\n")
print(results[order(results$AIC), ][1, ])

cat("\n=== BEST by Test RMSE ===\n")
print(results[order(results$Test_RMSE), ][1, ])
