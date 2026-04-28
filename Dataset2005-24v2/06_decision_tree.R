# ============================================================
# 06_DECISION_TREE.R
# Step 1: Baseline model (all features)
# Step 2: Feature importance
# Step 3: Tuning (cp x maxdepth grid)
# ============================================================

library(rpart)
library(rpart.plot)
library(Metrics)
library(dplyr)

# ============================================================
# STEP 1 - BASELINE (cp=0.01, maxdepth=5)
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
cat("   DECISION TREE - BASELINE\n")
cat("   cp=0.01 | maxdepth=5\n")
cat("==============================\n")
cat("AIC:  ", round(dt_aic, 2), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_dt), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_dt)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_dt), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_dt), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_dt)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_dt), 2), "\n")

# ============================================================
# STEP 2 - FEATURE IMPORTANCE
# ============================================================

dt_importance <- data.frame(
  Feature    = names(dt_model$variable.importance),
  Importance = dt_model$variable.importance
)
dt_importance <- dt_importance[order(dt_importance$Importance, decreasing = TRUE), ]

cat("\nTop 20 features by importance:\n")
print(head(dt_importance, 20), row.names = FALSE)

rpart.plot(dt_model, main = "Decision Tree - Baseline", type = 4, extra = 101)

barplot(head(dt_importance$Importance, 15),
        names.arg = head(dt_importance$Feature, 15),
        las = 2, cex.names = 0.6,
        main = "Decision Tree - Top 15 Feature Importance",
        col  = "steelblue", ylab = "Importance")

# ============================================================
# STEP 3 - TUNING (cp x maxdepth grid)
# ============================================================

cp_values       <- c(0.0001, 0.0005, 0.001, 0.005, 0.01)
maxdepth_values <- c(4, 5, 6, 7, 8)

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

# ── Update model comparison ───────────────────────────────────
model_comparison <- rbind(model_comparison,
  data.frame(Model     = "DT Baseline",
             AIC       = round(dt_aic, 2),
             Train_R2  = round(cor(y_train, pred_train_dt)^2, 4),
             Test_RMSE = round(rmse(y_test, pred_test_dt), 2),
             Test_R2   = round(cor(y_test, pred_test_dt)^2, 4))
)

cat("\n=== MODEL COMPARISON (so far) ===\n")
print(model_comparison, row.names = FALSE)

# ============================================================
# 06b_DECISION_TREE_TUNED.R
# Best parameters: cp=0.005, maxdepth=4
# ============================================================

library(rpart)
library(rpart.plot)
library(Metrics)
library(dplyr)

# ── Train model ───────────────────────────────────────────────
dt_tuned <- rpart(ProdPerBusiness ~ .,
                  data    = train,
                  method  = "anova",
                  control = rpart.control(cp = 0.005, maxdepth = 4))

pred_train_tuned <- predict(dt_tuned, newdata = X_train)
pred_test_tuned  <- predict(dt_tuned, newdata = X_test)

rss_t <- sum((y_train - pred_train_tuned)^2)
n_t   <- length(y_train)
df_t  <- length(unique(dt_tuned$where))
aic_t <- n_t * log(rss_t/n_t) + 2 * df_t

cat("==============================\n")
cat("   DECISION TREE - TUNED\n")
cat("   cp=0.005 | maxdepth=4\n")
cat("==============================\n")
cat("AIC:        ", round(aic_t, 2), "\n")
cat("N leaves:   ", df_t, "\n")
cat("Overfit gap:", round(cor(y_train, pred_train_tuned)^2 -
                          cor(y_test,  pred_test_tuned)^2, 4), "\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_tuned), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_tuned)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_tuned), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_tuned), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_tuned)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_tuned), 2), "\n")

# ── Feature importance ────────────────────────────────────────
dt_tuned_imp <- data.frame(
  Feature    = names(dt_tuned$variable.importance),
  Importance = dt_tuned$variable.importance
)
dt_tuned_imp <- dt_tuned_imp[order(dt_tuned_imp$Importance, decreasing = TRUE), ]

cat("\nTop 15 features:\n")
print(head(dt_tuned_imp, 15), row.names = FALSE)

# ── Plots ─────────────────────────────────────────────────────
rpart.plot(dt_tuned,
           main = "Decision Tree - Tuned (cp=0.005, maxdepth=4)",
           type = 4, extra = 101)

barplot(head(dt_tuned_imp$Importance, 15),
        names.arg = head(dt_tuned_imp$Feature, 15),
        las = 2, cex.names = 0.6,
        main = "DT Tuned - Top 15 Feature Importance",
        col  = "steelblue", ylab = "Importance")

# ── Actual vs Predicted ───────────────────────────────────────
plot(y_test, pred_test_tuned,
     main = "DT Tuned - Actual vs Predicted (Test)",
     xlab = "Actual ProdPerBusiness",
     ylab = "Predicted ProdPerBusiness",
     pch  = 16, col = "steelblue")
abline(0, 1, col = "red", lwd = 2)

# ── Update model comparison ───────────────────────────────────
model_comparison <- rbind(model_comparison,
  data.frame(Model     = "DT Tuned (cp=0.005, md=4)",
             AIC       = round(aic_t, 2),
             Train_R2  = round(cor(y_train, pred_train_tuned)^2, 4),
             Test_RMSE = round(rmse(y_test, pred_test_tuned), 2),
             Test_R2   = round(cor(y_test, pred_test_tuned)^2, 4))
)

cat("\n=== MODEL COMPARISON (so far) ===\n")
print(model_comparison, row.names = FALSE)
