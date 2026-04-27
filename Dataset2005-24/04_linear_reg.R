# ============================================================
# 04_LINEAR_REG.R
# Includes normalisation (Z-score) for Linear Regression only
# ============================================================

library(Metrics)

# ── Normalisation (Z-score) ───────────────────────────────────
# Using TRAIN mean and sd only to avoid data leakage
train_means <- colMeans(X_train)
train_sds   <- apply(X_train, 2, sd)

X_train_scaled <- as.data.frame(scale(X_train, center = train_means, scale = train_sds))
X_test_scaled  <- as.data.frame(scale(X_test,  center = train_means, scale = train_sds))

train_scaled <- X_train_scaled
train_scaled$ProdPerBusiness <- y_train

cat("=== Normalisation complete ===\n")
cat("Mean of first feature in train (should be ~0):", round(mean(X_train_scaled[,1]), 4), "\n")
cat("SD of first feature in train (should be ~1):  ", round(sd(X_train_scaled[,1]), 4), "\n\n")

# ── Train model ───────────────────────────────────────────────
lm_model <- lm(ProdPerBusiness ~ ., data = train_scaled)

# ── AIC ───────────────────────────────────────────────────────
lm_aic <- AIC(lm_model)
cat("AIC:", round(lm_aic, 2), "\n")

# ── Predictions ───────────────────────────────────────────────
pred_train_lm <- predict(lm_model, newdata = X_train_scaled)
pred_test_lm  <- predict(lm_model, newdata = X_test_scaled)

# ── Metrics ───────────────────────────────────────────────────
cat("==============================\n")
cat("  LINEAR REGRESSION RESULTS\n")
cat("==============================\n")
cat("TRAIN:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_lm), 2), "\n")
cat("  R2:  ", round(cor(y_train, pred_train_lm)^2, 4), "\n")
cat("  MAE: ", round(mae(y_train, pred_train_lm), 2), "\n")
cat("TEST:\n")
cat("  RMSE:", round(rmse(y_test, pred_test_lm), 2), "\n")
cat("  R2:  ", round(cor(y_test, pred_test_lm)^2, 4), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_lm), 2), "\n")

# ── Feature importance (significant coefficients) ─────────────
lm_summary <- summary(lm_model)
lm_coef <- as.data.frame(lm_summary$coefficients)
lm_coef$Feature <- rownames(lm_coef)
lm_coef <- lm_coef[lm_coef$Feature != "(Intercept)", ]
lm_coef <- lm_coef[order(lm_coef$`Pr(>|t|)`), ]

cat("\nTop 15 significant features (by p-value):\n")
print(round(head(lm_coef[, c("Estimate","Pr(>|t|)")], 15), 4))

# ── Actual vs Predicted plot ──────────────────────────────────
plot(y_test, pred_test_lm,
     main = "Linear Regression - Actual vs Predicted (Test)",
     xlab = "Actual ProdPerBusiness",
     ylab = "Predicted ProdPerBusiness",
     pch  = 16, col = "steelblue")
abline(0, 1, col = "red", lwd = 2)

lm_coef[grep("lag|Rainfall|Wheat", rownames(lm_coef)), c("Estimate", "Pr(>|t|)")]