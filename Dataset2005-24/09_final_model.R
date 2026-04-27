# ============================================================
# 09_FINAL_MODEL.R
# Uses only features consistent across DT + LASSO + RF
# ============================================================

library(dplyr)
library(rpart)
library(rpart.plot)
library(glmnet)
library(randomForest)
library(Metrics)
library(ggplot2)

# ── Blue features: consistent across DT + LASSO + RF ─────────
blue_features <- c(
  "LATITUDE", "LONGITUDE",
  "AirTempAvg_month_3",
  "AirTempAvg_month_5",
  "AirTempAvg_month_8",
  "AirTempAvg_month_9",
  "AirTempAvg_month_10",
  "AirTempAvg_month_11",
  "AirTempAvg_month_1",
  "Evaporation_month_8",
  "Evaporation_month_12",
  "Evaporation_month_2",
  "RelHumAvg_month_9",
  "RelHumAvg_month_10",
  "RelHumAvg_month_11",
  "Radiation_month_6"
)

cat("Number of blue features:", length(blue_features), "\n")
cat("Features:\n")
print(blue_features)

# ── Prepare data ──────────────────────────────────────────────
train_blue  <- train[, c(blue_features, "ProdPerBusiness")]
X_test_blue <- X_test[, blue_features]
X_train_blue <- train_blue %>% select(all_of(blue_features))

# ============================================================
# 1. LINEAR REGRESSION
# ============================================================
train_means_b <- colMeans(X_train_blue)
train_sds_b   <- apply(X_train_blue, 2, sd)
X_train_scaled_b <- as.data.frame(scale(X_train_blue, center = train_means_b, scale = train_sds_b))
X_test_scaled_b  <- as.data.frame(scale(X_test_blue,  center = train_means_b, scale = train_sds_b))
train_scaled_b   <- X_train_scaled_b
train_scaled_b$ProdPerBusiness <- y_train

lm_blue <- lm(ProdPerBusiness ~ ., data = train_scaled_b)
pred_train_lm_b <- predict(lm_blue, newdata = X_train_scaled_b)
pred_test_lm_b  <- predict(lm_blue, newdata = X_test_scaled_b)
lm_blue_aic <- AIC(lm_blue)

cat("\n==============================\n")
cat("  LINEAR REGRESSION - BLUE\n")
cat("==============================\n")
cat("AIC:", round(lm_blue_aic, 2), "\n")
cat("TRAIN R2:  ", round(cor(y_train, pred_train_lm_b)^2, 4), "\n")
cat("TEST  RMSE:", round(rmse(y_test, pred_test_lm_b), 2), "\n")
cat("TEST  R2:  ", round(cor(y_test, pred_test_lm_b)^2, 4), "\n")

# ============================================================
# 2. LASSO
# ============================================================
X_train_mat_b <- as.matrix(X_train_blue)
X_test_mat_b  <- as.matrix(X_test_blue)

set.seed(123)
cv_lasso_b    <- cv.glmnet(X_train_mat_b, y_train, alpha = 1, nfolds = 5)
lasso_blue    <- glmnet(X_train_mat_b, y_train, alpha = 1, lambda = cv_lasso_b$lambda.min)
pred_train_lasso_b <- as.vector(predict(lasso_blue, X_train_mat_b))
pred_test_lasso_b  <- as.vector(predict(lasso_blue, X_test_mat_b))
rss_lb  <- sum((y_train - pred_train_lasso_b)^2)
lasso_blue_aic <- length(y_train) * log(rss_lb/length(y_train)) + 2 * lasso_blue$df

cat("\n==============================\n")
cat("       LASSO - BLUE\n")
cat("==============================\n")
cat("AIC:", round(lasso_blue_aic, 2), "\n")
cat("TRAIN R2:  ", round(cor(y_train, pred_train_lasso_b)^2, 4), "\n")
cat("TEST  RMSE:", round(rmse(y_test, pred_test_lasso_b), 2), "\n")
cat("TEST  R2:  ", round(cor(y_test, pred_test_lasso_b)^2, 4), "\n")

# ============================================================
# 3. DECISION TREE
# ============================================================
dt_blue <- rpart(ProdPerBusiness ~ .,
                 data    = train_blue,
                 method  = "anova",
                 control = rpart.control(cp = 0.001, maxdepth = 5))

pred_train_dt_b <- predict(dt_blue, newdata = X_train_blue)
pred_test_dt_b  <- predict(dt_blue, newdata = X_test_blue)
rss_db  <- sum((y_train - pred_train_dt_b)^2)
n_db    <- length(y_train)
df_db   <- length(unique(dt_blue$where))
dt_blue_aic <- n_db * log(rss_db/n_db) + 2 * df_db

cat("\n==============================\n")
cat("   DECISION TREE - BLUE\n")
cat("==============================\n")
cat("AIC:", round(dt_blue_aic, 2), "\n")
cat("TRAIN R2:  ", round(cor(y_train, pred_train_dt_b)^2, 4), "\n")
cat("TEST  RMSE:", round(rmse(y_test, pred_test_dt_b), 2), "\n")
cat("TEST  R2:  ", round(cor(y_test, pred_test_dt_b)^2, 4), "\n")

rpart.plot(dt_blue, main = "Decision Tree - Blue Features", type = 4, extra = 101)

# ============================================================
# 4. RANDOM FOREST
# ============================================================
set.seed(123)
rf_blue <- randomForest(ProdPerBusiness ~ .,
                        data       = train_blue,
                        ntree      = 500,
                        mtry       = floor(sqrt(length(blue_features))),
                        importance = TRUE)

pred_train_rf_b <- predict(rf_blue, newdata = X_train_blue)
pred_test_rf_b  <- predict(rf_blue, newdata = X_test_blue)
rss_rb  <- sum((y_train - pred_train_rf_b)^2)
rf_blue_aic <- n_db * log(rss_rb/n_db) + 2 * rf_blue$mtry

cat("\n==============================\n")
cat("   RANDOM FOREST - BLUE\n")
cat("==============================\n")
cat("AIC:", round(rf_blue_aic, 2), "\n")
cat("TRAIN R2:  ", round(cor(y_train, pred_train_rf_b)^2, 4), "\n")
cat("TEST  RMSE:", round(rmse(y_test, pred_test_rf_b), 2), "\n")
cat("TEST  R2:  ", round(cor(y_test, pred_test_rf_b)^2, 4), "\n")

# ============================================================
# SUMMARY
# ============================================================
cat("\n=== FINAL MODEL SUMMARY (Blue Features Only) ===\n")
cat(sprintf("%-25s %10s %10s %10s\n", "Model", "AIC", "Test RMSE", "Test R2"))
cat(sprintf("%-25s %10.2f %10.2f %10.4f\n", "Linear Regression", lm_blue_aic,
            rmse(y_test, pred_test_lm_b), cor(y_test, pred_test_lm_b)^2))
cat(sprintf("%-25s %10.2f %10.2f %10.4f\n", "LASSO", lasso_blue_aic,
            rmse(y_test, pred_test_lasso_b), cor(y_test, pred_test_lasso_b)^2))
cat(sprintf("%-25s %10.2f %10.2f %10.4f\n", "Decision Tree", dt_blue_aic,
            rmse(y_test, pred_test_dt_b), cor(y_test, pred_test_dt_b)^2))
cat(sprintf("%-25s %10.2f %10.2f %10.4f\n", "Random Forest", rf_blue_aic,
            rmse(y_test, pred_test_rf_b), cor(y_test, pred_test_rf_b)^2))
