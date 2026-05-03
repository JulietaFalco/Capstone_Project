# =============================================================================
# WHEAT YIELD PREDICTION — Full ML Pipeline
# Dataset: 2006–2025 | Target: Yield
# Run in Positron / RStudio
# =============================================================================

# ── 0. PACKAGES ──────────────────────────────────────────────────────────────
required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "corrplot",
  "caret", "glmnet", "rpart", "rpart.plot",
  "vip", "patchwork", "scales", "tibble"
)

new_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(required_pkgs, library, character.only = TRUE))

set.seed(42)  # global seed for reproducibility

# =============================================================================
# 1. LOAD DATA
# =============================================================================
cat("\n===== 1. LOADING DATA =====\n")

# ── Update this path if needed ────────────────────────────────────────────────
data_path <- "C:/Users/Usuario/mi-proyecto-ml/Dataset2006-25/DatasetYield.xlsx"

raw <- read_excel(data_path)
cat("Dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n")
cat("Preview of first 3 rows:\n")
print(head(raw, 3))

# ── Define feature groups ─────────────────────────────────────────────────────
climate_vars <- grep("^(AirTempAvg|Rainfall_month|WindAvg|Evaporation)_month_",
                     names(raw), value = TRUE)

geo_vars   <- c("LONGITUDE", "LATITUDE")
econ_vars  <- c("Wheat_Price", "Nitrogen_Price")
lag_vars   <- c("Rainfall_growing_season_lag1", "Rainfall_annual_lag1")
target_var <- "Yield"

all_features <- c(climate_vars, geo_vars, econ_vars, lag_vars)
cat("\nTotal features selected:", length(all_features), "\n")
cat("  Climate vars :", length(climate_vars), "\n")
cat("  Geo vars     :", length(geo_vars), "\n")
cat("  Economic vars:", length(econ_vars), "\n")
cat("  Lag vars     :", length(lag_vars), "\n")

# ── Keep only modelling columns ───────────────────────────────────────────────
df <- raw %>%
  select(all_of(c(all_features, target_var))) %>%
  mutate(across(everything(), as.numeric))

cat("\nModelling dataset dimensions:", nrow(df), "x", ncol(df), "\n")

# =============================================================================
# 2. MISSING VALUES & OUTLIERS
# =============================================================================
cat("\n===== 2. MISSING VALUES & OUTLIERS =====\n")

# ── Missing values ────────────────────────────────────────────────────────────
miss_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
  filter(Missing > 0) %>%
  arrange(desc(Missing))

if (nrow(miss_summary) == 0) {
  cat("No missing values detected.\n")
} else {
  cat("Variables with missing values:\n")
  print(miss_summary)
}

# ── Outlier detection via IQR ─────────────────────────────────────────────────
outlier_summary <- df %>%
  summarise(across(everything(), ~ {
    q <- quantile(., probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    sum(. < (q[1] - 1.5 * iqr) | . > (q[2] + 1.5 * iqr), na.rm = TRUE)
  })) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Outliers") %>%
  filter(Outliers > 0) %>%
  arrange(desc(Outliers))

cat("\nVariables with IQR-detected outliers (top 15):\n")
print(head(outlier_summary, 15))

# ── Boxplot: Yield + key climate features ────────────────────────────────────
plot_cols <- c(target_var, "AirTempAvg_month_7", "Rainfall_month_7",
               "WindAvg_month_7", "Evaporation_month_7",
               "Rainfall_growing_season_lag1")

p_box <- df %>%
  select(all_of(plot_cols)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1.5, alpha = 0.7) +
  facet_wrap(~Variable, scales = "free") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Boxplots: Yield + Key Climate Variables",
       subtitle = "Red dots = IQR outliers") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        strip.text = element_text(size = 8),
        axis.text.x = element_blank())

print(p_box)

# ── Distribution of Yield ─────────────────────────────────────────────────────
p_yield_dist <- ggplot(df, aes(x = Yield)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#4292C6", colour = "white", alpha = 0.8) +
  geom_density(colour = "#084594", linewidth = 1) +
  labs(title = "Distribution of Yield", x = "Yield (t/ha)", y = "Density") +
  theme_minimal()

print(p_yield_dist)

# =============================================================================
# 3. CORRELATION ANALYSIS
# =============================================================================
cat("\n===== 3. CORRELATION ANALYSIS =====\n")

# ── 3a. Correlation of all features with Yield ───────────────────────────────
cor_with_yield <- df %>%
  summarise(across(all_of(all_features),
                   ~ cor(., Yield, use = "pairwise.complete.obs"))) %>%
  pivot_longer(everything(), names_to = "Feature", values_to = "Correlation") %>%
  arrange(desc(abs(Correlation)))

cat("Top 20 features correlated with Yield:\n")
print(head(cor_with_yield, 20))

p_cor_yield <- cor_with_yield %>%
  slice_max(abs(Correlation), n = 20) %>%
  mutate(Feature = reorder(Feature, abs(Correlation)),
         Direction = ifelse(Correlation >= 0, "Positive", "Negative")) %>%
  ggplot(aes(x = Feature, y = Correlation, fill = Direction)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2196F3", "Negative" = "#F44336")) +
  labs(title = "Top 20 Feature Correlations with Yield",
       x = NULL, y = "Pearson r") +
  theme_minimal()

print(p_cor_yield)

# ── 3b. Correlation matrix among climate variables (monthly averages) ─────────
# Summarise to one value per climate type to keep the matrix readable
climate_summary <- df %>%
  mutate(
    AirTemp_avg  = rowMeans(select(., starts_with("AirTempAvg_month_")),   na.rm = TRUE),
    Rainfall_avg = rowMeans(select(., starts_with("Rainfall_month_")),      na.rm = TRUE),
    Wind_avg     = rowMeans(select(., starts_with("WindAvg_month_")),        na.rm = TRUE),
    Evap_avg     = rowMeans(select(., starts_with("Evaporation_month_")),    na.rm = TRUE)
  ) %>%
  select(AirTemp_avg, Rainfall_avg, Wind_avg, Evap_avg,
         Rainfall_growing_season_lag1, Rainfall_annual_lag1, Yield)

cor_climate <- cor(climate_summary, use = "pairwise.complete.obs")
cat("\nClimate variable correlation matrix:\n")
print(round(cor_climate, 3))

corrplot(cor_climate,
         method   = "color",
         type     = "upper",
         addCoef.col = "black",
         tl.col   = "black",
         tl.cex   = 0.85,
         number.cex = 0.75,
         col      = colorRampPalette(c("#D73027","white","#4575B4"))(200),
         title    = "Climate Variables — Correlation Matrix",
         mar      = c(0, 0, 2, 0))

# ── Full feature correlation matrix (heatmap) ────────────────────────────────
cor_full <- cor(df[, all_features], use = "pairwise.complete.obs")

corrplot(cor_full,
         method  = "color",
         type    = "upper",
         tl.cex  = 0.45,
         tl.col  = "black",
         col     = colorRampPalette(c("#D73027","white","#4575B4"))(200),
         title   = "All Features — Correlation Matrix",
         mar     = c(0, 0, 2, 0))

# =============================================================================
# TRAIN / TEST SPLIT  (75 / 25, shuffled)
# =============================================================================
cat("\n===== TRAIN/TEST SPLIT (75/25) =====\n")

df_clean <- df %>% drop_na()
cat("Rows after dropping NA:", nrow(df_clean), "\n")

train_idx <- createDataPartition(df_clean[[target_var]],
                                 p = 0.75, list = FALSE)
train_df <- df_clean[train_idx, ]
test_df  <- df_clean[-train_idx, ]
cat("Train rows:", nrow(train_df), "| Test rows:", nrow(test_df), "\n")

# Shuffle train set at every model call to prevent memorisation
shuffle_train <- function(data) data[sample(nrow(data)), ]

X_train <- as.matrix(train_df[, all_features])
y_train <- train_df[[target_var]]
X_test  <- as.matrix(test_df[, all_features])
y_test  <- test_df[[target_var]]

# Reusable 5-fold cross-validation control
cv_ctrl <- trainControl(method = "cv", number = 5, verboseIter = FALSE)

# Helper: print regression metrics
eval_model <- function(pred, actual, label = "") {
  rmse <- sqrt(mean((pred - actual)^2))
  mae  <- mean(abs(pred - actual))
  ss_res <- sum((actual - pred)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2   <- 1 - ss_res / ss_tot
  cat(sprintf("  [%s]  RMSE: %.4f | MAE: %.4f | R²: %.4f\n", label, rmse, mae, r2))
  invisible(list(rmse = rmse, mae = mae, r2 = r2))
}

# =============================================================================
# 4a. LASSO — BASELINE (all features)
# =============================================================================
cat("\n===== 4a. LASSO BASELINE =====\n")

set.seed(42)
lasso_cv <- cv.glmnet(
  x            = X_train[sample(nrow(X_train)), ],   # shuffle
  y            = y_train[sample(length(y_train))],
  alpha        = 1,
  nfolds       = 5,
  standardize  = TRUE
)

cat("Optimal lambda (min):", round(lasso_cv$lambda.min, 6), "\n")
cat("Optimal lambda (1se):", round(lasso_cv$lambda.1se, 6), "\n")

lasso_pred_train <- predict(lasso_cv, newx = X_train, s = "lambda.min")[, 1]
lasso_pred_test  <- predict(lasso_cv, newx = X_test,  s = "lambda.min")[, 1]

cat("Training performance:\n"); eval_model(lasso_pred_train, y_train, "LASSO-Train")
cat("Test performance:\n");     eval_model(lasso_pred_test,  y_test,  "LASSO-Test")

# Coefficients
lasso_coef <- coef(lasso_cv, s = "lambda.min")
lasso_coef_df <- tibble(
  Feature     = rownames(lasso_coef)[-1],
  Coefficient = as.numeric(lasso_coef[-1])
) %>% filter(Coefficient != 0) %>% arrange(desc(abs(Coefficient)))

cat("\nNon-zero LASSO coefficients:\n")
print(lasso_coef_df)

p_lasso_coef <- lasso_coef_df %>%
  mutate(Feature = reorder(Feature, abs(Coefficient)),
         Sign    = ifelse(Coefficient > 0, "Positive", "Negative")) %>%
  ggplot(aes(x = Feature, y = Coefficient, fill = Sign)) +
  geom_col(alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2196F3", "Negative" = "#F44336")) +
  labs(title = "LASSO Baseline — Non-zero Coefficients",
       x = NULL, y = "Coefficient") +
  theme_minimal()

print(p_lasso_coef)

# =============================================================================
# 4b. LASSO — TOP-15 FEATURES (VIP)
# =============================================================================
cat("\n===== 4b. LASSO WITH TOP-15 FEATURES =====\n")

# Feature importance via absolute coefficient at lambda.min
lasso_vip <- tibble(
  Feature    = rownames(lasso_coef)[-1],
  Importance = abs(as.numeric(lasso_coef[-1]))
) %>% arrange(desc(Importance))

top15_lasso <- head(lasso_vip$Feature, 15)
cat("Top 15 features for LASSO:\n")
print(top15_lasso)

p_lasso_vip <- lasso_vip %>%
  slice_max(Importance, n = 15) %>%
  mutate(Feature = reorder(Feature, Importance)) %>%
  ggplot(aes(x = Feature, y = Importance)) +
  geom_col(fill = "#0277BD", alpha = 0.85) +
  coord_flip() +
  labs(title = "LASSO Feature Importance (|coeff|) — Top 15",
       x = NULL, y = "|Coefficient|") +
  theme_minimal()

print(p_lasso_vip)

X_train_l15 <- X_train[, top15_lasso]
X_test_l15  <- X_test[, top15_lasso]

set.seed(42)
lasso_top15_cv <- cv.glmnet(
  x           = X_train_l15[sample(nrow(X_train_l15)), ],
  y           = y_train[sample(length(y_train))],
  alpha       = 1,
  nfolds      = 5,
  standardize = TRUE
)

lasso15_pred_train <- predict(lasso_top15_cv, newx = X_train_l15, s = "lambda.min")[, 1]
lasso15_pred_test  <- predict(lasso_top15_cv, newx = X_test_l15,  s = "lambda.min")[, 1]

cat("Training performance:\n"); eval_model(lasso15_pred_train, y_train, "LASSO-Top15-Train")
cat("Test performance:\n");     eval_model(lasso15_pred_test,  y_test,  "LASSO-Top15-Test")

# Actual vs Predicted plot — LASSO top 15
p_lasso_avp <- ggplot(data.frame(Actual = y_test, Predicted = lasso15_pred_test),
                      aes(x = Actual, y = Predicted)) +
  geom_point(colour = "#0277BD", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "dashed") +
  labs(title = "LASSO Top-15: Actual vs Predicted (Test)",
       x = "Actual Yield", y = "Predicted Yield") +
  theme_minimal()

print(p_lasso_avp)

# =============================================================================
# 5. DECISION TREE — BASELINE (all features)
# =============================================================================
cat("\n===== 5. DECISION TREE BASELINE =====\n")

set.seed(42)
dt_train_shuf <- shuffle_train(train_df)

dt_base <- rpart(
  formula = Yield ~ .,
  data    = dt_train_shuf[, c(all_features, target_var)],
  method  = "anova",
  control = rpart.control(
    cp      = 0.001,
    minsplit = 10,
    maxdepth = 6
  )
)

# Prune using 1-SE rule
best_cp_base <- dt_base$cptable[
  which.min(dt_base$cptable[, "xerror"]), "CP"
]
dt_base_pruned <- prune(dt_base, cp = best_cp_base)

rpart.plot(dt_base_pruned,
           main   = "Decision Tree Baseline (Pruned)",
           type   = 4, extra = 101,
           box.palette = "Blues", shadow.col = "gray",
           nn = TRUE)

dt_pred_train <- predict(dt_base_pruned, newdata = train_df)
dt_pred_test  <- predict(dt_base_pruned, newdata = test_df)

cat("Training performance:\n"); eval_model(dt_pred_train, y_train, "DT-Baseline-Train")
cat("Test performance:\n");     eval_model(dt_pred_test,  y_test,  "DT-Baseline-Test")

# Variable importance
dt_imp <- as_tibble(dt_base_pruned$variable.importance,
                    rownames = "Feature") %>%
  rename(Importance = value) %>%
  arrange(desc(Importance))

cat("\nDecision Tree variable importance (top 15):\n")
print(head(dt_imp, 15))

# =============================================================================
# 6. DECISION TREE — TOP-15 FEATURES
# =============================================================================
cat("\n===== 6. DECISION TREE WITH TOP-15 FEATURES =====\n")

top15_dt <- head(dt_imp$Feature, 15)
cat("Top 15 features for DT:\n")
print(top15_dt)

p_dt_vip <- head(dt_imp, 15) %>%
  mutate(Feature = reorder(Feature, Importance)) %>%
  ggplot(aes(x = Feature, y = Importance)) +
  geom_col(fill = "#388E3C", alpha = 0.85) +
  coord_flip() +
  labs(title = "Decision Tree Feature Importance — Top 15",
       x = NULL, y = "Importance") +
  theme_minimal()

print(p_dt_vip)

set.seed(42)
dt_t15_shuf <- shuffle_train(train_df[, c(top15_dt, target_var)])

dt_top15 <- rpart(
  formula = Yield ~ .,
  data    = dt_t15_shuf,
  method  = "anova",
  control = rpart.control(cp = 0.001, minsplit = 10, maxdepth = 6)
)

best_cp_t15 <- dt_top15$cptable[
  which.min(dt_top15$cptable[, "xerror"]), "CP"
]
dt_top15_pruned <- prune(dt_top15, cp = best_cp_t15)

rpart.plot(dt_top15_pruned,
           main   = "Decision Tree Top-15 Features (Pruned)",
           type   = 4, extra = 101,
           box.palette = "Greens", shadow.col = "gray",
           nn = TRUE)

dt15_pred_train <- predict(dt_top15_pruned, newdata = train_df[, c(top15_dt, target_var)])
dt15_pred_test  <- predict(dt_top15_pruned, newdata = test_df[, c(top15_dt, target_var)])

cat("Training performance:\n"); eval_model(dt15_pred_train, y_train, "DT-Top15-Train")
cat("Test performance:\n");     eval_model(dt15_pred_test,  y_test,  "DT-Top15-Test")

# Actual vs Predicted — DT top 15
p_dt15_avp <- ggplot(data.frame(Actual = y_test, Predicted = dt15_pred_test),
                     aes(x = Actual, y = Predicted)) +
  geom_point(colour = "#388E3C", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "dashed") +
  labs(title = "DT Top-15: Actual vs Predicted (Test)",
       x = "Actual Yield", y = "Predicted Yield") +
  theme_minimal()

print(p_dt15_avp)

# =============================================================================
# 7. DECISION TREE — VARIABLE COMBINATIONS
# =============================================================================
cat("\n===== 7. DECISION TREE — VARIABLE COMBINATIONS =====\n")

# ── Helper: build, prune, evaluate, plot DT ───────────────────────────────────
run_dt_combo <- function(features, label, palette = "Blues") {
  cat(sprintf("\n--- Combo: %s ---\n", label))
  cat("Features:", paste(features, collapse = ", "), "\n")

  cols      <- intersect(features, names(train_df))
  train_sub <- shuffle_train(train_df[, c(cols, target_var)])
  test_sub  <- test_df[, c(cols, target_var)]

  set.seed(42)
  dt_fit <- rpart(
    formula = Yield ~ .,
    data    = train_sub,
    method  = "anova",
    control = rpart.control(cp = 0.001, minsplit = 10, maxdepth = 6)
  )
  best_cp <- dt_fit$cptable[which.min(dt_fit$cptable[, "xerror"]), "CP"]
  dt_pruned <- prune(dt_fit, cp = best_cp)

  rpart.plot(dt_pruned,
             main        = paste("DT:", label),
             type        = 4, extra = 101,
             box.palette = palette,
             shadow.col  = "gray", nn = TRUE)

  pred_train <- predict(dt_pruned, newdata = train_sub)
  pred_test  <- predict(dt_pruned, newdata = test_sub)
  cat("Train:\n"); eval_model(pred_train, y_train, paste(label, "Train"))
  cat("Test:\n");  eval_model(pred_test,  y_test,  paste(label, "Test"))

  invisible(dt_pruned)
}

# ── All monthly rain and geo variables ────────────────────────────────────────
rain_vars <- grep("^Rainfall_month_", names(df), value = TRUE)

# Combo 1: Geo + Rain
run_dt_combo(
  features = c(geo_vars, rain_vars),
  label    = "Combo1: Geo + Rain",
  palette  = "Blues"
)

# Combo 2: Geo + Rain + Wheat Price
run_dt_combo(
  features = c(geo_vars, rain_vars, "Wheat_Price"),
  label    = "Combo2: Geo + Rain + Wheat_Price",
  palette  = "Purples"
)

# Combo 3: Geo + Air Temp + Rain
airtemp_vars <- grep("^AirTempAvg_month_", names(df), value = TRUE)
run_dt_combo(
  features = c(geo_vars, airtemp_vars, rain_vars),
  label    = "Combo3: Geo + AirTemp + Rain",
  palette  = "Oranges"
)

# Combo 4: Economic + Climate (all)
wind_vars <- grep("^WindAvg_month_",        names(df), value = TRUE)
evap_vars <- grep("^Evaporation_month_",    names(df), value = TRUE)

run_dt_combo(
  features = c(econ_vars, lag_vars, rain_vars, airtemp_vars, wind_vars, evap_vars),
  label    = "Combo4: Economic + Climate",
  palette  = "Reds"
)

# =============================================================================
# 8. PERFORMANCE SUMMARY TABLE
# =============================================================================
cat("\n===== 8. PERFORMANCE SUMMARY =====\n")

compute_metrics <- function(pred, actual) {
  rmse <- sqrt(mean((pred - actual)^2))
  mae  <- mean(abs(pred - actual))
  r2   <- 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
  round(c(RMSE = rmse, MAE = mae, R2 = r2), 4)
}

summary_df <- rbind(
  data.frame(Model = "LASSO Baseline",
             t(compute_metrics(lasso_pred_test,   y_test))),
  data.frame(Model = "LASSO Top-15",
             t(compute_metrics(lasso15_pred_test,  y_test))),
  data.frame(Model = "DT Baseline",
             t(compute_metrics(dt_pred_test,       y_test))),
  data.frame(Model = "DT Top-15",
             t(compute_metrics(dt15_pred_test,     y_test)))
)

cat("\nTest Set Performance Summary:\n")
print(summary_df, row.names = FALSE)

p_summary <- summary_df %>%
  pivot_longer(cols = c(RMSE, MAE, R2),
               names_to  = "Metric",
               values_to = "Value") %>%
  ggplot(aes(x = Model, y = Value, fill = Model)) +
  geom_col(alpha = 0.85) +
  facet_wrap(~Metric, scales = "free_y") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Model Comparison — Test Set Metrics",
       x = NULL, y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_summary)

cat("\n===== PIPELINE COMPLETE =====\n")
