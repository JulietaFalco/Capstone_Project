# ============================================================
#  Lasso Regression — Lambda Comparison
#  DATA6000 · Capstone — Julieta Falco · 1849967
#
#  Purpose: Test different lambda (regularisation) values and
#           compare Train/Test RMSE and R² to find the best
#           performing model — equivalent to Orange's alpha search.
#
#  Note: Brookton has already been removed from the dataset file.
# ============================================================


# ── 0. Load packages ──────────────────────────────────────────────
# We need:
#   readxl  → read Excel files
#   glmnet  → Lasso regression
#   dplyr   → data manipulation
#   ggplot2 → plotting
#   tidyr   → reshaping data for plots

packages <- c("readxl", "glmnet", "dplyr", "ggplot2", "tidyr", "gt", "scales")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(readxl)
library(glmnet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gt)
library(scales)


# ── 1. Load data ──────────────────────────────────────────────────
# Brookton has already been removed from this file.
# We load the dataset directly — no station filtering needed.

DATA_PATH <- "C:/Users/Usuario/mi-proyecto-ml/Dataset6000/DatasetYieldwithOutliersV2.1.xlsx"

df <- read_excel(DATA_PATH, sheet = "AllYears_2005-2025")

cat("── Data loaded ──────────────────────────────────────────\n")
cat("  Rows :", nrow(df), "\n")
cat("  Cols :", ncol(df), "\n\n")


# ── 2. Drop excluded columns ──────────────────────────────────────
# We remove columns that are not useful for prediction:
#   - Administrative/identifier columns (FY, MyCode, SA2, etc.)
#   - Columns that would cause data leakage (Wheat produced, Area sown)
#   - SoilTemp: has ~70 missing values across all months
#   - Radiation: removed as it did not improve the model
#   - station_name / station_code: not predictive features

cols_to_remove <- c(
  "% TargArea", "FY", "MyCode", "% TargProdPB",
  "ReceiptPerTon2", "ProdReal_FY20-21", "AreaRealHa",
  "Business", "ReceiptPerTon", "SA2", "AreaPerBusiness",
  "ABARES_region", "Wheat receipts ($)", "YEAR",
  "ProdPerBusiness", "Wheat sold (t)", "station_code",
  "Wheat area sown (ha)", "Wheat produced (t)",
  "Cluster", "station_name",
  paste0("SoilTemp_month_",  1:12),   # 12 cols — too many NAs
  paste0("Radiation_month_", 1:12)    # 12 cols — removed after testing
)

df <- df |> select(-any_of(cols_to_remove))

cat("── Column selection ─────────────────────────────────────\n")
cat("  Features + target remaining:", ncol(df), "\n\n")


# ── 3. Handle missing values ──────────────────────────────────────
# After removing SoilTemp there should be no NAs left.
# na.omit() is kept as a safety check — it will not remove rows
# if the data is clean.

na_counts <- colSums(is.na(df))
if (any(na_counts > 0)) {
  cat("── Columns with NAs ─────────────────────────────────────\n")
  print(na_counts[na_counts > 0])
  cat("\n")
}

df <- df |> na.omit()
cat("── After NA removal ─────────────────────────────────────\n")
cat("  Complete rows for modelling:", nrow(df), "\n\n")


# ── 4. Prepare feature matrix and target vector ───────────────────
# X = all columns except Yield (the features / predictors)
# y = Yield (what we want to predict)

X <- df |> select(-Yield) |> as.matrix()
y <- df$Yield

cat("── Model input ──────────────────────────────────────────\n")
cat("  Features (p)     :", ncol(X), "\n")
cat("  Observations (n) :", nrow(X), "\n")
cat("  Target           : Yield\n\n")


# ── 5. Train / Test split — 80 / 20 ──────────────────────────────
# We fix the seed so results are reproducible every time we run
# the script. 80% of the data goes to training, 20% to testing.
# The test set is never seen by the model during training.

set.seed(42)
n         <- nrow(X)
train_idx <- sample(seq_len(n), size = floor(0.8 * n), replace = FALSE)
test_idx  <- setdiff(seq_len(n), train_idx)

X_train <- X[train_idx, ];  y_train <- y[train_idx]
X_test  <- X[test_idx,  ];  y_test  <- y[test_idx]

cat("── Train / Test split ───────────────────────────────────\n")
cat("  Training samples :", length(train_idx), "\n")
cat("  Test samples     :", length(test_idx),  "\n\n")


# ── 6. Z-score normalisation ──────────────────────────────────────
# We scale features to mean = 0 and SD = 1 (same as Orange default).
# IMPORTANT: We fit the scaler on TRAIN only, then apply to TEST.
# This prevents data leakage — the test set must be treated as
# if it were completely unseen data.

train_means <- colMeans(X_train, na.rm = TRUE)

# scale = FALSE → subtract mean only, do NOT divide by SD
# This matches Orange's "Center to μ=0" preprocessing option
X_train_sc  <- scale(X_train, center = train_means, scale = FALSE)
X_test_sc   <- scale(X_test,  center = train_means, scale = FALSE)

cat("── Normalisation ────────────────────────────────────────\n")
cat("  Method: Center to μ=0 (subtract mean only — mirrors Orange)\n\n")


# ── 7. Define lambda values to test ───────────────────────────────
# Lambda controls regularisation strength in Lasso:
#   - Small lambda → less penalty → more features retained → risk of overfitting
#   - Large lambda → strong penalty → fewer features → risk of underfitting
# We test a range from very small (0.001) to large (0.5) to find
# the best balance between Train and Test performance.

lambdas <- c(0.001, 0.005, 0.01, 0.02, 0.03, 0.05, 0.07, 0.1, 0.15, 0.2, 0.3, 0.5)

cat("── Lambda values to test ────────────────────────────────\n")
cat(" ", paste(lambdas, collapse = ", "), "\n\n")


# ── 8. Helper function: compute RMSE and R² ───────────────────────
# RMSE (Root Mean Squared Error): measures average prediction error
#   in the same units as Yield — lower is better.
# R²: proportion of variance explained by the model — higher is better.
#   R² = 1 means perfect prediction, R² = 0 means no better than the mean.

eval_metrics <- function(actual, predicted) {
  rmse   <- sqrt(mean((actual - predicted)^2))
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2     <- 1 - ss_res / ss_tot
  list(rmse = rmse, r2 = r2)
}


# ── 9. Run Lasso for each lambda and collect results ──────────────
# For each lambda value we:
#   1. Fit Lasso on the scaled training set
#   2. Predict on both train and test sets
#   3. Compute RMSE and R² for both
#   4. Count how many features have non-zero coefficients
# This gives us a complete picture of how each lambda performs.

cat("── Lambda comparison ─────────────────────────────────────\n")
cat(sprintf("  %-8s  %-10s  %-10s  %-10s  %-10s  %-10s\n",
            "Lambda", "Train RMSE", "Train R²", "Test RMSE", "Test R²", "Non-zero"))
cat(sprintf("  %-8s  %-10s  %-10s  %-10s  %-10s  %-10s\n",
            "────────", "──────────", "────────", "─────────", "───────", "────────"))

results <- data.frame(
  Lambda     = numeric(),
  Train_RMSE = numeric(),
  Train_R2   = numeric(),
  Test_RMSE  = numeric(),
  Test_R2    = numeric(),
  NonZero    = integer()
)

for (lam in lambdas) {

  # Fit Lasso — alpha = 1 means pure L1 (Lasso), not Ridge
  fit <- glmnet(
    x           = X_train_sc,
    y           = y_train,
    alpha       = 1,           # L1 = Lasso
    lambda      = lam,
    intercept   = TRUE,
    standardize = FALSE        # already normalised in step 6
  )

  # Predictions on train and test
  pred_train <- as.numeric(predict(fit, newx = X_train_sc, s = lam))
  pred_test  <- as.numeric(predict(fit, newx = X_test_sc,  s = lam))

  # Metrics
  m_train  <- eval_metrics(y_train, pred_train)
  m_test   <- eval_metrics(y_test,  pred_test)

  # Number of non-zero coefficients (features selected by Lasso)
  coef_vals <- as.numeric(coef(fit, s = lam))
  n_nonzero <- sum(coef_vals[-1] != 0)

  # Flag the best test R² and the Orange-equivalent (lambda = 0.1)
  flag <- ""
  if (lam == 0.1)              flag <- " ← current model"
  if (m_test$r2 == max(sapply(lambdas, function(l) {
    f <- glmnet(X_train_sc, y_train, alpha = 1, lambda = l,
                intercept = TRUE, standardize = FALSE)
    p <- as.numeric(predict(f, newx = X_test_sc, s = l))
    1 - sum((y_test - p)^2) / sum((y_test - mean(y_test))^2)
  })))                         flag <- " ← best test R²"

  cat(sprintf("  %-8.3f  %-10.4f  %-10.4f  %-10.4f  %-10.4f  %-10d%s\n",
              lam,
              m_train$rmse, m_train$r2,
              m_test$rmse,  m_test$r2,
              n_nonzero, flag))

  results <- rbind(results, data.frame(
    Lambda     = lam,
    Train_RMSE = m_train$rmse,
    Train_R2   = m_train$r2,
    Test_RMSE  = m_test$rmse,
    Test_R2    = m_test$r2,
    NonZero    = n_nonzero
  ))
}

cat("\n")


# ── 10. Identify best lambda ──────────────────────────────────────
# The best lambda is the one with the highest Test R².
# We also check for overfitting: a large gap between Train R²
# and Test R² means the model memorised the training data.

best_row    <- results[which.max(results$Test_R2), ]
best_lambda <- best_row$Lambda

cat("── Best lambda by Test R² ───────────────────────────────\n")
cat(sprintf("  Lambda   : %.3f\n",  best_lambda))
cat(sprintf("  Train R² : %.4f\n",  best_row$Train_R2))
cat(sprintf("  Test  R² : %.4f\n",  best_row$Test_R2))
cat(sprintf("  Test RMSE: %.4f\n",  best_row$Test_RMSE))
cat(sprintf("  Non-zero : %d features\n\n", best_row$NonZero))

# Overfitting check
gap <- best_row$Train_R2 - best_row$Test_R2
cat("── Overfitting check ────────────────────────────────────\n")
cat(sprintf("  Train R² - Test R² gap : %.4f\n", gap))
if (gap > 0.15) {
  cat("  ⚠ Large gap — model may be overfitting the training set\n\n")
} else {
  cat("  ✅ Gap is acceptable — model generalises well\n\n")
}


# ── 11. Plot A — R² comparison (Train vs Test) ────────────────────
# This plot shows how Train R² and Test R² change as lambda increases.
# Ideally we want both lines to be high and close together.
# A large gap = overfitting. Converging lines = good generalisation.

results_long_r2 <- results |>
  select(Lambda, Train_R2, Test_R2) |>
  pivot_longer(cols = c(Train_R2, Test_R2),
               names_to  = "Set",
               values_to = "R2") |>
  mutate(Set = recode(Set, "Train_R2" = "Train", "Test_R2" = "Test"))

p_r2 <- ggplot(results_long_r2, aes(x = Lambda, y = R2,
                                     colour = Set, linetype = Set)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = best_lambda, linetype = "dotted",
             colour = "#C8A84B", linewidth = 0.9) +
  geom_vline(xintercept = 0.1, linetype = "dashed",
             colour = "grey50", linewidth = 0.7) +
  scale_colour_manual(values = c("Train" = "#2C5F2D", "Test" = "#B85042")) +
  scale_x_log10(breaks = lambdas,
                labels = as.character(lambdas)) +
  labs(
    title    = "Lasso — R² vs Lambda (Train and Test sets)",
    subtitle = sprintf("Gold dotted = best Test R² (λ = %.3f) | Grey dashed = current model (λ = 0.1)",
                       best_lambda),
    x        = "Lambda (log scale)",
    y        = "R²",
    colour   = NULL,
    linetype = NULL,
    caption  = "Higher R² = better fit"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "top"
  )

print(p_r2)
ggsave("lambda_comparison_r2.png", plot = p_r2, width = 9, height = 6, dpi = 150)
cat("📊 Plot saved: lambda_comparison_r2.png\n")


# ── 12. Plot B — RMSE comparison (Train vs Test) ──────────────────
# Same idea but using RMSE. Lower RMSE = better.
# The test RMSE curve typically has a U-shape:
#   - Too small lambda: overfits → high test RMSE
#   - Too large lambda: underfits → high test RMSE
#   - Sweet spot in the middle: lowest test RMSE

results_long_rmse <- results |>
  select(Lambda, Train_RMSE, Test_RMSE) |>
  pivot_longer(cols = c(Train_RMSE, Test_RMSE),
               names_to  = "Set",
               values_to = "RMSE") |>
  mutate(Set = recode(Set, "Train_RMSE" = "Train", "Test_RMSE" = "Test"))

p_rmse <- ggplot(results_long_rmse, aes(x = Lambda, y = RMSE,
                                         colour = Set, linetype = Set)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = best_lambda, linetype = "dotted",
             colour = "#C8A84B", linewidth = 0.9) +
  geom_vline(xintercept = 0.1, linetype = "dashed",
             colour = "grey50", linewidth = 0.7) +
  scale_colour_manual(values = c("Train" = "#2C5F2D", "Test" = "#B85042")) +
  scale_x_log10(breaks = lambdas,
                labels = as.character(lambdas)) +
  labs(
    title    = "Lasso — RMSE vs Lambda (Train and Test sets)",
    subtitle = sprintf("Gold dotted = best Test R² (λ = %.3f) | Grey dashed = current model (λ = 0.1)",
                       best_lambda),
    x        = "Lambda (log scale)",
    y        = "RMSE",
    colour   = NULL,
    linetype = NULL,
    caption  = "Lower RMSE = better fit"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "top"
  )

print(p_rmse)
ggsave("lambda_comparison_rmse.png", plot = p_rmse, width = 9, height = 6, dpi = 150)
cat("📊 Plot saved: lambda_comparison_rmse.png\n")


# ── 13. Plot C — Non-zero features vs Lambda ──────────────────────
# This shows how many features Lasso retains at each lambda.
# As lambda increases, Lasso forces more coefficients to zero.
# This is the key advantage of Lasso over Ridge regression:
# it performs automatic feature selection.

p_nz <- ggplot(results, aes(x = Lambda, y = NonZero)) +
  geom_line(colour = "#2C5F2D", linewidth = 1.1) +
  geom_point(colour = "#2C5F2D", size = 2.5) +
  geom_vline(xintercept = best_lambda, linetype = "dotted",
             colour = "#C8A84B", linewidth = 0.9) +
  geom_vline(xintercept = 0.1, linetype = "dashed",
             colour = "grey50", linewidth = 0.7) +
  scale_x_log10(breaks = lambdas,
                labels = as.character(lambdas)) +
  labs(
    title    = "Lasso — Features retained vs Lambda",
    subtitle = "As lambda increases, Lasso eliminates more features",
    x        = "Lambda (log scale)",
    y        = "Number of non-zero coefficients",
    caption  = "Gold dotted = best Test R² | Grey dashed = current model (λ = 0.1)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

print(p_nz)
ggsave("lambda_features_retained.png", plot = p_nz, width = 9, height = 5, dpi = 150)
cat("📊 Plot saved: lambda_features_retained.png\n\n")


# ── 14. Final summary table — formatted with gt ───────────────────
# gt renders a clean HTML table in the Positron / RStudio viewer.
# Colour gradients highlight best and worst values automatically.

library(gt)
library(scales)

gt_table <- results |>
  mutate(
    Label = case_when(
      Lambda == best_lambda & Lambda == 0.1 ~ "★ best  |  current",
      Lambda == best_lambda                 ~ "★ best Test R²",
      Lambda == 0.1                         ~ "current model",
      TRUE                                  ~ ""
    )
  ) |>
  gt() |>
  tab_header(
    title    = md("**Lasso Regression — Lambda Comparison**"),
    subtitle = md("*Train / Test RMSE and R² across regularisation strengths*")
  ) |>
  cols_label(
    Lambda     = "Lambda (λ)",
    Train_RMSE = "Train RMSE",
    Train_R2   = "Train R²",
    Test_RMSE  = "Test RMSE",
    Test_R2    = "Test R²",
    NonZero    = "Non-zero features",
    Label      = ""
  ) |>
  fmt_number(columns = c(Train_RMSE, Train_R2, Test_RMSE, Test_R2), decimals = 4) |>
  fmt_number(columns = Lambda, decimals = 3) |>
  # Colour gradient: green = good R², red = poor R²
  data_color(
    columns = c(Train_R2, Test_R2),
    palette = c("#B85042", "#FFFFFF", "#2C5F2D"),
    domain  = c(0, 1)
  ) |>
  # Colour gradient: green = low RMSE, red = high RMSE
  data_color(
    columns = c(Train_RMSE, Test_RMSE),
    palette = c("#2C5F2D", "#FFFFFF", "#B85042")
  ) |>
  # Highlight current model row
  tab_style(
    style     = list(cell_fill(color = "#FFF8E1"),
                     cell_text(weight = "bold")),
    locations = cells_body(rows = Lambda == 0.1)
  ) |>
  # Highlight best lambda row
  tab_style(
    style     = list(cell_fill(color = "#E8F5E9"),
                     cell_text(weight = "bold")),
    locations = cells_body(rows = Lambda == best_lambda & Lambda != 0.1)
  ) |>
  tab_footnote(
    footnote  = "Yellow highlight = current model (λ = 0.1) | Green highlight = best Test R²",
    locations = cells_title(groups = "subtitle")
  ) |>
  tab_options(
    table.font.size       = 13,
    heading.align         = "left",
    column_labels.font.weight = "bold"
  )

# Print in viewer
print(gt_table)

# Also save as HTML file for reference
gtsave(gt_table, "lambda_comparison_table.html")

cat("════════════════════════════════════════════════════════\n")
cat("  TABLE saved : lambda_comparison_table.html\n")
cat("  PLOTS saved :\n")
cat("    lambda_comparison_r2.png\n")
cat("    lambda_comparison_rmse.png\n")
cat("    lambda_features_retained.png\n")
cat("════════════════════════════════════════════════════════\n")
