# ============================================================
# 02_CLEAN_DATA.R
# - Remove SoilTemp columns
# - Detect and report outliers
# - Impute missing values
# - Split 80/20 temporal
# ============================================================

library(dplyr)
library(ggplot2)

# ── Define features ───────────────────────────────────────────
climate_vars <- c(
  paste0("AirTempAvg_month_",  1:12),
  paste0("Rainfall_month_",    1:12),
  paste0("Radiation_month_",   1:12),
  paste0("WindAvg_month_",     1:12),
  paste0("Evaporation_month_", 1:12),
  paste0("RelHumAvg_month_",   1:12)
)

features <- c(
  "LATITUDE", "LONGITUDE",
  climate_vars,
  "Rainfall_growing_season_lag1",
  "Rainfall_annual_lag1",
  "Nitrogen_price",
  "Wheat_PriceAvgAprDec"
)

target <- "ProdPerBusiness"

# ── Select columns (SoilTemp excluded) ───────────────────────
df_model <- df_raw %>%
  select(YEAR, all_of(features), all_of(target)) %>%
  filter(!is.na(ProdPerBusiness))

cat("After removing SoilTemp and selecting features:", nrow(df_model), "rows\n")

# ── Impute missing values ─────────────────────────────────────
na_counts <- colSums(is.na(df_model))
na_cols   <- na_counts[na_counts > 0]

if (length(na_cols) > 0) {
  cat("\nMissing values found:\n")
  print(na_cols)
  for (col in names(na_cols)) {
    df_model[[col]][is.na(df_model[[col]])] <- mean(df_model[[col]], na.rm = TRUE)
    cat("  Imputed:", col, "\n")
  }
} else {
  cat("No missing values found\n")
}

cat("NAs remaining:", sum(is.na(df_model)), "\n")

# ── Outlier detection ─────────────────────────────────────────
cat("\n=== OUTLIER DETECTION ===\n")
cat("Method: IQR (values beyond Q1 - 1.5*IQR or Q3 + 1.5*IQR)\n\n")

outlier_summary <- data.frame()

key_vars <- c(target, "LATITUDE", "LONGITUDE",
              "Rainfall_growing_season_lag1", "Rainfall_annual_lag1",
              "Nitrogen_price", "Wheat_PriceAvgAprDec",
              "AirTempAvg_month_1", "AirTempAvg_month_7",
              "Rainfall_month_7", "Rainfall_month_8",
              "WindAvg_month_10")

for (var in key_vars) {
  if (var %in% names(df_model)) {
    x    <- df_model[[var]]
    q1   <- quantile(x, 0.25, na.rm = TRUE)
    q3   <- quantile(x, 0.75, na.rm = TRUE)
    iqr  <- q3 - q1
    low  <- q1 - 1.5 * iqr
    high <- q3 + 1.5 * iqr
    n_out <- sum(x < low | x > high, na.rm = TRUE)

    outlier_summary <- rbind(outlier_summary, data.frame(
      Variable   = var,
      Min        = round(min(x, na.rm=TRUE), 2),
      Q1         = round(q1, 2),
      Median     = round(median(x, na.rm=TRUE), 2),
      Q3         = round(q3, 2),
      Max        = round(max(x, na.rm=TRUE), 2),
      N_outliers = n_out
    ))
  }
}

print(outlier_summary, row.names = FALSE)

# ── Boxplot of target variable ────────────────────────────────
ggplot(df_model, aes(y = ProdPerBusiness)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(title = "ProdPerBusiness — Outlier Check",
       y = "Production per Business (tonnes)") +
  theme_minimal()

# ── Identify extreme outlier rows in target ───────────────────
q1_t   <- quantile(df_model$ProdPerBusiness, 0.25)
q3_t   <- quantile(df_model$ProdPerBusiness, 0.75)
iqr_t  <- q3_t - q1_t
high_t <- q3_t + 1.5 * iqr_t

outlier_rows <- df_model %>%
  filter(ProdPerBusiness > high_t) %>%
  select(YEAR, ProdPerBusiness)

cat("\nOutlier rows in ProdPerBusiness (above", round(high_t, 1), "):\n")
print(outlier_rows)

# ── Split 80/20 temporal ──────────────────────────────────────
split_year <- 2022

train <- df_model %>% filter(YEAR < split_year)  %>% select(-YEAR)
test  <- df_model %>% filter(YEAR >= split_year) %>% select(-YEAR)

X_train <- train %>% select(all_of(features))
y_train <- train[[target]]
X_test  <- test %>% select(all_of(features))
y_test  <- test[[target]]

cat("\nTrain:", nrow(train), "rows | Years: 2005-2021 |",
    round(nrow(train)/(nrow(train)+nrow(test))*100,1), "%\n")
cat("Test: ", nrow(test),  "rows | Years: 2022-2024 |",
    round(nrow(test)/(nrow(train)+nrow(test))*100,1), "%\n")
