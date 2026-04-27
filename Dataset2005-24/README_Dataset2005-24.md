# Wheat Production Forecasting – Dataset 2005-2024

This model forecasts average wheat production per business in WA farming areas using climatic, geographical, and economic variables as predictors.

---

## Target Variable

**ProdPerBusiness** — Average wheat production per business (tonnes).

---

## Data

| File | Description |
|---|---|
| `Dataset_2005-24.xlsx` | Full dataset 2005–2024 |

- **298 rows** × **99 columns** after cleaning
- **19 weather stations** across WA farming regions
- **20 years** of annual observations (2005–2024)
- 1 missing value in `Radiation_month_2` imputed with column mean
- `SoilTemp` variables excluded due to ~30% missing values

---

## Features Used

| Category | Variables | Count |
|---|---|---|
| Geographic | LATITUDE, LONGITUDE | 2 |
| Climate | AirTempAvg, Rainfall, Radiation, WindAvg, Evaporation, RelHumAvg — one per month (months 1–12) | 72 |
| Rainfall lag | Rainfall_growing_season_lag1, Rainfall_annual_lag1 | 2 |
| Economic | Nitrogen_Price ($/mt), Wheat_Price ($/mt) | 2 |

> Rainfall lag variables represent the previous year's rainfall (YEAR - 1), used as a proxy for the farmer's fertiliser purchase decision.

> SoilTemp excluded due to ~30% missing values across all 12 months.

---

## Data Split

| Set | Years | Rows | % |
|---|---|---|---|
| Train | 2005 – 2021 | 241 | 80.9% |
| Test | 2022 – 2024 | 57 | 19.1% |

> Temporal split used instead of random split to reflect real forecasting conditions.

---

## Scripts

| Script | Description |
|---|---|
| `main.R` | Runs the full pipeline |
| `01_load_data.R` | Loads dataset |
| `02_clean_data.R` | Cleans data, imputes missing values, splits 80/20 |
| `03_correlation.R` | Correlation of all features with target |
| `04_linear_reg.R` | Linear Regression with Z-score normalisation |
| `05_lasso.R` | LASSO regression with cross-validated lambda |
| `06_decision_tree.R` | Decision Tree — base model + cp/maxdepth tuning |
| `06b_decision_tree_top_features.R` | Decision Tree with top 15 and top 30 features |
| `06c_decision_tree_tuning.R` | Decision Tree tuning grid (merged into 06) |
| `06d_decision_tree_combinations.R` | Feature group combination testing |
| `06e_dt_tuning_geo_airtemp_wind.R` | Tuning for best feature combination |
| `06f_dt_cross_validation.R` | 5-fold cross validation for Decision Tree |
| `07_random_forest.R` | Random Forest — all features |
| `07b_random_forest_top_features.R` | Random Forest — top 15 features |
| `07c_random_forest_no_lag.R` | Random Forest — top 15 without lag |
| `07d_random_forest_tuning.R` | Random Forest hyperparameter tuning |
| `08_feature_importance_comparison.R` | Feature importance across all models |
| `09_final_model.R` | All models with blue (consistent) features only |

---

## Correlation Analysis

Top positive correlations with ProdPerBusiness:
- AirTempAvg months 8, 9, 10 (r ≈ 0.32–0.34)
- Evaporation months 8, 9 (r ≈ 0.30–0.32)
- WindAvg months 9, 10, 11 (r ≈ 0.27–0.31)

Top negative correlations:
- RelHumAvg months 8, 9 (r ≈ -0.35)
- LONGITUDE (r ≈ -0.19)

Notable: Rainfall variables show weak or near-zero correlation with target.

---

## Feature Importance — Consistent Across Models

Features selected by ALL 4 models (Linear Regression, LASSO, Decision Tree, Random Forest):

| Feature | LM rank | LASSO rank | DT rank | RF rank |
|---|---|---|---|---|
| LATITUDE | 1 | 1 | 2 | 1 |
| AirTempAvg_month_3 | 4 | 2 | 6 | 8 |
| AirTempAvg_month_8 | 8 | 4 | 23 | 16 |
| AirTempAvg_month_5 | 10 | 6 | 16 | 19 |
| AirTempAvg_month_1 | 6 | 3 | 18 | 22 |
| WindAvg_month_9 | 7 | 5 | 13 | 27 |
| AirTempAvg_month_4 | 9 | 8 | 3 | 29 |

> Best performing feature combination: **Geo + AirTemp + Wind** (26 features) — adding rainfall, economic or lag variables did not improve DT performance.

---

## Results Summary

### All Models — Full Feature Set

| Model | AIC | Train R² | Test RMSE | Test R² |
|---|---|---|---|---|
| Linear Regression | 4,190 | 0.698 | 2,912 | 0.413 |
| LASSO | 3,479 | 0.679 | 1,770 | 0.581 |
| Decision Tree (base) | 3,465 | 0.537 | 1,916 | 0.704 |
| Decision Tree (top 30) | 3,464 | 0.555 | 1,887 | 0.736 |
| Random Forest (all features) | 3,121 | 0.955 | 2,040 | 0.553 |
| Random Forest (top 15 with lag) | 3,092 | 0.944 | 1,925 | 0.693 |

### Best Feature Combination — Geo + AirTemp + Wind (26 features)

| Model | cp | maxdepth | AIC | Train R² | Test RMSE | Test R² |
|---|---|---|---|---|---|---|
| DT Geo+AirTemp+Wind | 0.0001 | 4 | 3,477 | 0.519 | 1,944 | **0.747** |
| DT Geo+AirTemp+Wind | 0.001 | 5 | 3,469 | 0.544 | 1,850 | 0.730 |
| DT Geo+AirTemp+Wind | 0.0001 | 7 | 3,467 | 0.567 | 1,827 | 0.717 |

### Cross Validation — 5-fold (Geo + AirTemp + Wind)

| cp | maxdepth | CV RMSE | CV R² | SD RMSE |
|---|---|---|---|---|
| 0.0001 | 7 | 1,844 | 0.298 | 406 |
| 0.01 | 6 | 1,839 | 0.284 | 405 |

> ⚠️ CV R² (≈0.30) is substantially lower than test R² (≈0.73), suggesting the test years 2022–2024 were relatively easy to predict. True model generalisation is estimated at R² ≈ 0.30, consistent with the limited dataset size (298 rows, 20 annual observations).

---

## Key Findings

1. **LATITUDE is the strongest and most consistent predictor** across all models
2. **Temperature variables** (AirTempAvg) are the most informative climate features
3. **Rainfall shows weak correlation** with production — consistent with Steve's insight that farmer decisions are based on expected rainfall (lag), not actual rainfall
4. **Rainfall lag variables** are important for Random Forest but not for Decision Tree or LASSO
5. **Fertiliser (Nitrogen) price** has a small positive effect — higher prices correlate with higher production years (likely reverse causality: low supply years drive both high prices and high demand)
6. **Best model: Decision Tree** with Geo+AirTemp+Wind features (Test R²=0.747)
7. **Cross validation confirms** limited generalisation power due to small dataset size

---

## Limitations

- Small dataset (n=298 rows, 20 annual observations across 19 stations)
- Temporal split means test set covers only 3 years (2022–2024)
- CV R² ≈ 0.30 suggests model generalisation is limited
- No data available prior to 2005

---

## Requirements

```r
install.packages(c(
  "readxl", "dplyr", "ggplot2", "corrplot",
  "glmnet", "rpart", "rpart.plot",
  "randomForest", "caret", "Metrics"
))
```

---

## How to Run

```r
setwd("C:/Users/Usuario/mi-proyecto-ml/Dataset2005-24")
source("main.R")
```

Or individually in order:
```r
source("01_load_data.R")
source("02_clean_data.R")
source("03_correlation.R")
source("04_linear_reg.R")
source("05_lasso.R")
source("06_decision_tree.R")
source("07_random_forest.R")
source("08_feature_importance_comparison.R")
```

---

## Author

**Julieta Falco**
[github.com/JulietaFalco](https://github.com/JulietaFalco)
