# Wheat Production Forecasting – Dataset 2005-2024 v2

This model forecasts average wheat production per business in WA farming areas using climatic and geographical variables as predictors.

---

## Target Variable

**ProdPerBusiness** — Average wheat production per business (tonnes).

---

## Data

| File | Description |
|---|---|
| `Dataset_2005-24v2.xlsx` | Full dataset 2005–2024 (corrected version) |

- **298 rows** × **99 columns** after cleaning
- **19 weather stations** across WA farming regions
- **20 years** of annual observations (2005–2024)
- 1 missing value in `Radiation_month_2` imputed with column mean
- `SoilTemp` variables excluded due to ~30% missing values
- Outliers retained (Option A) — tree-based models are robust to outliers

---

## Features

| Category | Variables | Count |
|---|---|---|
| Geographic | LATITUDE, LONGITUDE | 2 |
| Air Temperature | AirTempAvg months 1–12 | 12 |
| Rainfall | Rainfall months 1–12 | 12 |
| Radiation | Radiation months 1–12 | 12 |
| Wind | WindAvg months 1–12 | 12 |
| Evaporation | Evaporation months 1–12 | 12 |
| Relative Humidity | RelHumAvg months 1–12 | 12 |
| Rainfall lag | Rainfall_growing_season_lag1, Rainfall_annual_lag1 | 2 |
| Economic | Nitrogen_price, Wheat_PriceAvgAprDec | 2 |

> SoilTemp excluded due to ~30% missing values.
> Economic and lag variables showed no significant predictive power across models.

---

## Data Split

| Set | Years | Rows | % |
|---|---|---|---|
| Train | 2005 – 2021 | 241 | 80.9% |
| Test | 2022 – 2024 | 57 | 19.1% |

> Temporal split used to reflect real forecasting conditions.

---

## Correlation Analysis

**Top positive correlations with ProdPerBusiness (train set):**
- Evaporation_month_8 (0.61), Evaporation_month_9 (0.58)
- AirTempAvg_month_9 (0.56), LATITUDE (0.54)
- AirTempAvg months 3, 5, 8, 10 (0.50–0.53)

**Top negative correlations:**
- RelHumAvg_month_9 (-0.56), RelHumAvg_month_8 (-0.53)
- RelHumAvg months 5, 6, 10 (-0.40 to -0.45)

**Not relevant:**
- Nitrogen_price (0.04), Wheat_PriceAvgAprDec (0.08)
- Rainfall lags (-0.07 to -0.12)

---

## Scripts

| Script | Description |
|---|---|
| `01_load_data.R` | Loads dataset |
| `02_clean_data.R` | Removes SoilTemp, detects outliers, imputes missing, splits 80/20 |
| `03_correlation.R` | Correlation of all features with target |
| `04_linear_reg.R` | Linear Regression — all features (normalised) |
| `04b_linear_reg_significant.R` | Linear Regression — p<0.05 features (15) |
| `04c_linear_reg_robust.R` | Linear Regression — robust features (7) |
| `05_lasso.R` | LASSO — baseline + feature importance + top 15 |
| `06_decision_tree.R` | Decision Tree — baseline + feature importance + tuning |
| `06b_decision_tree_tuned.R` | Decision Tree — best parameters (cp=0.005, maxdepth=4) |
| `06c_dt_feature_combinations.R` | Decision Tree — 14 feature group combinations |
| `06d_dt_tune_geo_airtemp.R` | Decision Tree — tuning grid for Geo+AirTemp |
| `07_random_forest.R` | Random Forest — RF selects own features via importance |

---

## Results Summary

| Model | AIC | Train R² | Test RMSE | Test R² |
|---|---|---|---|---|
| Linear Regression (all) | 3,981 | 0.776 | 2,368 | 0.114 |
| LM Significant (15 feat) | 4,010 | 0.575 | 2,168 | 0.222 |
| LM Robust (7 feat) | 4,008 | 0.549 | 2,151 | 0.219 |
| LASSO Baseline | 3,322 | 0.650 | 1,681 | 0.423 |
| LASSO Top 15 | 3,318 | 0.578 | 1,774 | 0.406 |
| DT Baseline | 3,162 | 0.775 | 1,845 | 0.530 |
| DT Tuned (cp=0.005, md=4) | 3,226 | 0.700 | 1,876 | 0.594 |
| **DT Geo+AirTemp** ⭐ | 3,223 | 0.698 | 1,504 | **0.858** |
| RF Geo+AirTemp baseline | 2,916 | 0.936 | 1,403 | 0.758 |
| RF Top 5 (RF-selected) | 2,936 | 0.917 | 1,538 | 0.799 |

---

## Best Model

**Decision Tree — Geo + AirTemp (cp=0.005, maxdepth=4)**

| Parameter | Value |
|---|---|
| Features | LATITUDE, LONGITUDE, AirTempAvg months 1–12 |
| N features | 14 |
| cp | 0.005 |
| maxdepth | 4 |
| N leaves | 7 |
| AIC | 3,223 |
| Train R² | 0.698 |
| Test RMSE | 1,504 |
| **Test R²** | **0.858** |
| Overfit gap | -0.160 (test > train — stable) |

---

## Key Findings

1. **LATITUDE is the strongest and most consistent predictor** across all models
2. **Air temperature** (all months) is the most informative climate variable group
3. **Geo + AirTemp alone** (14 features) outperforms all models with more features
4. **Adding more variables hurts performance** — Evaporation, RelHum, Rainfall all reduced Test R² when added to Geo+AirTemp
5. **Economic variables** (Nitrogen price, Wheat price) are not significant in any model
6. **Rainfall lag variables** show no significant effect — slightly negative correlation
7. **Linear Regression overfits** severely with many features (Train R²=0.78 vs Test R²=0.11)
8. **LASSO** is the best linear model — reduces overfitting by dropping 41 features automatically
9. **Decision Tree** generalises best — negative overfit gap suggests test years follow clear temperature patterns
10. **Random Forest** selects: LATITUDE, RelHumAvg_month_9, LONGITUDE, Evaporation_month_9, Evaporation_month_7 as top 5

---

## Limitations

- Small dataset (n=298 rows, 20 annual observations across 19 stations)
- Temporal split means test set covers only 3 years (2022–2024)
- Negative overfit gap suggests 2022–2024 may be easier to predict than average years
- Economic variables (prices) not significant — may require lagged values or different price indices

---

## Requirements

```r
install.packages(c(
  "readxl", "dplyr", "ggplot2", "corrplot",
  "glmnet", "rpart", "rpart.plot",
  "randomForest", "Metrics"
))
```

---

## How to Run

```r
setwd("C:/Users/Usuario/mi-proyecto-ml/Dataset2005-24v2")

source("01_load_data.R")
source("02_clean_data.R")
source("03_correlation.R")
source("04_linear_reg.R")
source("05_lasso.R")
source("06_decision_tree.R")
source("07_random_forest.R")
```

---

## Author

**Julieta Falco**
[github.com/JulietaFalco](https://github.com/JulietaFalco)
