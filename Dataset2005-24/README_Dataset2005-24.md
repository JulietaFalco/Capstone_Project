# Wheat Production Forecasting – Dataset 2005-2024

This model forecasts average wheat production per business in WA farming areas using climatic, geographical, and economic variables.

---

## Target Variable

**ProdPerBusiness** — Average wheat production per business (tonnes).

---

## Features Used

| Category | Variables |
|---|---|
| Geographic | LATITUDE, LONGITUDE |
| Climate | AirTempAvg, Rainfall, Radiation, WindAvg, SoilTemp, Evaporation, RelHumAvg — one feature per month (months 1–12) |
| Rainfall lag | Rainfall_growing_season_lag1, Rainfall_annual_lag1 |
| Economic | Nitron_$ (nitrogen price per tonne), Wheat_$ (wheat price) |

> **Note:** YEAR, station_code, station_name, SA2, MyCode, ABARES_region and AreaPerBusiness are excluded from the model.

> **Note:** Rainfall lag variables represent the previous year's rainfall (YEAR - 1), used as a proxy for the farmer's fertiliser purchase decision.

---

## Data Split

| Set | Years | Rows |
|---|---|---|
| Train | 2005 – 2017 | ~80% |
| Test | 2018 – 2024 | ~20% |

> Temporal split was used instead of random split to reflect real forecasting conditions.

---

## Models

| Script | Model | Feature Selection |
|---|---|---|
| `04_linear_reg.R` | Linear Regression | All features |
| `05_lasso.R` | LASSO Regression | Auto (regularisation) |
| `06_decision_tree.R` | Decision Tree | All features |
| `06b_decision_tree_top_features.R` | Decision Tree | Top features only |
| `07_random_forest.R` | Random Forest | All features + importance |

---

## How to Run

Run scripts in order, or use `main.R` to execute the full pipeline:

```r
source("main.R")
```

Or individually:

```r
source("01_load_data.R")
source("02_clean_data.R")
source("03_correlation.R")
source("04_linear_reg.R")
source("05_lasso.R")
source("06_decision_tree.R")
source("06b_decision_tree_top_features.R")
source("07_random_forest.R")
```

---

## Results Summary

*To be updated as models are run.*

| Model | RMSE | R² | MAE |
|---|---|---|---|
| Linear Regression | - | - | - |
| LASSO | - | - | - |
| Decision Tree (all features) | - | - | - |
| Decision Tree (top features) | - | - | - |
| Random Forest | - | - | - |

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

## Author

**Julieta Falco**
[github.com/JulietaFalco](https://github.com/JulietaFalco)
