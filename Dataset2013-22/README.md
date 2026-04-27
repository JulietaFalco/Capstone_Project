# Wheat Production Forecasting – ML Models (Australia)

This project builds and evaluates machine learning models to forecast teh average wheat production per business in WA farming areas, using climatic, geographical and economic variables as predictors.

The **target variable** is: **Average Wheat Production per Business** (tonnes).

Models implemented: **Linear Regression**, **LASSO**, **Decision Tree**, and **Random Forest**.

---

## Project Structure

```
mi-proyecto-ml/
│
├── Dataset_Train_v1.xlsx       # Training dataset
├── Dataset_Test_v1.xlsx        # Test dataset
│
├── k-means files/              # K-means clustering exploration
│
├── 0_main.R                    # Master script – runs the full pipeline
├── 01_load_data.R              # Load and merge all data sources
├── 02_clean_data.R             # Data cleaning and preprocessing
├── 03_correlation.R            # Correlation analysis and variable exploration
├── 04_linear_reg.R             # Linear regression model
├── 05_lasso.R                  # LASSO regression (regularisation / feature selection)
├── 06_decision_tree.R          # Decision tree – base model
├── 06b_decision_tree.R         # Decision tree – variant B
├── 06c_decision_tree.R         # Decision tree – variant C
├── 07_random_forest.R          # Random forest model
└── ML_Model.R                  # Combined model script
```

---

## How to Run

### Option 1 – Run the full pipeline at once
Open and run `0_main.R`. This script calls all steps in order.

### Option 2 – Run scripts individually (in order)

| Step | Script | Description |
|------|--------|-------------|
| 1 | `01_load_data.R` | Loads training/test datasets and supplementary data |
| 2 | `02_clean_data.R` | Cleans, formats, and prepares data |
| 3 | `03_correlation.R` | Explores variable correlations |
| 4 | `04_linear_reg.R` | Fits a linear regression model |
| 5 | `05_lasso.R` | Fits a LASSO model for feature selection |
| 6 | `06_decision_tree.R` | Decision tree (base version) |
| 6b | `06b_decision_tree.R` | Decision tree variant B |
| 6c | `06c_decision_tree.R` | Decision tree variant C |
| 7 | `07_random_forest.R` | Random forest model |

> **Note:** Scripts 06, 06b, and 06c are separate iterations of decision tree tuning — these may be consolidated into a single script in a future version.

---

## Data

| File | Description |
|------|-------------|
| `Dataset_Train_v1.xlsx` | Training data with features and target variable |
| `Dataset_Test_v1.xlsx` | Held-out test data for model evaluation |

> The dataset contains approximately 8 data points (annual observations), so results should be interpreted with caution. Model complexity has been kept deliberately limited given the small sample size.

---

## Requirements

This project is written in **R**. Install the following packages before running:

```r
install.packages(c(
  "readxl",
  "dplyr",
  "ggplot2",
  "glmnet",      # LASSO
  "rpart",       # Decision Tree
  "rpart.plot",  # Decision Tree visualisation
  "randomForest",
  "caret",
  "corrplot"
))
```

---

## Target Variable

**Production per Business** (tonnes) — measures wheat production output relative to the number of businesses operating in the farming area.

Key predictors include:
- Climate indicators
- Lat & Lon
- Wheat pricing
- Fertilizer prices

---
---

## Author

**Julieta Falco**  
[github.com/JulietaFalco](https://github.com/JulietaFalco)

