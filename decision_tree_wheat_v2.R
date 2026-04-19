# =============================================================================
# WHEAT PRODUCTION - DECISION TREE MODEL (Version 2)
# =============================================================================
# This script builds a Decision Tree to predict wheat production (tonnes)
# based on monthly climate data (rainfall, temperature, radiation, etc.)
#
# Train set: 2013-2022 (model learns from historical data)
# Test set:  2023-2026 (model is evaluated on recent/future data)
# =============================================================================


# =============================================================================
# STEP 1: INSTALL AND LOAD LIBRARIES
# =============================================================================
# Libraries are packages that add extra functionality to R.
# You only need to install them ONCE. After that, just load them with library().

# Run this block only the first time:
install.packages("tidymodels")   # Main ML framework (train, test, evaluate)
install.packages("readxl")       # To read Excel files
install.packages("rpart.plot")   # To visualize the decision tree
install.packages("vip")          # To see which variables matter most
install.packages("dplyr")        # To manipulate and clean data

# Load libraries every time you run the script:
library(tidymodels)
library(readxl)
library(rpart.plot)
library(vip)
library(dplyr)


# =============================================================================
# STEP 2: DEFINE THE CLEANING FUNCTION
# =============================================================================
# Instead of cleaning the data manually each time, we create a function that:
#   1. Removes columns the model should NOT learn from
#   2. Removes rows where the target (wheat production) is missing
#
# By writing it as a function, we apply the EXACT same cleaning to both
# the train and test files — avoiding any inconsistencies.

clean_dataset <- function(df) {
  df %>%
    select(
      -station_code, -station_name, -LONGITUDE, -LATITUDE,
      -MyCode, -ABARES_region,       # SA2 is no longer removed
      -starts_with("SoilTemp"),
      -`Wheat area sown (ha)`,
      -`Wheat receipts ($)`,
      -`Wheat sold (t)`
    ) %>%
    filter(!is.na(`Wheat produced (t)`)) %>%
    mutate(SA2 = as.factor(SA2))     # ← Convert SA2 to factor (categorical)
}


# =============================================================================
# STEP 3: LOAD AND CLEAN TRAIN AND TEST DATA
# =============================================================================
# We use a TIME-BASED split instead of a random split:
#   - TRAIN (2013-2022): the model LEARNS from historical climate + production data
#   - TEST  (2023-2026): we CHECK if the model predicts recent years correctly
#
# This is more realistic because in real life you always predict FUTURE years
# based on patterns learned from PAST data.
#
# Remember to update the file paths if your files are in a different location!

train_data <- read_excel("C:/Users/Usuario/mi-proyecto-ml/Dataset_Train.xlsx") %>%
  clean_dataset()

test_data <- read_excel("C:/Users/Usuario/mi-proyecto-ml/Dataset_Test.xlsx") %>%
  clean_dataset()

# Check how many rows we have in each set:
cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows: ", nrow(test_data),  "\n")

# Quick look at the data:
glimpse(train_data)


# =============================================================================
# STEP 4: DEFINE THE MODEL
# =============================================================================
# Here we define WHAT kind of model we want to build.
# We are using a Decision Tree for REGRESSION (predicting a number).
#
# Key parameters:
#   max_depth = 4  → maximum number of questions the tree can ask
#                    (higher = more complex, risk of overfitting)
#   min_n = 3      → minimum number of rows needed to make a split
#                    (lower = finer splits, risk of overfitting)
#
# Overfitting = the model memorizes training data instead of learning
# general patterns. A simpler tree generalizes better to new data.

dt_model <- decision_tree(
  tree_depth = 4,
  min_n     = 3
) %>%
  set_engine("rpart") %>%      # rpart is the R package that runs the tree
  set_mode("regression")       # regression = predicting a number


# =============================================================================
# STEP 5: TRAIN THE MODEL
# =============================================================================
# Now we FIT (train) the model using the training data.
# The formula `Wheat produced (t)` ~ . means:
#   "predict Wheat produced using ALL other columns as inputs"

dt_fit <- dt_model %>%
  fit(`Wheat produced (t)` ~ ., data = train_data)

# Print a summary of what the tree learned:
dt_fit


# =============================================================================
# STEP 6: VISUALIZE THE DECISION TREE
# =============================================================================
# This creates a visual diagram of the tree — the most intuitive part!
# Each node shows:
#   - The question being asked (e.g., "Rainfall_month_6 >= 80?")
#   - The predicted value at that leaf
#   - The % of training data that falls into that branch

rpart.plot(
  dt_fit$fit,
  type  = 4,       # Style of the tree diagram
  extra = 101,     # Shows number of observations and predicted values
  main  = "Decision Tree - Wheat Production (tonnes)",
  cex   = 0.7,      # Font size
  roundint  = FALSE
)
  


# =============================================================================
# STEP 7: MAKE PREDICTIONS ON THE TEST SET
# =============================================================================
# We ask the model to predict wheat production for the TEST rows
# (years 2023-2026 that the model has NEVER seen before).

predictions <- predict(dt_fit, new_data = test_data) %>%
  bind_cols(test_data %>% select(`Wheat produced (t)`, YEAR))

# See predictions side by side with the real values:
predictions %>%
  rename(
    Predicted = .pred,
    Actual    = `Wheat produced (t)`
  ) %>%
  mutate(Error = Actual - Predicted)   # Positive = underpredicted


# =============================================================================
# STEP 8: EVALUATE THE MODEL PERFORMANCE
# =============================================================================
# We use 3 metrics to understand how well the model predicts:
#
#   RMSE (Root Mean Square Error):
#     → Average prediction error in the same units as the target (tonnes)
#     → Lower is better. e.g., RMSE = 300 means predictions are off by ~300t
#
#   MAE (Mean Absolute Error):
#     → Similar to RMSE but less sensitive to large errors
#     → Easier to interpret: "on average, predictions are off by X tonnes"
#
#   R² (R-squared):
#     → How much of the variation in production the model explains
#     → Ranges from 0 to 1. Closer to 1 = better fit
#     → e.g., R² = 0.75 means the model explains 75% of the variation

metrics_result <- metrics(
  predictions,
  truth    = `Wheat produced (t)`,
  estimate = .pred
)

print(metrics_result)


# =============================================================================
# STEP 9: VARIABLE IMPORTANCE
# =============================================================================
# Which climate variables matter most for predicting wheat production?
# This plot ranks all input variables by their influence on the model.
# The longer the bar, the more important that variable is.

vip(dt_fit, num_features = 15) +
  labs(
    title = "Top 15 Most Important Variables",
    x     = "Importance",
    y     = "Variable"
  )


# =============================================================================
# STEP 10: SAVE YOUR RESULTS
# =============================================================================
# Save the predictions to a CSV file so you can review them later.

write.csv(
  predictions,
  "C:/Users/Usuario/mi-proyecto-ml/predictions_v2.csv",
  row.names = FALSE
)

cat("Done! Predictions saved to predictions_v2.csv\n")
