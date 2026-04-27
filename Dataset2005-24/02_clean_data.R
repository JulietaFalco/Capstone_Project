# ============================================================
# 02_CLEAN_DATA.R
# ============================================================

library(dplyr)

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
  "Nitrogen_Price",
  "Wheat_Price"
)

target <- "ProdPerBusiness"

# Select only relevant columns
df_model <- df_raw %>%
  select(YEAR, all_of(features), all_of(target)) %>%
  filter(!is.na(ProdPerBusiness))

# Impute the 1 missing value in Radiation_month_2 with column mean
df_model$Radiation_month_2[is.na(df_model$Radiation_month_2)] <- mean(df_model$Radiation_month_2, na.rm = TRUE)

cat("After cleaning:", nrow(df_model), "rows\n")
cat("NAs remaining:", sum(is.na(df_model)), "\n")

# Split 80/20 temporal
split_year <- 2022

train <- df_model %>% filter(YEAR < split_year)  %>% select(-YEAR)
test  <- df_model %>% filter(YEAR >= split_year) %>% select(-YEAR)

X_train <- train %>% select(all_of(features))
y_train <- train[[target]]
X_test  <- test %>% select(all_of(features))
y_test  <- test[[target]]

cat("\nTrain:", nrow(train), "rows | Years: 2005-2021 |", round(nrow(train)/(nrow(train)+nrow(test))*100,1), "%\n")
cat("Test: ", nrow(test),  "rows | Years: 2022-2024 |", round(nrow(test)/(nrow(train)+nrow(test))*100,1), "%\n")
