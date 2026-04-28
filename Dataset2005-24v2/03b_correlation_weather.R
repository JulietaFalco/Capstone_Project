# ============================================================
# 03b_CORRELATION_WEATHER.R
# Correlation between weather variable GROUPS
# ============================================================

library(corrplot)
library(dplyr)
library(ggplot2)

# ── 1. CORRELATION BETWEEN GROUPS (monthly averages) ──────────
# Calculate monthly average for each variable group

airtemp_cols <- paste0("AirTempAvg_month_",  1:12)
evap_cols    <- paste0("Evaporation_month_", 1:12)
relhum_cols  <- paste0("RelHumAvg_month_",   1:12)
rain_cols    <- paste0("Rainfall_month_",    1:12)
rad_cols     <- paste0("Radiation_month_",   1:12)
wind_cols    <- paste0("WindAvg_month_",     1:12)

group_means <- data.frame(
  AirTemp_avg     = rowMeans(train[, airtemp_cols]),
  Evaporation_avg = rowMeans(train[, evap_cols]),
  RelHum_avg      = rowMeans(train[, relhum_cols]),
  Rainfall_avg    = rowMeans(train[, rain_cols]),
  Radiation_avg   = rowMeans(train[, rad_cols]),
  Wind_avg        = rowMeans(train[, wind_cols]),
  ProdPerBusiness = train$ProdPerBusiness
)

cat("=== CORRELATION BETWEEN WEATHER GROUPS ===\n")
cor_groups <- cor(group_means, use = "complete.obs")
print(round(cor_groups, 3))

corrplot(cor_groups,
         method      = "color",
         type        = "upper",
         tl.cex      = 0.8,
         tl.col      = "black",
         addCoef.col = "black",
         number.cex  = 0.8,
         title       = "Correlation Between Weather Variable Groups",
         mar         = c(0,0,2,0))

# ── 2. CORRELATION WITHIN AIRTEMP (all months) ────────────────
cat("\n=== CORRELATION WITHIN AIRTEMP MONTHS ===\n")
cor_airtemp <- cor(train[, airtemp_cols], use = "complete.obs")

corrplot(cor_airtemp,
         method      = "color",
         type        = "upper",
         tl.cex      = 0.7,
         tl.col      = "black",
         addCoef.col = "black",
         number.cex  = 0.6,
         title       = "Correlation Between AirTemp Months",
         mar         = c(0,0,2,0))

# ── 3. AIRTEMP vs EVAPORATION vs RELHUM (key variables) ───────
cat("\n=== KEY WEATHER VARIABLES CORRELATION ===\n")

key_weather <- train %>%
  select(
    AirTempAvg_month_3, AirTempAvg_month_5,
    AirTempAvg_month_8, AirTempAvg_month_9,
    Evaporation_month_7, Evaporation_month_8, Evaporation_month_9,
    RelHumAvg_month_8, RelHumAvg_month_9,
    Radiation_month_6, Radiation_month_8,
    Rainfall_month_7, Rainfall_month_8,
    WindAvg_month_9, WindAvg_month_10,
    ProdPerBusiness
  )

cor_key <- cor(key_weather, use = "complete.obs")

corrplot(cor_key,
         method      = "color",
         type        = "upper",
         tl.cex      = 0.65,
         tl.col      = "black",
         addCoef.col = "black",
         number.cex  = 0.55,
         title       = "Key Weather Variables — Correlation Matrix",
         mar         = c(0,0,2,0))

# ── 4. CORRELATION WITH TARGET BY MONTH ───────────────────────
cat("\n=== CORRELATION WITH TARGET BY MONTH ===\n")

months <- 1:12
month_cor <- data.frame(
  Month       = months,
  AirTemp     = sapply(months, function(m) cor(train[[paste0("AirTempAvg_month_", m)]], train$ProdPerBusiness)),
  Evaporation = sapply(months, function(m) cor(train[[paste0("Evaporation_month_", m)]], train$ProdPerBusiness)),
  RelHum      = sapply(months, function(m) cor(train[[paste0("RelHumAvg_month_", m)]], train$ProdPerBusiness)),
  Rainfall    = sapply(months, function(m) cor(train[[paste0("Rainfall_month_", m)]], train$ProdPerBusiness)),
  Radiation   = sapply(months, function(m) cor(train[[paste0("Radiation_month_", m)]], train$ProdPerBusiness)),
  Wind        = sapply(months, function(m) cor(train[[paste0("WindAvg_month_", m)]], train$ProdPerBusiness))
)

print(round(month_cor, 3))

# ── Plot correlation by month ─────────────────────────────────
month_cor_long <- tidyr::pivot_longer(month_cor,
                                      cols = -Month,
                                      names_to = "Variable",
                                      values_to = "Correlation")

ggplot(month_cor_long, aes(x = Month, y = Correlation,
                            colour = Variable, group = Variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title    = "Correlation with ProdPerBusiness by Month",
       subtitle = "Positive = higher value → more production",
       x        = "Month",
       y        = "Correlation with ProdPerBusiness",
       colour   = "Variable") +
  theme_minimal() +
  theme(legend.position = "right")
