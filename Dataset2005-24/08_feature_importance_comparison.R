# ============================================================
# 08_FEATURE_IMPORTANCE_COMPARISON.R
# Compares feature importance across all models
# ============================================================

library(dplyr)
library(ggplot2)

# ── 1. LINEAR REGRESSION - top features by p-value ───────────
lm_features <- data.frame(
  Feature  = rownames(lm_coef),
  LM_pvalue = lm_coef$`Pr(>|t|)`
) %>%
  filter(LM_pvalue < 0.1) %>%
  arrange(LM_pvalue) %>%
  mutate(LM_rank = row_number())

# ── 2. LASSO - non-zero coefficients ─────────────────────────
lasso_features <- lasso_coef_df %>%
  mutate(LASSO_abs_coef = abs(Coefficient)) %>%
  arrange(desc(LASSO_abs_coef)) %>%
  mutate(LASSO_rank = row_number()) %>%
  select(Feature, LASSO_rank, LASSO_abs_coef)

# ── 3. DECISION TREE - top 30 features ───────────────────────
dt_features <- dt_importance %>%
  head(30) %>%
  mutate(DT_rank = row_number()) %>%
  select(Feature, DT_rank)

# ── 4. RANDOM FOREST - all features ──────────────────────────
rf_features <- rf_importance %>%
  mutate(RF_rank = row_number()) %>%
  select(Feature, RF_rank, RF_IncMSE = IncMSE)

# ── Merge all ─────────────────────────────────────────────────
comparison <- lm_features %>%
  full_join(lasso_features, by = "Feature") %>%
  full_join(dt_features,    by = "Feature") %>%
  full_join(rf_features,    by = "Feature") %>%
  arrange(RF_rank) %>%
  select(Feature, LM_rank, LASSO_rank, DT_rank, RF_rank)

cat("=== FEATURE IMPORTANCE ACROSS MODELS ===\n")
cat("Lower rank = more important | NA = not selected\n\n")
print(as.data.frame(comparison), row.names = FALSE)

# ── Features in ALL 4 models ──────────────────────────────────
cat("\n=== FEATURES IN ALL 4 MODELS ===\n")
all4 <- comparison %>%
  filter(!is.na(LM_rank) & !is.na(LASSO_rank) &
         !is.na(DT_rank) & !is.na(RF_rank)) %>%
  arrange(RF_rank)
print(as.data.frame(all4), row.names = FALSE)

# ── Features in 3+ models ─────────────────────────────────────
cat("\n=== FEATURES IN 3+ MODELS ===\n")
three_plus <- comparison %>%
  mutate(n_models = (!is.na(LM_rank)) + (!is.na(LASSO_rank)) +
                    (!is.na(DT_rank))  + (!is.na(RF_rank))) %>%
  filter(n_models >= 3) %>%
  arrange(desc(n_models), RF_rank)
print(as.data.frame(three_plus), row.names = FALSE)

# ── Plot ──────────────────────────────────────────────────────
plot_data <- rf_features %>%
  mutate(
    in_DT    = Feature %in% dt_features$Feature,
    in_LASSO = Feature %in% lasso_features$Feature,
    in_LM    = Feature %in% lm_features$Feature,
    group    = case_when(
      in_DT & in_LASSO ~ "DT + LASSO + RF",
      in_DT             ~ "DT + RF",
      in_LASSO          ~ "LASSO + RF",
      TRUE              ~ "RF only"
    )
  ) %>%
  head(25)

ggplot(plot_data, aes(x = reorder(Feature, RF_IncMSE), y = RF_IncMSE, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c(
    "DT + LASSO + RF" = "steelblue",
    "DT + RF"         = "orange",
    "LASSO + RF"      = "lightgreen",
    "RF only"         = "lightgrey"
  )) +
  labs(title = "Top 25 RF Features — coloured by model overlap",
       x = "Feature", y = "%IncMSE", fill = "Models") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
