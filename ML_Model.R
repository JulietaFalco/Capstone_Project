#STEP 1 — Load and Explore the Data

# ── Install required packages if needed ───────────────────────
if (!requireNamespace("readxl",  quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr",   quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("skimr",   quietly = TRUE)) install.packages("skimr")

library(readxl)
library(dplyr)
library(ggplot2)
library(skimr)

# ── Load datasets ─────────────────────────────────────────────
Dataset_Train_v1 <- read_excel("C:/Users/Usuario/mi-proyecto-ml/Dataset_Train_v1.xlsx")
Dataset_Test_v1  <- read_excel("C:/Users/Usuario/mi-proyecto-ml/Dataset_Test_v1.xlsx")

# ── TRAIN: Basic structure ────────────────────────────────────
cat("=== TRAIN DIMENSIONS ===\n")
cat("Rows:", nrow(Dataset_Train_v1), "\n")
cat("Columns:", ncol(Dataset_Train_v1), "\n")

cat("\n=== TRAIN MISSING VALUES (only columns with NA) ===\n")
na_cols <- colSums(is.na(Dataset_Train_v1))
print(na_cols[na_cols > 0])

cat("\n=== TRAIN TARGET SUMMARY (ProdPerBusiness) ===\n")
print(summary(Dataset_Train_v1$ProdPerBusiness))

cat("\n=== TRAIN CLUSTER DISTRIBUTION ===\n")
print(table(Dataset_Train_v1$Cluster))

# ── TEST: Basic structure ─────────────────────────────────────
cat("\n=== TEST DIMENSIONS ===\n")
cat("Rows:", nrow(Dataset_Test_v1), "\n")
cat("Columns:", ncol(Dataset_Test_v1), "\n")

cat("\n=== TEST MISSING VALUES (only columns with NA) ===\n")
na_cols_test <- colSums(is.na(Dataset_Test_v1))
print(na_cols_test[na_cols_test > 0])

cat("\n=== TEST CLUSTER DISTRIBUTION ===\n")
print(table(Dataset_Test_v1$Cluster))

# ── ProdPerBusiness summary by cluster (TRAIN) ────────────────
cat("\n=== TRAIN TARGET SUMMARY BY CLUSTER ===\n")
Dataset_Train_v1 %>%
  group_by(Cluster) %>%
  summarise(
    n           = n(),
    mean_prod   = round(mean(ProdPerBusiness,   na.rm = TRUE), 2),
    median_prod = round(median(ProdPerBusiness, na.rm = TRUE), 2),
    sd_prod     = round(sd(ProdPerBusiness,     na.rm = TRUE), 2),
    min_prod    = round(min(ProdPerBusiness,    na.rm = TRUE), 2),
    max_prod    = round(max(ProdPerBusiness,    na.rm = TRUE), 2)
  ) %>%
  print()

# ── Target distribution histogram by cluster ──────────────────
ggplot(Dataset_Train_v1, aes(x = ProdPerBusiness, fill = Cluster)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.7) +
  facet_wrap(~Cluster) +
  labs(title = "Distribution of ProdPerBusiness by Cluster (Train)",
       x     = "Production per Business",
       y     = "Count") +
  theme_minimal()

# ── Step 2: Clean the data ────────────────────────────────────

# All columns to drop
cols_to_drop <- c(
  # 100% missing
  "...93",
  
  # 35% missing - SoilTemp all months
  paste0("SoilTemp_month_", 1:12),
  
  # Identifier columns (non-features)
  "station_code", "station_name", "YEAR",
  "SA2", "MyCode", "ABARES_region",
  
  # Cluster - not using as feature at this stage
  "Cluster",
  
  # Fertiliser prices - already combined in Fertiliser$tPerHA
  "DAP (AU$/mt)", "TSP (AU$/mt)", 
  "Urea (AU$/mt)", "Potassium chloride (AU$/mt)",
  
  # Fertiliser quantities - already combined in Fertiliser$tPerHA
  "DAP (Qty * ha)", "Superphosphate (Qty * ha)", 
  "Urea (Qty * ha)", "Potassium (Qty * 1ha)",
  
  # Wheat outcome variables - leakage
  "Wheat produced (t)", "Wheat area sown (ha)", 
  "Wheat receipts ($)", "Wheat sold (t)",
  
  # Target-related leakage columns
  "Yield_1", "Yield_2", "ProdReal", "AreaRealHa",
  "Business", "% TargProd", "% TargArea", "AreaPerBusiness"
)

# ── Apply to TRAIN ────────────────────────────────────────────
Dataset_Train_v2 <- Dataset_Train_v1 %>%
  select(-any_of(cols_to_drop))

# ── Apply to TEST ─────────────────────────────────────────────
Dataset_Test_v2 <- Dataset_Test_v1 %>%
  select(-any_of(cols_to_drop))

# ── Verify dimensions ─────────────────────────────────────────
cat("=== TRAIN AFTER CLEANING ===\n")
cat("Rows:", nrow(Dataset_Train_v2), "\n")
cat("Columns:", ncol(Dataset_Train_v2), "\n")

cat("\n=== TEST AFTER CLEANING ===\n")
cat("Rows:", nrow(Dataset_Test_v2), "\n")
cat("Columns:", ncol(Dataset_Test_v2), "\n")

# ── Verify remaining missing values ───────────────────────────
cat("\n=== REMAINING MISSING VALUES (TRAIN) ===\n")
na_cols <- colSums(is.na(Dataset_Train_v2))
if(sum(na_cols > 0) == 0){
  cat("No missing values found\n")
} else {
  print(na_cols[na_cols > 0])
}

cat("\n=== REMAINING MISSING VALUES (TEST) ===\n")
na_cols_test <- colSums(is.na(Dataset_Test_v2))
if(sum(na_cols_test > 0) == 0){
  cat("No missing values found\n")
} else {
  print(na_cols_test[na_cols_test > 0])
}

# ── Confirm final column list ─────────────────────────────────
cat("\n=== FINAL COLUMNS IN TRAIN ===\n")
print(colnames(Dataset_Train_v2))

# ── Confirm features and target ───────────────────────────────
cat("\n=== KEY COLUMNS CHECK ===\n")
cat("TARGET - ProdPerBusiness in Train:", 
    "ProdPerBusiness" %in% colnames(Dataset_Train_v2), "\n")
cat("FEATURE - Fertiliser$tPerHA in Train:", 
    "Fertiliser$tPerHA" %in% colnames(Dataset_Train_v2), "\n")
cat("FEATURE - Wheat AU($/mt) in Train:", 
    "Wheat AU($/mt)" %in% colnames(Dataset_Train_v2), "\n")
cat("FEATURE - LONGITUDE in Train:", 
    "LONGITUDE" %in% colnames(Dataset_Train_v2), "\n")
cat("FEATURE - LATITUDE in Train:", 
    "LATITUDE" %in% colnames(Dataset_Train_v2), "\n")

# ── Install required packages if needed ───────────────────────
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("ggplot2",  quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr",    quietly = TRUE)) install.packages("dplyr")

library(corrplot)
library(ggplot2)
library(dplyr)


# STEP 3:
# ── 1. Correlation of ALL features with TARGET ────────────────
cor_with_target <- Dataset_Train_v2 %>%
  select(-ProdPerBusiness) %>%
  cor(Dataset_Train_v2$ProdPerBusiness, use = "complete.obs") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("feature") %>%
  rename(correlation = V1) %>%
  arrange(desc(abs(correlation)))

cat("=== CORRELATION WITH ProdPerBusiness (sorted by absolute value) ===\n")
print(tibble::as_tibble(cor_with_target), n = 76)

# ── 2. Top 15 positive and negative correlations ──────────────
top_positive <- cor_with_target %>% 
  filter(correlation > 0) %>% 
  head(15)

top_negative <- cor_with_target %>% 
  filter(correlation < 0) %>% 
  head(15)

cat("\n=== TOP 15 POSITIVE CORRELATIONS ===\n")
print(top_positive)

cat("\n=== TOP 15 NEGATIVE CORRELATIONS ===\n")
print(top_negative)

# ── 3. Bar plot of top 20 features by absolute correlation ────
top_20 <- cor_with_target %>% head(20)

ggplot(top_20, aes(x = reorder(feature, abs(correlation)), 
                   y = correlation, 
                   fill = correlation > 0)) +
  geom_bar(stat = "identity", color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    labels = c("Negative", "Positive"),
                    name   = "Direction") +
  labs(title = "Top 20 Features by Correlation with ProdPerBusiness",
       x     = "Feature",
       y     = "Correlation") +
  theme_minimal()

# ── 4. Identify highly correlated features (multicollinearity) ─
cor_matrix <- Dataset_Train_v2 %>%
  select(-ProdPerBusiness) %>%
  cor(use = "complete.obs")

# Find pairs with correlation > 0.85
high_cor <- which(abs(cor_matrix) > 0.85 & 
                  upper.tri(cor_matrix), arr.ind = TRUE)

cat("\n=== HIGHLY CORRELATED FEATURE PAIRS (|r| > 0.85) ===\n")
if(nrow(high_cor) == 0){
  cat("No highly correlated pairs found\n")
} else {
  high_cor_df <- data.frame(
    feature_1   = rownames(cor_matrix)[high_cor[, 1]],
    feature_2   = colnames(cor_matrix)[high_cor[, 2]],
    correlation = cor_matrix[high_cor]
  ) %>% arrange(desc(abs(correlation)))
  print(high_cor_df)
}

#STEP 4 — Baseline Linear Regression

# ── Install required packages if needed ───────────────────────
if (!requireNamespace("caret",   quietly = TRUE)) install.packages("caret")
if (!requireNamespace("metrics", quietly = TRUE)) install.packages("Metrics")

library(caret)
library(Metrics)
library(dplyr)

# ── 1. Create clean dataset (drop Fertiliser$tPerHA) ─────────
# Keep Wheat AU($/mt) for now as discussed
cols_to_drop_v2 <- c("Fertiliser$tPerHA")

Dataset_Train_v3 <- Dataset_Train_v2 %>%
  select(-any_of(cols_to_drop_v2))

Dataset_Test_v3 <- Dataset_Test_v2 %>%
  select(-any_of(cols_to_drop_v2))