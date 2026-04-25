# =============================================================================
# K-MEANS CLUSTERING — SA2 Climate Regions
# Dataset: Dataset_Train.xlsx | Variables: monthly climate variables + lat/lon
# Years: 2013–2020
# This script ONLY evaluates and displays results.
# The final k decision is made by the user.
# =============================================================================

# ── CONFIGURABLE PARAMETERS ───────────────────────────────────────────────────

YEAR_START <- 2013                                          # First year to include
YEAR_END   <- 2020                                          # Last year to include
K_MIN      <- 2                                             # Minimum clusters to evaluate
K_MAX      <- 7                                             # Maximum clusters to evaluate
N_ITER     <- 100                                     # Maximum iterations per run
N_START    <- 25                                           # Number of random initializations
SEED       <- NULL                                          # NULL = random | number = reproducible
FILE_PATH  <- "C:/Users/Usuario/mi-proyecto-ml/Dataset_Train_A2v5.xlsx"   # ← YOUR FILE PATH
SHEET_NAME <- "TrainSet_13-22"                              # Sheet name

# =============================================================================


# ── 1. PACKAGES ───────────────────────────────────────────────────────────────

required_packages <- c("readxl", "dplyr", "tidyr", "cluster",
                       "ggplot2", "factoextra", "gridExtra")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}


# ── 2. LOAD AND FILTER DATA ───────────────────────────────────────────────────

cat("📂 Loading file:", FILE_PATH, "\n")
df_raw <- read_excel(FILE_PATH, sheet = SHEET_NAME)

cat("   Total rows:", nrow(df_raw), "| Years available:",
    paste(sort(unique(df_raw$YEAR)), collapse = ", "), "\n")

df <- df_raw %>% filter(YEAR >= YEAR_START & YEAR <= YEAR_END)
cat("   Rows after filtering (", YEAR_START, "-", YEAR_END, "):", nrow(df), "\n\n")


# ── 3. SELECT CLIMATE + GEOGRAPHIC VARIABLES ──────────────────────────────────

climate_vars <- names(df)[grepl("AirTemp|Rainfall|Radiation|WindAvg|Evaporation",
                                names(df))]
geo_vars     <- c("LONGITUDE", "LATITUDE")
feature_vars <- c(climate_vars, geo_vars)

cat("📊 Variables selected:", length(feature_vars),
    "(", length(climate_vars), "climate +", length(geo_vars), "geographic )\n\n")


# ── 4. AVERAGE BY SA2 — one row per region ────────────────────────────────────
# Averaging across years gives us the typical climate profile of each SA2,
# which is what we want to cluster on (not year-to-year variability)

sa2_avg <- df %>%
  group_by(SA2) %>%
  summarise(across(all_of(feature_vars), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

cat("🗺️  SA2 regions found:", nrow(sa2_avg), "\n")
cat("   ", paste(sa2_avg$SA2, collapse = " | "), "\n\n")


# ── 5. PREPROCESSING: IMPUTE MISSING VALUES + SCALE ──────────────────────────

X <- sa2_avg %>% select(all_of(feature_vars))

# Replace any remaining NAs with the column mean
X <- X %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Scale all variables to mean=0, sd=1
# This is critical — radiation is in millions while rainfall is in mm,
# so without scaling radiation would dominate the clustering entirely
X_scaled <- scale(X)

# Save scaled data so it can be reused without re-processing
saveRDS(list(X_scaled   = X_scaled,
             sa2_avg    = sa2_avg,
             YEAR_START = YEAR_START,
             YEAR_END   = YEAR_END,
             N_ITER     = N_ITER,
             N_START    = N_START),
        file = "kmeans_data.rds")

cat("✅ Data scaled. Matrix dimensions:", nrow(X_scaled), "x", ncol(X_scaled), "\n\n")


# ── 6. RANDOM SEED ────────────────────────────────────────────────────────────

if (!is.null(SEED)) {
  set.seed(SEED)
  cat("🔒 Fixed seed:", SEED, "\n\n")
} else {
  cat("🎲 Random initialization active (no fixed seed — results may vary between runs)\n\n")
}


# ── 7. EVALUATION: INERTIA + SILHOUETTE FOR k = K_MIN to K_MAX ───────────────

cat("⏳ Computing metrics for k =", K_MIN, "to", K_MAX, "...\n\n")

results <- data.frame(k = integer(), inertia = numeric(), silhouette = numeric())

for (k in K_MIN:K_MAX) {

  km <- kmeans(X_scaled,
               centers  = k,
               iter.max = N_ITER,
               nstart   = N_START)

  sil     <- silhouette(km$cluster, dist(X_scaled))
  sil_avg <- mean(sil[, 3])

  results <- rbind(results,
                   data.frame(k = k, inertia = km$tot.withinss, silhouette = sil_avg))

  cat(sprintf("   k = %d | Inertia: %8.1f | Silhouette: %.4f\n",
              k, km$tot.withinss, sil_avg))
}


# ── 8. PRINT SUMMARY TABLE ────────────────────────────────────────────────────

best_k_sil   <- results$k[which.max(results$silhouette)]
best_k_elbow <- results$k[which(diff(diff(results$inertia)) == max(diff(diff(results$inertia)))) + 1]

cat("\n=======================================================\n")
cat("  EVALUATION SUMMARY\n")
cat("=======================================================\n")
cat(sprintf("  %-6s %-14s %-14s\n", "k", "Inertia", "Silhouette"))
cat("  -----------------------------------------\n")

for (i in 1:nrow(results)) {
  elbow_tag <- ifelse(results$k[i] == best_k_elbow, " ← elbow",     "")
  sil_tag   <- ifelse(results$k[i] == best_k_sil,   " ← best silhouette", "")
  cat(sprintf("  %-6d %-14.1f %-14.4f%s%s\n",
              results$k[i], results$inertia[i], results$silhouette[i],
              elbow_tag, sil_tag))
}

cat("=======================================================\n")
cat(sprintf("  Elbow suggestion:      k = %d\n", best_k_elbow))
cat(sprintf("  Silhouette suggestion: k = %d  (score = %.4f)\n",
            best_k_sil, max(results$silhouette)))
cat("=======================================================\n\n")
cat("👉 The final choice of k is YOURS.\n")
cat("   Use kmeans_final.R and set K_FINAL to your chosen value.\n\n")


# ── 9. PLOTS — elbow and silhouette, no automatic selection marked ────────────

# Elbow plot — annotates the suggested elbow point only as a reference
p1 <- ggplot(results, aes(x = k, y = inertia)) +
  geom_line(color = "#378ADD", linewidth = 1) +
  geom_point(size = 4, shape = 21, stroke = 2,
             fill  = ifelse(results$k == best_k_elbow, "#378ADD", "white"),
             color = "#378ADD") +
  annotate("text", x = best_k_elbow, y = max(results$inertia) * 0.97,
           label = paste("elbow at k =", best_k_elbow),
           color = "#378ADD", hjust = 0.5, size = 3.5, fontface = "italic") +
  scale_x_continuous(breaks = K_MIN:K_MAX) +
  labs(title    = "Elbow Method — look for the bend where inertia stops dropping sharply",
       subtitle = paste("iter.max =", N_ITER, "| nstart =", N_START),
       x = "Number of clusters (k)", y = "Inertia (WCSS)") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Silhouette plot — highlights peak as a reference, not a decision
p2 <- ggplot(results, aes(x = k, y = silhouette)) +
  geom_line(color = "#1D9E75", linewidth = 1) +
  geom_point(size = 4, shape = 21, stroke = 2,
             fill  = ifelse(results$k == best_k_sil, "#1D9E75", "white"),
             color = "#1D9E75") +
  annotate("text", x = best_k_sil, y = max(results$silhouette) + 0.005,
           label = paste("peak at k =", best_k_sil),
           color = "#1D9E75", hjust = 0.5, size = 3.5, fontface = "italic") +
  scale_x_continuous(breaks = K_MIN:K_MAX) +
  labs(title    = "Silhouette Score — higher means better-separated clusters",
       subtitle = "Values range from -1 (bad) to +1 (perfect) | final k decision is yours",
       x = "Number of clusters (k)", y = "Average silhouette") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Show in Positron Plots panel
print(p1)
print(p2)

# Save plots
ggsave("kmeans_elbow.png",      p1, width = 7, height = 5, dpi = 130)
ggsave("kmeans_silhouette.png", p2, width = 7, height = 5, dpi = 130)

cat("✅ Plots saved: kmeans_elbow.png | kmeans_silhouette.png\n")
cat("✅ Data saved:  kmeans_data.rds\n\n")
cat("=======================================================\n")
cat("  NEXT STEP: open kmeans_final.R, set K_FINAL,\n")
cat("  and run it to get your cluster assignments.\n")
cat("=======================================================\n")
