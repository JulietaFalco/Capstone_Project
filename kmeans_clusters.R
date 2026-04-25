# =============================================================================
# K-MEANS CLUSTERING — SA2 Climate Regions
# Dataset: Dataset_Train.xlsx | Variables: monthly climate variables + lat/lon
# Years: 2013–2020
# =============================================================================

# ── CONFIGURABLE PARAMETERS ───────────────────────────────────────────────────

YEAR_START    <- 2013          # First year to include in the analysis
YEAR_END      <- 2020          # Last year to include in the analysis
K_MIN         <- 2             # Minimum number of clusters to evaluate
K_MAX         <- 7             # Maximum number of clusters to evaluate
K_FINAL       <- 4             # Final cluster count to use (adjust after reviewing elbow/silhouette)
N_ITER        <- 50          # Maximum number of iterations per K-means run
N_START       <- 10            # Number of random initializations (tries different starting points)
SEED          <- NULL          # NULL = fully random each run | set a number for reproducibility
SHEET_NAME <- "TrainSet_13-22"   

# =============================================================================


# ── 1. PACKAGES ───────────────────────────────────────────────────────────────

required_packages <- c("readxl", "dplyr", "tidyr", "cluster",
                       "ggplot2", "factoextra", "scales")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}


# ── 2. LOAD AND FILTER DATA ───────────────────────────────────────────────────

cat("📂 Loading file:", "C:/Users/Usuario/mi-proyecto-ml/Dataset_Train_A2v4.xlsx", "\n")
df_raw <- read_excel("C:/Users/Usuario/mi-proyecto-ml/Dataset_Train_A2v4.xlsx", sheet = SHEET_NAME)

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

cat("✅ Data scaled. Matrix dimensions:", nrow(X_scaled), "x", ncol(X_scaled), "\n\n")


# ── 6. RANDOM SEED ────────────────────────────────────────────────────────────

if (!is.null(SEED)) {
  set.seed(SEED)
  cat("🔒 Fixed seed:", SEED, "\n")
} else {
  cat("🎲 Random initialization active (no fixed seed — results may vary between runs)\n")
}


# ── 7. EVALUATION: INERTIA + SILHOUETTE FOR k = K_MIN to K_MAX ───────────────

cat("\n⏳ Computing metrics for k =", K_MIN, "to", K_MAX, "...\n")

results <- data.frame(k = integer(), inertia = numeric(), silhouette = numeric())

for (k in K_MIN:K_MAX) {

  km <- kmeans(X_scaled,
               centers  = k,
               iter.max = N_ITER,    # max iterations per run
               nstart   = N_START)   # number of random initializations

  sil     <- silhouette(km$cluster, dist(X_scaled))
  sil_avg <- mean(sil[, 3])

  results <- rbind(results,
                   data.frame(k = k, inertia = km$tot.withinss, silhouette = sil_avg))

  cat(sprintf("   k = %d | Inertia: %8.1f | Silhouette: %.4f\n",
              k, km$tot.withinss, sil_avg))
}

best_k_sil <- results$k[which.max(results$silhouette)]
cat("\n⭐ Best k by Silhouette:", best_k_sil,
    "(score =", round(max(results$silhouette), 4), ")\n\n")


# ── 8. EVALUATION PLOTS ───────────────────────────────────────────────────────

# Elbow plot — look for the bend where inertia stops dropping sharply
p1 <- ggplot(results, aes(x = k, y = inertia)) +
  geom_line(color = "#378ADD", linewidth = 1) +
  geom_point(color = "#378ADD", size = 3, fill = "white", shape = 21, stroke = 2) +
  geom_vline(xintercept = K_FINAL, linetype = "dashed", color = "#D85A30", alpha = 0.7) +
  annotate("text", x = K_FINAL + 0.2, y = max(results$inertia) * 0.95,
           label = paste("k =", K_FINAL, "selected"),
           color = "#D85A30", hjust = 0, size = 3.5) +
  labs(title    = "Elbow Method",
       subtitle = paste("iter.max =", N_ITER, "| nstart =", N_START),
       x = "Number of clusters (k)", y = "Inertia (WCSS)") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Silhouette plot — higher is better; peak indicates the most natural k
p2 <- ggplot(results, aes(x = k, y = silhouette)) +
  geom_line(color = "#1D9E75", linewidth = 1) +
  geom_point(aes(color = k == best_k_sil), size = 4, shape = 21,
             fill = ifelse(results$k == best_k_sil, "#1D9E75", "white"),
             stroke = 2) +
  scale_color_manual(values = c("TRUE" = "#1D9E75", "FALSE" = "#1D9E75"),
                     guide  = "none") +
  geom_vline(xintercept = best_k_sil, linetype = "dashed",
             color = "#1D9E75", alpha = 0.6) +
  annotate("text", x = best_k_sil + 0.2, y = min(results$silhouette),
           label = paste("Best k =", best_k_sil),
           color = "#1D9E75", hjust = 0, size = 3.5) +
  labs(title    = "Silhouette Score",
       subtitle = "Higher = more compact and well-separated clusters",
       x = "Number of clusters (k)", y = "Average silhouette") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank())

print(p1)   # ← add this
print(p2)   # ← add this

# Save both evaluation plots side by side
png("kmeans_evaluation.png", width = 1400, height = 550, res = 130)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()
# Save both evaluation plots side by side
png("kmeans_evaluation.png", width = 1400, height = 550, res = 130)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()

# Fall back to saving separately if gridExtra is not available
tryCatch({
  if (!requireNamespace("gridExtra", quietly = TRUE)) stop("no gridExtra")
}, error = function(e) {
  ggsave("kmeans_elbow.png",      p1, width = 7, height = 5, dpi = 130)
  ggsave("kmeans_silhouette.png", p2, width = 7, height = 5, dpi = 130)
})


# ── 9. FINAL CLUSTER MODEL WITH K_FINAL ──────────────────────────────────────

cat("🔄 Running final K-means with k =", K_FINAL,
    "| iter.max =", N_ITER, "| nstart =", N_START, "\n\n")

km_final <- kmeans(X_scaled,
                   centers  = K_FINAL,
                   iter.max = N_ITER,
                   nstart   = N_START)

sa2_avg$cluster <- km_final$cluster

# Compute silhouette for the final model
sil_final <- silhouette(km_final$cluster, dist(X_scaled))
sil_score <- mean(sil_final[, 3])

cat("📈 Final silhouette score (k =", K_FINAL, "):", round(sil_score, 4), "\n")
cat("   Iterations used:", km_final$iter, "/", N_ITER, "\n\n")


# ── 10. RESULTS: WHICH SA2 BELONGS TO WHICH CLUSTER ──────────────────────────

cat("=======================================================\n")
cat("  RESULTS: SA2 BY CLUSTER\n")
cat("=======================================================\n")

for (c in sort(unique(sa2_avg$cluster))) {
  members <- sa2_avg$SA2[sa2_avg$cluster == c]
  cat(sprintf("\nCluster %d  (%d SA2):\n", c, length(members)))
  cat(paste(" •", members, collapse = "\n"), "\n")
}


# ── 11. CLUSTER CHARACTERIZATION ─────────────────────────────────────────────
# Summarise each cluster using January and July values to capture
# the summer and winter extremes of the climate profile

vars_summary <- c("AirTempAvg_month_1", "AirTempAvg_month_7",
                  "Rainfall_month_1",   "Rainfall_month_7",
                  "Radiation_month_1",  "Radiation_month_7",
                  "Evaporation_month_1","Evaporation_month_7")

cat("\n=======================================================\n")
cat("  CLUSTER CHARACTERIZATION (averages per cluster)\n")
cat("=======================================================\n")

characterization <- sa2_avg %>%
  group_by(cluster) %>%
  summarise(across(all_of(vars_summary), ~ round(mean(.x, na.rm = TRUE), 1)),
            n_SA2 = n(),
            .groups = "drop")

print(as.data.frame(characterization))


# ── 12. DETAILED SILHOUETTE PLOT ─────────────────────────────────────────────
# Shows the silhouette width for each individual SA2 — useful for spotting
# regions that are borderline between two clusters (values close to 0)

png("kmeans_silhouette_detail.png", width = 900, height = 600, res = 130)
plot(fviz_silhouette(sil_final, palette = c("#D85A30","#1D9E75","#378ADD","#F0A500"),
                     ggtheme = theme_minimal()) +
       labs(title    = paste("Silhouette detail — k =", K_FINAL),
            subtitle = paste("Average score:", round(sil_score, 4))) +
       theme(plot.title = element_text(face = "bold")))
dev.off()

print(fviz_silhouette(sil_final))  

# ── 13. GEOGRAPHIC MAP ────────────────────────────────────────────────────────
# Scatter plot using lat/lon to show the spatial distribution of clusters —
# well-separated clusters should also make geographic sense

cluster_colors <- c("#D85A30", "#1D9E75", "#378ADD", "#F0A500",
                    "#9B59B6", "#E67E22", "#1ABC9C")

p_map <- ggplot(sa2_avg, aes(x = LONGITUDE, y = LATITUDE,
                              color = factor(cluster),
                              label = SA2)) +
  geom_point(size = 5, alpha = 0.85) +
  geom_text(nudge_y = 0.3, size = 2.8, show.legend = FALSE) +
  scale_color_manual(values = cluster_colors[1:K_FINAL],
                     name   = "Cluster",
                     labels = paste("Cluster", 1:K_FINAL)) +
  labs(title    = paste("Geographic distribution — k =", K_FINAL, "clusters"),
       subtitle = paste("SA2 regions |", YEAR_START, "–", YEAR_END,
                        "| Silhouette:", round(sil_score, 3)),
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        legend.position  = "right",
        panel.grid.minor = element_blank())

ggsave("kmeans_map.png", p_map, width = 9, height = 7, dpi = 130)

print(p_map)

# ── 14. EXPORT CSV FOR POWER BI ───────────────────────────────────────────────
# This file can be imported into Power BI and joined to the main table
# on the SA2 column to colour maps and visuals by cluster

result_export <- sa2_avg %>%
  select(SA2, LONGITUDE, LATITUDE, cluster) %>%
  arrange(cluster, SA2)

write.csv(result_export, "SA2_clusters_R.csv", row.names = FALSE)

cat("\n=======================================================\n")
cat("✅ Files generated:\n")
cat("   • kmeans_evaluation.png        — Elbow + Silhouette curves\n")
cat("   • kmeans_silhouette_detail.png — Silhouette width per SA2\n")
cat("   • kmeans_map.png               — Geographic cluster map\n")
cat("   • SA2_clusters_R.csv           — Ready to import into Power BI\n")
cat("=======================================================\n")
