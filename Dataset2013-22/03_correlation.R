# ══════════════════════════════════════════════════════════════
# STEP 3 — Correlation Analysis
# ══════════════════════════════════════════════════════════════

if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("ggplot2",  quietly = TRUE)) install.packages("ggplot2")

library(corrplot)
library(ggplot2)

# Correlation with target
cor_with_target <- Dataset_Train_v2 %>%
  select(-ProdPerBusiness) %>%
  cor(Dataset_Train_v2$ProdPerBusiness, use = "complete.obs") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("feature") %>%
  rename(correlation = V1) %>%
  arrange(desc(abs(correlation)))

# Top 20 plot
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

cat("✓ Step 3 complete — Correlation analysis done\n")