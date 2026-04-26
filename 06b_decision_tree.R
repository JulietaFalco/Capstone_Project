# ══════════════════════════════════════════════════════════════
# STEP 6b — Decision Tree Variants (Overfitting Test)
# ══════════════════════════════════════════════════════════════

# ── Define cp values to test ──────────────────────────────────
cp_values <- c(0.15, 0.08, 0.045, 0.01)
tree_names <- c("Very Simple (cp=0.15)", 
                "Simple (cp=0.08)", 
                "Current (cp=0.045)", 
                "Complex (cp=0.01)")

# ── Train and evaluate each tree ──────────────────────────────
results_trees <- data.frame()

for(i in seq_along(cp_values)){
  
  # Train tree
  tree_i <- rpart(
    ProdPerBusiness ~ .,
    data    = Train_scaled,
    method  = "anova",
    control = rpart.control(
      cp       = cp_values[i],
      minsplit = 10,
      maxdepth = 10
    )
  )
  
  # Get tree size
  n_leaves <- sum(tree_i$frame$var == "<leaf>")
  n_splits <- nrow(tree_i$frame) - n_leaves
  
  # Train performance
  train_pred_i <- predict(tree_i, Train_scaled)
  train_rmse_i <- rmse(Train_scaled$ProdPerBusiness, train_pred_i)
  train_r2_i   <- cor(Train_scaled$ProdPerBusiness,  train_pred_i)^2
  
  # Test performance
  test_pred_i  <- predict(tree_i, Test_scaled)
  test_rmse_i  <- rmse(Test_scaled$ProdPerBusiness,  test_pred_i)
  test_r2_i    <- cor(Test_scaled$ProdPerBusiness,   test_pred_i)^2
  
  # AIC
  resid_i <- Train_scaled$ProdPerBusiness - train_pred_i
  rss_i   <- sum(resid_i^2)
  aic_i   <- nrow(Train_scaled) * log(rss_i / nrow(Train_scaled)) + 2 * n_leaves
  
  # Store results
  results_trees <- rbind(results_trees, data.frame(
    Model      = tree_names[i],
    cp         = cp_values[i],
    Splits     = n_splits,
    Leaves     = n_leaves,
    Train_RMSE = round(train_rmse_i, 2),
    Train_R2   = round(train_r2_i,   4),
    Test_RMSE  = round(test_rmse_i,  2),
    Test_R2    = round(test_r2_i,    4),
    AIC        = round(aic_i,        2)
  ))
}

cat("=== DECISION TREE VARIANTS COMPARISON ===\n")
print(results_trees)

# ── Plot Train vs Test RMSE by complexity ─────────────────────
results_long <- results_trees %>%
  select(Model, cp, Splits, Train_RMSE, Test_RMSE) %>%
  tidyr::pivot_longer(
    cols      = c(Train_RMSE, Test_RMSE),
    names_to  = "Dataset",
    values_to = "RMSE"
  )

ggplot(results_long, aes(x = Splits, y = RMSE, 
                          color = Dataset, group = Dataset)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Train_RMSE" = "steelblue", 
                                "Test_RMSE"  = "tomato")) +
  labs(title = "Decision Tree — Train vs Test RMSE by Tree Complexity",
       x     = "Number of Splits",
       y     = "RMSE") +
  theme_minimal()

# ── Plot Train vs Test R2 by complexity ───────────────────────
results_long_r2 <- results_trees %>%
  select(Model, cp, Splits, Train_R2, Test_R2) %>%
  tidyr::pivot_longer(
    cols      = c(Train_R2, Test_R2),
    names_to  = "Dataset",
    values_to = "R2"
  )

ggplot(results_long_r2, aes(x = Splits, y = R2,
                             color = Dataset, group = Dataset)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Train_R2" = "steelblue",
                                "Test_R2"  = "tomato")) +
  labs(title = "Decision Tree — Train vs Test R² by Tree Complexity",
       x     = "Number of Splits",
       y     = "R²") +
  theme_minimal()

# ── Visualize the best alternative tree ───────────────────────
# Simple tree (cp = 0.08)
tree_simple <- rpart(
  ProdPerBusiness ~ .,
  data    = Train_scaled,
  method  = "anova",
  control = rpart.control(cp = 0.08, minsplit = 10, maxdepth = 10)
)

png("decision_tree_simple.png", width = 1200, height = 800, res = 120)
rpart.plot(
  tree_simple,
  type          = 4,
  extra         = 101,
  fallen.leaves = TRUE,
  main          = "Decision Tree Simple (cp=0.08)",
  cex           = 0.7
)
dev.off()
cat("✓ Simple tree saved to decision_tree_simple.png\n")

# Complex tree (cp = 0.01)
tree_complex <- rpart(
  ProdPerBusiness ~ .,
  data    = Train_scaled,
  method  = "anova",
  control = rpart.control(cp = 0.01, minsplit = 10, maxdepth = 10)
)

png("decision_tree_complex.png", width = 1600, height = 1000, res = 120)
rpart.plot(
  tree_complex,
  type          = 4,
  extra         = 101,
  fallen.leaves = TRUE,
  main          = "Decision Tree Complex (cp=0.01)",
  cex           = 0.6
)
dev.off()
cat("✓ Complex tree saved to decision_tree_complex.png\n")

cat("\n✓ Step 6b complete — Decision Tree variants done\n")