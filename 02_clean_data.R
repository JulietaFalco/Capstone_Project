# ══════════════════════════════════════════════════════════════
# STEP 2 — Clean Data
# ══════════════════════════════════════════════════════════════

cols_to_drop <- c(
  # 100% missing
  "...93",
  
  # 35% missing - SoilTemp all months
  paste0("SoilTemp_month_", 1:12),
  
  # Identifier columns
  "station_code", "station_name", "YEAR",
  "SA2", "MyCode", "ABARES_region",
  
  # Cluster - not using as feature at this stage
  "Cluster",
  
  # Fertiliser prices
  "DAP (AU$/mt)", "TSP (AU$/mt)",
  "Urea (AU$/mt)", "Potassium chloride (AU$/mt)",
  
  # Fertiliser quantities
  "DAP (Qty * ha)", "Superphosphate (Qty * ha)",
  "Urea (Qty * ha)", "Potassium (Qty * 1ha)",
  
  # Wheat outcome variables - leakage
  "Wheat produced (t)", "Wheat area sown (ha)",
  "Wheat receipts ($)", "Wheat sold (t)",
  
  # Target-related leakage columns
  "Yield_1", "Yield_2", "ProdReal", "AreaRealHa",
  "Business", "% TargProd", "% TargArea", "AreaPerBusiness",
  
  # High multicollinearity with Wheat price
  "Fertiliser$tPerHA"
)

# Apply to TRAIN and TEST
Dataset_Train_v2 <- Dataset_Train_v1 %>% select(-any_of(cols_to_drop))
Dataset_Test_v2  <- Dataset_Test_v1  %>% select(-any_of(cols_to_drop))

cat("✓ Step 2 complete — Data cleaned\n")
cat("  Train:", nrow(Dataset_Train_v2), "rows x", ncol(Dataset_Train_v2), "cols\n")
cat("  Test: ", nrow(Dataset_Test_v2),  "rows x", ncol(Dataset_Test_v2),  "cols\n")
