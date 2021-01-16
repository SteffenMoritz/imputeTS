# Confirm that the unSurvDF(dataCensored) workflow with default settings will
# yield a 3-column matrix with correct names
test_that("unSurvDF, colnames default", {
  # Example
  df <- dataCensored[dataCensored$station=="CB3.3C", ][1:20,]
  # Default values
  df2 <- unSurvDF(df)
  
  # Define Expected
  qc_col <- c("station", "date", "layer", "secchi", "salinity", "do",       "wtemp",    "tss",
              "chla_lo", "chla_hi",  "din_lo",   "din_hi",   "nh4_lo",   "nh4_hi",   "no23_lo",  "no23_hi",
              "po4_lo", "po4_hi",   "tdn_lo",   "tdn_hi",   "tdp_lo",   "tdp_hi",   "tn_lo",    "tn_hi",
              "tp_lo", "tp_hi")
  
  # Test
  expect_equal(colnames(df2), qc_col)
})

# Confirm that the unSurvDF(dataCensored) workflow with user-specified settings will
# yield a 3-column matrix with correct names
test_that("unSurvDF, colnames user", {
  # Example
  df <- dataCensored[dataCensored$station=="CB3.3C", ][1:20,]
  # User values
  df3 <- unSurvDF(df, "_LOW", "_HIGH")
  
  # Define Expected
  qc_col <- c("station", "date", "layer", "secchi", "salinity", "do",       "wtemp",    "tss",
  "chla_LOW", "chla_HIGH",  "din_LOW",   "din_HIGH",   "nh4_LOW",   "nh4_HIGH",   "no23_LOW",  "no23_HIGH",
  "po4_LOW", "po4_HIGH",   "tdn_LOW",   "tdn_HIGH",   "tdp_LOW",   "tdp_HIGH",   "tn_LOW",    "tn_HIGH",
  "tp_LOW", "tp_HIGH")
  
  # Test
  expect_equal(colnames(df3), qc_col)
})