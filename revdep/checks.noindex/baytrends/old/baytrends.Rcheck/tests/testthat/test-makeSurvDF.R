# Confirm that the unSurvDF(dataCensored) -> makeSurvDF(dataCensored) workflow
# using default settings will yield the expected column names
test_that("makeSurvDF, colnames default", {
  # Example
  df <- dataCensored[1:20,]
  df1 <- unSurvDF(df)
  # Default values
  df2 <- makeSurvDF(df1)
  
  # Define expected
  qc_col <-  c("station",  "date",     "layer",    "secchi",   "salinity", "do",       "wtemp",    "tss",     
    "chla",     "din",      "nh4",      "no23",     "po4",      "tdn",      "tdp",      "tn",      
    "tp")
  
  # Test
  expect_equal(colnames(df2), qc_col)
})

# Confirm that the unSurvDF(dataCensored) -> makeSurvDF(dataCensored) workflow
# using user-specified settings will yield the expected column names
test_that("makeSurvDF, colnames user", {
  # Example
  df <- dataCensored[1:20,]
  # User values
  df3 <- unSurvDF(df, "_LOW", "_HIGH")
  df4 <- makeSurvDF(df3, "_LOW", "_HIGH")

  # Define expected
  qc_col <-  c("station",  "date",     "layer",    "secchi",   "salinity", "do",       "wtemp",    "tss",     
               "chla",     "din",      "nh4",      "no23",     "po4",      "tdn",      "tdp",      "tn",      
               "tp")
  
  # Test
  expect_equal(colnames(df4), qc_col)
})
