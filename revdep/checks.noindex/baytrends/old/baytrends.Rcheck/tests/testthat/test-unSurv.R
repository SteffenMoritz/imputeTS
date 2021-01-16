# Confirm that the unSurv(dataCensored) workflow with default settings will
# yield a 3-column matrix with correct names
test_that("unSurv, colnames default", {
  # Example
  df1 <- dataCensored[dataCensored$station=="CB3.3C","chla"][1:30]
  # Default values
  df2 <- unSurv(df1)
 
  # Define Expected
  qc_col <- c("lo", "hi", "type")

    # Test
  expect_equal(colnames(df2), qc_col)
})

# Confirm that the unSurv(dataCensored) workflow with user-specified settings will
# yield a 3-column matrix with correct names
test_that("unSurv, colnames user", {
  # Example
  df1 <- dataCensored[dataCensored$station=="CB3.3C","chla"][1:30]
  # User values
  df3 <- unSurv(df1, "LOW", "HIGH")
  
  # Define Expected
  qc_col <- c("LOW", "HIGH", "type")
  
  # Test
  expect_equal(colnames(df3), qc_col)
})
