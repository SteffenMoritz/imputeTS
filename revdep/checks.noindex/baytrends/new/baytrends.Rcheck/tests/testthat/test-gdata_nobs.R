# Confirm that baytrends::nobs gives the same results as gdata::nobs
# vector
test_that("nobs, vector", {
  # Example
  x <- c(1,2,3,5,NA,6,7,1,NA )
  length(x)
  calc_nobs <- nobs(x)
  
  qc_nobs <- 7
  
  # Test
  expect_equal(qc_nobs, calc_nobs)
})

# Confirm that baytrends::nobs gives the same results as gdata::nobs
# data frame
test_that("nobs, data frame", {
  # Example
  df <- data.frame(x=rnorm(100), y=rnorm(100))
  df[1,1] <- NA
  df[1,2] <- NA
  df[2,1] <- NA
  
  calc_nobs <- nobs(df)
  
  qc_nobs <- c(98, 99)
  names(qc_nobs) <- c("x", "y")
  
  # Test
  expect_equal(qc_nobs, calc_nobs)
})

# Confirm that baytrends::nobs gives the same results as gdata::nobs
# linear model
test_that("nobs, linear model", {
  # Example
  df <- data.frame(x=rnorm(100), y=rnorm(100))
  df[1,1] <- NA
  df[1,2] <- NA
  df[2,1] <- NA
  
  fit <- lm(y ~ x, data=df)
  calc_nobs <- nobs(fit)
  
  qc_nobs <- 98
  
  # Test
  expect_equal(qc_nobs, calc_nobs)
})