
context("error-handling")
# These test are to make sure, the right errors / warnings are given 
# for wrong input data or wrong parameter inputs
# Especially important is the error handling for multivariate inputs like data.frames

test_that("Too few non-NA data points", {
  x <- c(NA, NA, NA, NA, NA, NA, NA)
  expect_error(na_seasplit(x), regexp = "At least")
  expect_error(na_seadec(x), regexp = "At least")
  expect_error(na_random(x), regexp = "At least")
  expect_error(na_mean(x), regexp = "At least")
  expect_error(na_ma(x), regexp = "At least")
  expect_error(na_locf(x), regexp = "At least")
  expect_error(na_kalman(x), regexp = "At least")
  expect_error(na_interpolation(x), regexp = "At least")
})

test_that("Data not numeric", {
  x1 <- rep("string",144)
  x1[3:15] <- NA
  expect_error(na_seasplit(x1), regexp = "not numeric")
  expect_error(na_seadec(x1), regexp = "not numeric")
  expect_error(na_random(x1), regexp = "not numeric")
  expect_error(na_mean(x1), regexp = "not numeric")
  expect_error(na_ma(x1), regexp = "not numeric")
  expect_error(na_locf(x1), regexp = "not numeric")
  expect_error(na_kalman(x1), regexp = "not numeric")
  expect_error(na_interpolation(x1), regexp = "not numeric")
})

test_that("Correct error messages for multiveriate inputs", {
  x1 <- rep("string",144)
  x1[3:15] <- NA
  x2 <- rep(3,144)
  x2[3] <- NA
  xyz <- data.frame(tsAirgap, x1, tsAirgap, x2)
  expect_warning(na_seasplit(xyz), regexp = "na_seasplit: No imputation performed for column 2")
  expect_warning(na_seasplit(xyz), regexp = "No seasonality information for dataset could be found")
  expect_warning(na_seadec(xyz), regexp = "na_seadec: No imputation performed for column 2")
  expect_warning(na_seadec(xyz), regexp = "No seasonality information for dataset could be found")
  expect_warning(na_random(xyz), regexp = "na_random: No imputation performed for column 2")
  expect_warning(na_mean(xyz), regexp = "na_mean: No imputation performed for column 2")
  expect_warning(na_ma(xyz), regexp = "na_ma: No imputation performed for column 2")
  expect_warning(na_locf(xyz), regexp = "na_locf: No imputation performed for column 2")
  expect_warning(na_kalman(xyz), regexp = "na_kalman: No imputation performed for column 2")
  expect_warning(na_interpolation(xyz), regexp = "na_interpolation: No imputation performed for column 2")
})
