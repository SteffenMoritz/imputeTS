context("na_interpolation")

test_that("All NA vector throws error", {
  expect_error(na_interpolation(c(NA, NA, NA, NA, NA)))
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)", {
  skip_on_cran()

  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[135:144] <- NA
  expect_equal(round(mean(na_interpolation(x, option = "linear")), digits = 1), 273.6)
  expect_equal(round(mean(na_interpolation(x, option = "spline")), digits = 1), 276.2)
  expect_equal(round(mean(na_interpolation(x, option = "stine")), digits = 1), 273.4)
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)", {
  skip_on_cran()

  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[1:5] <- NA
  expect_equal(round(mean(na_interpolation(x, option = "linear")), digits = 1), 281.1)
  expect_equal(round(mean(na_interpolation(x, option = "spline")), digits = 1), 283.0)
  expect_equal(round(mean(na_interpolation(x, option = "stine")), digits = 1), 280.8)
})


test_that("Correct results for all options with the tsAirgap dataset", {
  skip_on_cran()

  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  expect_equal(round(mean(na_interpolation(x, option = "linear")), digits = 1), 280.7)
  expect_equal(round(mean(na_interpolation(x, option = "spline")), digits = 1), 280.1)
  expect_equal(round(mean(na_interpolation(x, option = "stine")), digits = 1), 280.5)
})

test_that("Imputation works for data.frame", {
  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
  expect_false(anyNA(na_interpolation(x, option = "linear")))
  expect_false(anyNA(na_interpolation(x, option = "spline")))
  expect_false(anyNA(na_interpolation(x, option = "stine")))
})

test_that("Error for wrong input for option parameter", {
  expect_error(na_interpolation(tsAirgap, option = "wrongOption"))
})



test_that("Test NA at beginning", {
  x <- tsAirgap
  x[1:2] <- NA
  expect_false(anyNA(na_interpolation(x, option = "linear")))
  expect_false(anyNA(na_interpolation(x, option = "spline")))
  expect_false(anyNA(na_interpolation(x, option = "stine")))
  expect_false(anyNA(na_interpolation(x)))
})

test_that("Test NA at end", {
  x <- tsAirgap
  x[143:144] <- NA
  expect_false(anyNA(na_interpolation(x, option = "linear")))
  expect_false(anyNA(na_interpolation(x, option = "spline")))
  expect_false(anyNA(na_interpolation(x, option = "stine")))
  expect_false(anyNA(na_interpolation(x)))
})

test_that("Multiple NAs in a row", {
  x <- tsAirgap
  x[40:80] <- NA
  expect_false(anyNA(na_interpolation(x, option = "linear")))
  expect_false(anyNA(na_interpolation(x, option = "spline")))
  expect_false(anyNA(na_interpolation(x, option = "stine")))
  expect_false(anyNA(na_interpolation(x)))
})

test_that("Over 90% NAs", {
  x <- tsAirgap
  x[10:140] <- NA
  expect_false(anyNA(na_interpolation(x, option = "linear")))
  expect_false(anyNA(na_interpolation(x, option = "spline")))
  expect_false(anyNA(na_interpolation(x, option = "stine")))
  expect_false(anyNA(na_interpolation(x)))
})
