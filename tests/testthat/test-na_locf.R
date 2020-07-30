context("na_locf")

test_that("All NA vector throws error", {
  expect_error(na_locf(c(NA, NA, NA, NA, NA)))
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)", {
  skip_on_cran()

  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[135:144] <- NA
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "mean")), digits = 1), 271.9)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "mean")), digits = 1), 266.7)
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "rev")), digits = 1), 271.9)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "rev")), digits = 1), 275.3)
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "rm")), digits = 1), 271.9)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "rm")), digits = 1), 266.7)
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "keep"), na.rm = T), digits = 1), 271.9)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "keep"), na.rm = T), digits = 1), 266.7)
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)", {
  skip_on_cran()

  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[1:5] <- NA
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "mean")), digits = 1), 284.3)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "mean")), digits = 1), 283.0)
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "rev")), digits = 1), 279.2)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "rev")), digits = 1), 283.0)
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "rm")), digits = 1), 284.3)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "rm")), digits = 1), 283.0)
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "keep"), na.rm = T), digits = 1), 284.3)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "keep"), na.rm = T), digits = 1), 283.0)
})


test_that("Correct results for all options with the tsAirgap dataset", {
  skip_on_cran()

  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "mean")), digits = 1), 278.8)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "mean")), digits = 1), 282.7)
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "rev")), digits = 1), 278.8)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "rev")), digits = 1), 282.7)
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "rm")), digits = 1), 278.8)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "rm")), digits = 1), 282.7)
  expect_equal(round(mean(na_locf(x, option = "locf", na_remaining = "keep"), na.rm = T), digits = 1), 278.8)
  expect_equal(round(mean(na_locf(x, option = "nocb", na_remaining = "keep"), na.rm = T), digits = 1), 282.7)
})

test_that("Imputation works for data.frame", {
  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "mean")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "mean")))
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "rev")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "rev")))
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "rm")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "rm")))
})



test_that("Error for wrong input for option parameter", {
  expect_error(na_locf(tsAirgap, option = "wrongOption"))
})

test_that("Error for wrong input for na_remaining parameter", {
  x <- tsAirgap
  x[1:2] <- NA
  expect_error(na_locf(x, na_remaining = "Wrong"))
})


test_that("Test NA at beginning", {
  x <- tsAirgap
  x[1:2] <- NA
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "mean")))
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "rev")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "mean")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "rev")))
  expect_equal(length(na_locf(x, option = "nocb", na_remaining = "rm")), 144)
  expect_equal(length(na_locf(x, option = "locf", na_remaining = "rm")), 142)
  expect_equal(length(na_locf(x, option = "nocb", na_remaining = "keep")), 144)

  expect_false(anyNA(na_locf(x)))
})

test_that("Test NA at end", {
  x <- tsAirgap
  x[143:144] <- NA
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "mean")))
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "rev")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "mean")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "rev")))
  expect_equal(length(na_locf(x, option = "nocb", na_remaining = "rm")), 142)
  expect_equal(length(na_locf(x, option = "locf", na_remaining = "rm")), 144)
  expect_false(anyNA(na_locf(x)))
})

test_that("Multiple NAs in a row", {
  x <- tsAirgap
  x[40:80] <- NA
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "mean")))
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "rev")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "mean")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "rev")))
  expect_equal(length(na_locf(x, option = "nocb", na_remaining = "rm")), 144)
  expect_equal(length(na_locf(x, option = "locf", na_remaining = "rm")), 144)
  expect_false(anyNA(na_locf(x)))
})

test_that("Over 90% NAs", {
  x <- tsAirgap
  x[10:140] <- NA
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "mean")))
  expect_false(anyNA(na_locf(x, option = "locf", na_remaining = "rev")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "mean")))
  expect_false(anyNA(na_locf(x, option = "nocb", na_remaining = "rev")))
  expect_false(anyNA(na_locf(x)))
})
