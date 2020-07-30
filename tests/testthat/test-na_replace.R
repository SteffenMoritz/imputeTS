context("na_replace")


test_that("All NA vector throws no error", {
  expect_equal(sum(na_replace(c(NA, NA, NA, NA, NA), fill = 2.0)), 10.0)
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)", {
  skip_on_cran()

  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[135:144] <- NA
  expect_equal(round(mean(na_replace(x, fill = -1)), digits = 1), 223.6)
  expect_equal(round(mean(na_replace(x, fill = 200)), digits = 1), 254.3)
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)", {
  skip_on_cran()

  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[1:5] <- NA
  expect_equal(round(mean(na_replace(x, fill = -1)), digits = 1), 251.0)
  expect_equal(round(mean(na_replace(x, fill = 200)), digits = 1), 274.8)
})


test_that("Correct results for all options with the tsAirgap dataset", {
  skip_on_cran()

  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  expect_equal(round(mean(na_replace(x, fill = -1)), digits = 1), 254.5)
  expect_equal(round(mean(na_replace(x, fill = 200)), digits = 1), 272.6)
})

test_that("Imputation works for data.frame", {
  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
  expect_false(anyNA(na_replace(x, fill = -1)))
  expect_false(anyNA(na_replace(x, fill = 200)))
})

test_that("Test NA at beginning", {
  x <- tsAirgap
  x[1:2] <- NA
  expect_false(anyNA(na_replace(x, fill = -5)))
  expect_false(anyNA(na_replace(x, fill = 1000)))
  expect_false(anyNA(na_replace(x)))
})

test_that("Test NA at end", {
  x <- tsAirgap
  x[143:144] <- NA
  expect_false(anyNA(na_replace(x, fill = -5)))
  expect_false(anyNA(na_replace(x, fill = 1000)))
  expect_false(anyNA(na_replace(x)))
})

test_that("Multiple NAs in a row", {
  x <- tsAirgap
  x[40:80] <- NA
  expect_false(anyNA(na_replace(x, fill = -5)))
  expect_false(anyNA(na_replace(x, fill = 1000)))
  expect_false(anyNA(na_replace(x)))
})

test_that("Over 90% NAs", {
  x <- tsAirgap
  x[10:140] <- NA
  expect_false(anyNA(na_replace(x, fill = -5)))
  expect_false(anyNA(na_replace(x, fill = 1000)))
  expect_false(anyNA(na_replace(x)))
})

test_that("Handling for no NAs", {
  x <- tsAirgapComplete
  expect_false(anyNA(na_replace(x)))
})
