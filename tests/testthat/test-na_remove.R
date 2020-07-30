context("na_remove")

test_that("All NA vector throws error", {
  expect_error(na_remove(c(NA, NA, NA, NA, NA)))
})

test_that("Wrong input", {
  x <- data.frame(tsAirgap, tsAirgap)
  expect_error(na_remove(x))
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)", {
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[135:144] <- NA
  expect_equal(round(mean(na_remove(x)), digits = 1), 264.1)
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)", {
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[1:5] <- NA
  expect_equal(round(mean(na_remove(x)), digits = 1), 284.8)
})


test_that("Correct results for all options with the tsAirgap dataset", {
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  expect_equal(round(mean(na_remove(x)), digits = 1), 279.8)
})

test_that("Test NA at beginning", {
  x <- tsAirgapComplete
  x[1:2] <- NA
  expect_equal(length(na_remove(x)), 142)
})

test_that("Test NA at end", {
  x <- tsAirgapComplete
  x[143:144] <- NA
  expect_equal(length(na_remove(x)), 142)
})

test_that("Multiple NAs in a row", {
  x <- tsAirgapComplete
  x[40:80] <- NA
  expect_equal(length(na_remove(x)), 103)
})

test_that("Over 90% NAs", {
  x <- tsAirgapComplete
  x[10:140] <- NA
  expect_equal(length(na_remove(x)), 13)
})
