context("na_random")

test_that("All NA vector throws error", {
  expect_error(na_random(c(NA, NA, NA, NA, NA)))
})

test_that("Wrong input", {
  x <- data.frame(tsAirgap)
  expect_error(na_random(x, lower_bound = 1, upper_bound = -1))

  x <- rep("string", 144)
  x[3] <- NA
  expect_error(na_random(x))

  x <- rep(NA, 144)
  expect_error(na_random(x))
})

test_that("Imputation works for data.frame", {
  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
  expect_false(anyNA(na_random(x)))
})


test_that("Error for lower_bound > upper_bound", {
  expect_error(na_random(tsAirgap, lower_bound = 300, upper_bound = 100))
})




test_that("Test NA at beginning", {
  x <- tsAirgap
  x[1:2] <- NA
  expect_false(anyNA(na_random(x, lower_bound = 100, upper_bound = 200)))
  expect_false(anyNA(na_random(x, lower_bound = -10, upper_bound = 5000)))
  expect_false(anyNA(na_random(x)))
})

test_that("Test NA at end", {
  x <- tsAirgap
  x[143:144] <- NA
  expect_false(anyNA(na_random(x, lower_bound = 100, upper_bound = 200)))
  expect_false(anyNA(na_random(x, lower_bound = -10, upper_bound = 5000)))
  expect_false(anyNA(na_random(x)))
})

test_that("Multiple NAs in a row", {
  x <- tsAirgap
  x[40:80] <- NA
  expect_false(anyNA(na_random(x, lower_bound = 100, upper_bound = 200)))
  expect_false(anyNA(na_random(x, lower_bound = -10, upper_bound = 5000)))
  expect_false(anyNA(na_random(x)))
})

test_that("Over 90% NAs", {
  x <- tsAirgap
  x[10:140] <- NA
  expect_false(anyNA(na_random(x, lower_bound = 100, upper_bound = 200)))
  expect_false(anyNA(na_random(x, lower_bound = -10, upper_bound = 5000)))
  expect_false(anyNA(na_random(x)))
})
