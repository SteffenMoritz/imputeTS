context("na_seasplit")

test_that("All NA vector throws error", {
  expect_error(na_seasplit(c(NA, NA, NA, NA, NA)))
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)", {
  skip_on_cran()
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[135:144] <- NA
  expect_equal(round(mean(na_seasplit(x, algorithm = "interpolation")), digits = 1), 276.6)
  expect_equal(round(mean(na_seasplit(x, algorithm = "locf")), digits = 1), 274.8)
  expect_equal(round(mean(na_seasplit(x, algorithm = "mean")), digits = 1), 264.0)
  expect_true(round(mean(na_seasplit(x, algorithm = "kalman", model = "auto.arima")), digits = 1) > 273 &
    round(mean(na_seasplit(x, algorithm = "kalman", model = "auto.arima")), digits = 1) < 277)
  expect_equal(round(mean(na_seasplit(x, algorithm = "ma")), digits = 1), 275.0)
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)", {
  skip_on_cran()
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[1:5] <- NA
  expect_equal(round(mean(na_seasplit(x, algorithm = "interpolation")), digits = 1), 280.5)
  expect_equal(round(mean(na_seasplit(x, algorithm = "locf")), digits = 1), 278.4)
  expect_equal(round(mean(na_seasplit(x, algorithm = "mean")), digits = 1), 283.4)
  expect_true(round(mean(na_seasplit(x, algorithm = "kalman", model = "auto.arima")), digits = 1) > 276 &
    round(mean(na_seasplit(x, algorithm = "kalman", model = "auto.arima")), digits = 1) < 280)
  expect_equal(round(mean(na_seasplit(x, algorithm = "ma")), digits = 1), 281.1)
})


test_that("Correct results for all options with the tsAirgap dataset", {
  skip_on_cran()
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  expect_equal(round(mean(na_seasplit(x, algorithm = "interpolation")), digits = 1), 280.3)
  expect_equal(round(mean(na_seasplit(x, algorithm = "locf")), digits = 1), 278.2)
  expect_equal(round(mean(na_seasplit(x, algorithm = "mean")), digits = 1), 279.3)
  expect_true(round(mean(na_seasplit(x, algorithm = "kalman", model = "auto.arima")), digits = 1) > 277 &
    round(mean(na_seasplit(x, algorithm = "kalman", model = "auto.arima")), digits = 1) < 281)
  expect_equal(round(mean(na_seasplit(x, algorithm = "ma")), digits = 1), 280.3)
})

test_that("Imputation works for data.frame", {
  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
  expect_false(anyNA(na_seasplit(x, algorithm = "interpolation")))
  expect_false(anyNA(na_seasplit(x, algorithm = "locf")))
  expect_false(anyNA(na_seasplit(x, algorithm = "mean")))
  expect_false(anyNA(na_seasplit(x, algorithm = "random")))
  expect_false(anyNA(na_seasplit(x, algorithm = "kalman")))
  expect_false(anyNA(na_seasplit(x, algorithm = "ma")))
})



test_that("Error for wrong input for algorithm parameter", {
  expect_error(na_seasplit(tsAirgap, algorithm = "wrong"))
})



test_that("Test NA at beginning", {
  x <- tsAirgap
  x[1:2] <- NA
  expect_false(anyNA(na_seasplit(x, algorithm = "interpolation")))
  expect_false(anyNA(na_seasplit(x, algorithm = "kalman")))
  expect_false(anyNA(na_seasplit(x, algorithm = "locf")))
  expect_false(anyNA(na_seasplit(x, algorithm = "ma")))
  expect_false(anyNA(na_seasplit(x, algorithm = "mean")))
  expect_false(anyNA(na_seasplit(x, algorithm = "random")))
  expect_false(anyNA(na_seasplit(x)))
})

test_that("Test NA at end", {
  x <- tsAirgap
  x[143:144] <- NA
  expect_false(anyNA(na_seasplit(x, algorithm = "interpolation")))
  expect_false(anyNA(na_seasplit(x, algorithm = "kalman")))
  expect_false(anyNA(na_seasplit(x, algorithm = "locf")))
  expect_false(anyNA(na_seasplit(x, algorithm = "ma")))
  expect_false(anyNA(na_seasplit(x, algorithm = "mean")))
  expect_false(anyNA(na_seasplit(x, algorithm = "random")))
  expect_false(anyNA(na_seasplit(x)))
})

test_that("Multiple NAs in a row", {
  x <- tsAirgap
  x[40:80] <- NA
  expect_false(anyNA(na_seasplit(x, algorithm = "interpolation")))
  expect_false(anyNA(na_seasplit(x, algorithm = "kalman")))
  expect_false(anyNA(na_seasplit(x, algorithm = "locf")))
  expect_false(anyNA(na_seasplit(x, algorithm = "ma")))
  expect_false(anyNA(na_seasplit(x, algorithm = "mean")))
  expect_false(anyNA(na_seasplit(x, algorithm = "random")))
  expect_false(anyNA(na_seasplit(x)))
})

test_that("Over 50% NAs", {
  x <- tsAirgap
  x[30:100] <- NA
  expect_false(anyNA(na_seasplit(x, algorithm = "interpolation")))
  expect_false(anyNA(na_seasplit(x, algorithm = "kalman")))
  expect_false(anyNA(na_seasplit(x, algorithm = "locf")))
  expect_false(anyNA(na_seasplit(x, algorithm = "ma")))
  expect_false(anyNA(na_seasplit(x, algorithm = "mean")))
  expect_false(anyNA(na_seasplit(x, algorithm = "random")))
  expect_false(anyNA(na_seasplit(x)))
})

test_that("No Seasonality in series", {
  x <- ts(c(3, 5, 6, 7, 8, 4, 5, 6, NA, NA, 5, 7, 4, 2, NA, NA, 5, 7, 8))
  expect_false(anyNA(na_seasplit(x, algorithm = "interpolation")))
  expect_false(anyNA(na_seasplit(x, algorithm = "kalman")))
  expect_false(anyNA(na_seasplit(x, algorithm = "locf")))
  expect_false(anyNA(na_seasplit(x, algorithm = "ma")))
  expect_false(anyNA(na_seasplit(x, algorithm = "mean")))
  expect_false(anyNA(na_seasplit(x, algorithm = "random")))
  expect_false(anyNA(na_seasplit(x)))
})

test_that("Handling for no NAs", {
  x <- tsAirgapComplete
  expect_false(anyNA(na_seasplit(x)))
})


test_that("Correct results for all options with the tsAirgap dataset for find_frequency", {
  skip_on_cran()
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- as.vector(tsAirgap)

  expect_equal(round(mean(na_seasplit(x, algorithm = "interpolation", find_frequency = TRUE)), digits = 1), 280.3)
  expect_equal(round(mean(na_seasplit(x, algorithm = "interpolation", find_frequency = FALSE)), digits = 1), 280.7)

  # Check that other frequencys lead to different result if find_frequency is FALSE
  expect_equal(round(mean(na_seasplit(ts(x, frequency = 2), algorithm = "interpolation", find_frequency = FALSE)), digits = 1), 281.7)

  # Check that find_frequency overrides frequency for find_frequency = T
  expect_equal(round(mean(na_seasplit(ts(x, frequency = 2), algorithm = "interpolation", find_frequency = TRUE)), digits = 1), 280.3)

  expect_equal(round(mean(na_seasplit(x, algorithm = "locf", find_frequency = TRUE)), digits = 1), 278.2)
  expect_equal(round(mean(na_seasplit(x, algorithm = "locf", find_frequency = FALSE)), digits = 1), 278.8)

  expect_equal(round(mean(na_seasplit(x, algorithm = "mean", find_frequency = TRUE)), digits = 1), 279.3)
  expect_equal(round(mean(na_seasplit(x, algorithm = "mean", find_frequency = FALSE)), digits = 1), 279.8)


  expect_true(round(mean(na_seasplit(x, algorithm = "kalman", model = "auto.arima", find_frequency = TRUE)), digits = 1) > 277 &
    round(mean(na_seasplit(x, algorithm = "kalman", model = "auto.arima", find_frequency = TRUE)), digits = 1) < 281)

  expect_equal(round(mean(na_seasplit(x, algorithm = "ma", find_frequency = TRUE)), digits = 1), 280.3)
  expect_equal(round(mean(na_seasplit(x, algorithm = "ma", find_frequency = FALSE)), digits = 1), 281.2)
})
