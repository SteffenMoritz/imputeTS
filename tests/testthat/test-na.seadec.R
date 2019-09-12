context("na.seadec")

test_that("All NA vector throws error",
          {
            expect_error(na.seadec(c(NA,NA,NA,NA,NA)))
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)", {
  skip_on_cran()
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[135:144] <- NA
  expect_that(round(mean(na.seadec(x, algorithm = "interpolation")), digits = 1), is_identical_to(276.7))
  expect_that(round(mean(na.seadec(x, algorithm = "locf")), digits = 1), is_identical_to(276.0))
  expect_that(round(mean(na.seadec(x, algorithm = "mean")), digits = 1), is_identical_to(264.3))
  expect_that(round(mean(na.seadec(x, algorithm = "kalman", model = "auto.arima")), digits = 1) > 277 &
    round(mean(na.seadec(x, algorithm = "kalman", model = "auto.arima")), digits = 1) < 283, is_true())
  expect_that(round(mean(na.seadec(x, algorithm = "ma")), digits = 1), is_identical_to(277.1))
})

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)", {
  skip_on_cran()
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  x[1:5] <- NA
  expect_that(round(mean(na.seadec(x, algorithm = "interpolation")), digits = 1), is_identical_to(279.9))
  expect_that(round(mean(na.seadec(x, algorithm = "locf")), digits = 1), is_identical_to(279.2))
  expect_that(round(mean(na.seadec(x, algorithm = "mean")), digits = 1), is_identical_to(284.1))
  expect_that(round(mean(na.seadec(x, algorithm = "kalman", model = "auto.arima")), digits = 1) > 282 &
    round(mean(na.seadec(x, algorithm = "kalman", model = "auto.arima")), digits = 1) < 288, is_true())
  expect_that(round(mean(na.seadec(x, algorithm = "ma")), digits = 1), is_identical_to(280.0))
})


test_that("Correct results for all options with the tsAirgap dataset", {
  skip_on_cran()
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- tsAirgap
  expect_that(round(mean(na.seadec(x, algorithm = "interpolation")), digits = 1), is_identical_to(280.4))
  expect_that(round(mean(na.seadec(x, algorithm = "locf")), digits = 1), is_identical_to(279.7))
  expect_that(round(mean(na.seadec(x, algorithm = "mean")), digits = 1), is_identical_to(279.5))
  expect_that(round(mean(na.seadec(x, algorithm = "kalman", model = "auto.arima")), digits = 1) > 277 &
    round(mean(na.seadec(x, algorithm = "kalman", model = "auto.arima")), digits = 1) < 283, is_true())
  expect_that(round(mean(na.seadec(x, algorithm = "ma")), digits = 1), is_identical_to(280.6))
})

test_that("Imputation works for data.frame", {
  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
  expect_that(anyNA(na.seadec(x, algorithm = "interpolation")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "locf")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "mean")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "random")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "kalman")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "ma")), is_false())
})


test_that("Error for wrong input for algorithm parameter", {
  expect_that(na.seadec(tsAirgap, algorithm = "wrong"), throws_error())
})



test_that("Test NA at beginning", {
  x <- tsAirgap
  x[1:2] <- NA
  expect_that(anyNA(na.seadec(x, algorithm = "interpolation")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "kalman")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "locf")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "ma")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "mean")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "random")), is_false())
  expect_that(anyNA(na.seadec(x)), is_false())
})

test_that("Test NA at end", {
  x <- tsAirgap
  x[143:144] <- NA
  expect_that(anyNA(na.seadec(x, algorithm = "interpolation")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "kalman")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "locf")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "ma")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "mean")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "random")), is_false())
  expect_that(anyNA(na.seadec(x)), is_false())
})

test_that("Multiple NAs in a row", {
  x <- tsAirgap
  x[40:80] <- NA
  expect_that(anyNA(na.seadec(x, algorithm = "interpolation")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "kalman")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "locf")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "ma")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "mean")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "random")), is_false())
  expect_that(anyNA(na.seadec(x)), is_false())
})

test_that("Over 90% NAs", {
  x <- tsAirgap
  x[10:140] <- NA
  expect_that(anyNA(na.seadec(x, algorithm = "interpolation")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "kalman")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "locf")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "ma")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "mean")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "random")), is_false())
  expect_that(anyNA(na.seadec(x)), is_false())
})
test_that("No Seasonality in series", {
  x <- ts(c(3, 5, 6, 7, 8, 4, 5, 6, NA, NA, 5, 7, 4, 2, NA, NA, 5, 7, 8))
  expect_that(anyNA(na.seadec(x, algorithm = "interpolation")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "kalman")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "locf")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "ma")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "mean")), is_false())
  expect_that(anyNA(na.seadec(x, algorithm = "random")), is_false())
  expect_that(anyNA(na.seadec(x)), is_false())
})

test_that("Handling for no NAs", {
  x <- tsAirgapComplete
  expect_that(anyNA(na.seadec(x)), is_false())
})

# Test that findFrequency works

test_that("Given frequency is not overwritten if findFrequency==T", {

})

test_that("Given frequency is not overwritten if findFrequency==T", {

})



test_that("Correct results for all options with the tsAirgap dataset with find frequency", {
  skip_on_cran()
  # Using mean over resulting vector to check correctness
  # In order to avoid writing down the complete resulting vector
  # Using rounded version in order to avoid writing down all decimals
  x <- as.vector(tsAirgap)
  expect_that(round(mean(na.seadec(x, algorithm = "interpolation", find_frequency = TRUE)), digits = 1), is_identical_to(280.4))
  expect_that(round(mean(na.seadec(x, algorithm = "interpolation", find_frequency = FALSE)), digits = 1), is_identical_to(280.7))
  
  #Test if set frequencys are accepted as expected for find_frequency= F
  expect_that(round(mean(na.seadec(ts(x,frequency = 2), algorithm = "interpolation", find_frequency = FALSE)), digits = 1), is_identical_to(280.8))
  
  #Test if find_frequency works
  expect_that(round(mean(na.seadec(ts(x,frequency = 2), algorithm = "interpolation", find_frequency = TRUE)), digits = 1), is_identical_to(280.4))
  
  
  expect_that(round(mean(na.seadec(x, algorithm = "locf", find_frequency = TRUE)), digits = 1), is_identical_to(279.7))
  expect_that(round(mean(na.seadec(x, algorithm = "locf", find_frequency = FALSE)), digits = 1), is_identical_to(278.8))
  
  expect_that(round(mean(na.seadec(x, algorithm = "mean", find_frequency = TRUE)), digits = 1), is_identical_to(279.5))
  expect_that(round(mean(na.seadec(x, algorithm = "mean", find_frequency = FALSE)), digits = 1), is_identical_to(279.8))
  
  expect_that(round(mean(na.seadec(x, algorithm = "kalman", model = "auto.arima", find_frequency = TRUE)), digits = 1) > 277 &
                round(mean(na.seadec(x, algorithm = "kalman", model = "auto.arima", find_frequency = TRUE)), digits = 1) < 283, is_true())
  
  expect_that(round(mean(na.seadec(x, algorithm = "ma", find_frequency = TRUE)), digits = 1), is_identical_to(280.6))
  expect_that(round(mean(na.seadec(x, algorithm = "ma", find_frequency = FALSE)), digits = 1), is_identical_to(281.2))
  
})
