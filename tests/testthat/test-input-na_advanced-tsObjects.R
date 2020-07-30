context("Advanced Time Series Objects Input")


test_that("tsibble objects", {
  skip_on_cran()
  if (!requireNamespace("tsibble", quietly = TRUE)) {
    warning("Pkg tsibble needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("tsibble")
    x <- as_tsibble(tsAirgap)
    expect_false(anyNA(na_mean(x)))
    expect_false(anyNA(na_kalman(x)))
    expect_false(anyNA(na_interpolation(x)))
    expect_false(anyNA(na_locf(x)))
    expect_false(anyNA(na_ma(x)))
    expect_false(anyNA(na_random(x)))
    expect_false(anyNA(na_seadec(x)))
    expect_false(anyNA(na_seasplit(x)))
    expect_false(anyNA(na_replace(x)))
  }
})

test_that("Multivariate tsibble ", {
  skip_on_cran()
  if (!requireNamespace("tsibble", quietly = TRUE)) {
    warning("Pkg tsibble needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("tsibble")
    x <- data.frame(tsAirgap, tsAirgap)
    x <- as.ts(x)
    x <- as_tsibble(x)
    expect_false(anyNA(na_mean(x)))
    expect_false(anyNA(na_kalman(x)))
    expect_false(anyNA(na_interpolation(x)))
    expect_false(anyNA(na_locf(x)))
    expect_false(anyNA(na_ma(x)))
    expect_false(anyNA(na_random(x)))
    expect_false(anyNA(na_seadec(x)))
    expect_false(anyNA(na_seasplit(x)))
    expect_false(anyNA(na_replace(x)))
  }
})

test_that("tibble objects", {
  skip_on_cran()
  if (!requireNamespace("tibble", quietly = TRUE)) {
    warning("Pkg tibble needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("tibble")
    x <- as_tibble(tsAirgap)
    expect_false(anyNA(na_mean(x)))
    expect_false(anyNA(na_kalman(x)))
    expect_false(anyNA(na_interpolation(x)))
    expect_false(anyNA(na_locf(x)))
    expect_false(anyNA(na_ma(x)))
    expect_false(anyNA(na_random(x)))
    expect_false(anyNA(na_seadec(x)))
    expect_false(anyNA(na_seasplit(x)))
    expect_false(anyNA(na_replace(x)))
  }
})


test_that("multivariate tibble objects", {
  skip_on_cran()
  if (!requireNamespace("tibble", quietly = TRUE)) {
    warning("Pkg tibble needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("tibble")

    x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
    z <- as_tibble(x)

    expect_false(anyNA(na_mean(z)))
    expect_false(anyNA(na_kalman(z)))
    expect_false(anyNA(na_interpolation(z)))
    expect_false(anyNA(na_locf(z)))
    expect_false(anyNA(na_ma(z)))
    expect_false(anyNA(na_random(z)))
    expect_false(anyNA(na_seadec(z)))
    expect_false(anyNA(na_seasplit(z)))
    expect_false(anyNA(na_replace(z)))
  }
})


test_that("multivariate tibble objects - non-numeric input", {
  skip_on_cran()
  if (!requireNamespace("tibble", quietly = TRUE)) {
    warning("Pkg tibble needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("tibble")

    x <- data.frame(rep("x", 144), tsAirgap, tsAirgap, rep("qq", 144), tsAirgapComplete)
    z <- as_tibble(x)

    expect_false(anyNA(na_mean(z)))
    expect_false(anyNA(na_kalman(z)))
    expect_false(anyNA(na_interpolation(z)))
    expect_false(anyNA(na_locf(z)))
    expect_false(anyNA(na_ma(z)))
    expect_false(anyNA(na_random(z)))
    expect_false(anyNA(na_seadec(z)))
    expect_false(anyNA(na_seasplit(z)))
    expect_false(anyNA(na_replace(z)))
  }
})



test_that("zoo objects", {
  skip_on_cran()
  if (!requireNamespace("zoo", quietly = TRUE)) {
    warning("Pkg zoo needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("zoo")
    x <- as.zoo(tsAirgap)
    expect_false(anyNA(na_mean(x)))
    expect_false(anyNA(na_kalman(x)))
    expect_false(anyNA(na_interpolation(x)))
    expect_false(anyNA(na_locf(x)))
    expect_false(anyNA(na_ma(x)))
    expect_false(anyNA(na_random(x)))
    expect_false(anyNA(na_seadec(x)))
    expect_false(anyNA(na_seasplit(x)))
    expect_false(anyNA(na_replace(x)))
  }
})

test_that("multivariate zoo objects", {
  skip_on_cran()
  if (!requireNamespace("zoo", quietly = TRUE)) {
    warning("Pkg zoo needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("zoo")
    require("stats")
    time <- base::seq(
      from = zoo::as.Date(zoo::as.yearmon(stats::start(tsAirgap)))[1], by = "month",
      length.out = length(tsAirgap)
    )
    x <- data.frame(time, zoo::coredata(tsAirgap), zoo::coredata(tsAirgap), zoo::coredata(tsAirgapComplete))

    z <- zoo::read.zoo(x, format = "%Y-%m-%d")

    expect_false(anyNA(na_mean(z)))
    expect_false(anyNA(na_kalman(z)))
    expect_false(anyNA(na_interpolation(z)))
    expect_false(anyNA(na_locf(z)))
    expect_false(anyNA(na_ma(z)))
    expect_false(anyNA(na_random(z)))
    expect_false(anyNA(na_seadec(z)))
    expect_false(anyNA(na_seasplit(z)))
    expect_false(anyNA(na_replace(z)))
  }
})


test_that("xts objects", {
  skip_on_cran()
  if (!requireNamespace("xts", quietly = TRUE)) {
    warning("Pkg xts needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("xts")
    x <- as.xts(tsAirgap)
    expect_false(anyNA(na_mean(x)))
    expect_false(anyNA(na_kalman(x)))
    expect_false(anyNA(na_interpolation(x)))
    expect_false(anyNA(na_locf(x)))
    expect_false(anyNA(na_ma(x)))
    expect_false(anyNA(na_random(x)))
    expect_false(anyNA(na_seadec(x)))
    expect_false(anyNA(na_seasplit(x)))
    expect_false(anyNA(na_replace(x)))
  }
})


test_that("timeSeries objects", {
  skip_on_cran()

  if (!requireNamespace("timeSeries", quietly = TRUE)) {
    warning("Pkg timeSeries needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("timeSeries")
    x <- as.timeSeries(tsAirgap)
    expect_false(anyNA(na_mean(x)))
    expect_false(anyNA(na_kalman(x)))
    expect_false(anyNA(na_interpolation(x)))
    expect_false(anyNA(na_locf(x)))
    expect_false(anyNA(na_ma(x)))
    expect_false(anyNA(na_random(x)))
    expect_false(anyNA(na_seadec(x)))
    expect_false(anyNA(na_seasplit(x)))
    expect_false(anyNA(na_replace(x)))
  }
})



test_that("tis objects", {
  skip_on_cran()

  if (!requireNamespace("tis", quietly = TRUE)) {
    warning("Pkg timeSeries needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("tis")
    x <- as.tis(tsAirgap)
    expect_false(anyNA(na_mean(x)))
    expect_false(anyNA(na_kalman(x)))
    expect_false(anyNA(na_interpolation(x)))
    expect_false(anyNA(na_locf(x)))
    expect_false(anyNA(na_ma(x)))
    expect_false(anyNA(na_random(x)))
    expect_false(anyNA(na_seadec(x)))
    #     expect_false(anyNA(na_seasplit(x)))
    expect_false(anyNA(na_replace(x)))
  }
})

test_that("Imputation works for mts", {
  skip_on_cran()

  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
  x <- ts(x)
  expect_false(anyNA(na_mean(x)))
  expect_false(anyNA(na_kalman(x)))
  expect_false(anyNA(na_interpolation(x)))
  expect_false(anyNA(na_locf(x)))
  expect_false(anyNA(na_ma(x)))
  expect_false(anyNA(na_random(x)))
  expect_false(anyNA(na_seadec(x)))
  expect_false(anyNA(na_seasplit(x)))
  expect_false(anyNA(na_replace(x)))
})

test_that("Imputation works for data.frame", {
  skip_on_cran()

  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
  expect_false(anyNA(na_mean(x)))
  expect_false(anyNA(na_kalman(x)))
  expect_false(anyNA(na_interpolation(x)))
  expect_false(anyNA(na_locf(x)))
  expect_false(anyNA(na_ma(x)))
  expect_false(anyNA(na_random(x)))
  expect_false(anyNA(na_seadec(x)))
  expect_false(anyNA(na_seasplit(x)))
  expect_false(anyNA(na_replace(x)))
})

test_that("Imputation works for data.frame univariate", {
  skip_on_cran()

  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap)
  expect_false(anyNA(na_mean(x)))
  expect_false(anyNA(na_kalman(x)))
  expect_false(anyNA(na_interpolation(x)))
  expect_false(anyNA(na_locf(x)))
  expect_false(anyNA(na_ma(x)))
  expect_false(anyNA(na_random(x)))
  expect_false(anyNA(na_seadec(x)))
  expect_false(anyNA(na_seasplit(x)))
  expect_false(anyNA(na_replace(x)))
})


test_that("Imputation works for matrix", {
  skip_on_cran()

  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
  x <- as.matrix(x)
  expect_false(anyNA(na_mean(x)))
  expect_false(anyNA(na_kalman(x)))
  expect_false(anyNA(na_interpolation(x)))
  expect_false(anyNA(na_locf(x)))
  expect_false(anyNA(na_ma(x)))
  expect_false(anyNA(na_random(x)))
  expect_false(anyNA(na_seadec(x)))
  expect_false(anyNA(na_seasplit(x)))
  expect_false(anyNA(na_replace(x)))
})


test_that("Imputation works for matrix univariate", {
  skip_on_cran()

  # Checking if NAs remain in data.frame
  x <- data.frame(tsAirgap)
  x <- as.matrix(x)
  expect_false(anyNA(na_mean(x)))
  expect_false(anyNA(na_kalman(x)))
  expect_false(anyNA(na_interpolation(x)))
  expect_false(anyNA(na_locf(x)))
  expect_false(anyNA(na_ma(x)))
  expect_false(anyNA(na_random(x)))
  expect_false(anyNA(na_seadec(x)))
  expect_false(anyNA(na_seasplit(x)))
  expect_false(anyNA(na_replace(x)))
})
