context("ggplot_na_imputations")

test_that("Old functions give error", {
  imp_mean <- na_mean(tsAirgap)
  expect_error(plotNA.imputations(tsAirgap, imp_mean))
})

test_that("Check that all parameters of plot run without error", {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Pkg ggplot2 needed for this test.",
      call. = FALSE
    )
  }

  {
    require("ggplot2")
    imp_mean <- na_mean(tsAirgap)
    expect_true(is.recursive(ggplot_na_imputations(tsAirgap, imp_mean)))
    expect_true(is.recursive(ggplot_na_imputations(tsAirgap, imp_mean, tsAirgapComplete)))

    expect_true(is.list(ggplot_na_imputations(
      x_with_na = tsAirgap,
      x_with_imputations = imp_mean
    )))


    expect_true(is.list(ggplot_na_imputations(
      x_with_na = tsAirgap,
      x_with_imputations = imp_mean,
      x_with_truth = tsAirgapComplete,
      title = "test",
      subtitle = "test",
      xlab = "test",
      ylab = "test"
    )))

    expect_true(is.list(ggplot_na_imputations(
      x_with_na = tsAirgap,
      x_with_imputations = imp_mean,
      x_with_truth = tsAirgapComplete,
      color_points = "gold",
      color_imputations = "blue",
      color_truth = "yellow"
    )))

    expect_true(is.list(ggplot_na_imputations(
      x_with_na = tsAirgap,
      x_with_imputations = imp_mean,
      x_with_truth = tsAirgapComplete,
      shape_points = 15,
      shape_imputations = 15,
      shape_truth = 15
    )))

    expect_true(is.list(ggplot_na_imputations(
      x_with_na = tsAirgap,
      x_with_imputations = imp_mean,
      x_with_truth = tsAirgapComplete,
      size_points = 2,
      size_imputations = 2.2,
      size_truth = 2
    )))


    expect_true(is.list(ggplot_na_imputations(
      x_with_na = tsAirgap,
      x_with_imputations = imp_mean,
      x_with_truth = tsAirgapComplete,
      size_lines = 0.6,
      linetype = "dotted"
    )))

    expect_true(is.list(ggplot_na_imputations(
      x_with_na = tsAirgap,
      x_with_imputations = imp_mean,
      x_with_truth = tsAirgapComplete,
      connect_na = FALSE,
      legend = FALSE
    )))

    expect_true(is.list(ggplot_na_imputations(
      x_with_na = tsAirgap,
      x_with_imputations = imp_mean,
      x_with_truth = tsAirgapComplete,
      legend_size = 6,
      label_known = "known",
      label_imputations = "imputed",
      label_truth = "truth",
      theme = ggplot2::theme_classic()
    )))
  }
})


test_that("Errors for wrong input", {


  ## input not univariate
  x <- data.frame(
    x = runif(10, 0, 10),
    y = runif(10, 0, 10)
  )
  expect_error(ggplot_na_imputations(x, tsAirgapComplete))
  expect_error(ggplot_na_imputations(tsAirgapComplete, x))
  expect_error(ggplot_na_imputations(tsAirgap, tsAirgapComplete, x))

  ## input not numeric
  x <- c("a", 1, 2, 3)
  expect_error(ggplot_na_imputations(x, tsAirgapComplete))
  expect_error(ggplot_na_imputations(tsAirgapComplete, x))
  expect_error(ggplot_na_imputations(tsAirgap, tsAirgapComplete, x))

  # input only NA
  all_na <- as.numeric(rep(NA, 144))
  expect_error(ggplot_na_imputations(all_na, tsAirgapComplete))
  expect_error(ggplot_na_imputations(tsAirgapComplete, all_na))

  # Input datasets do not belong together / length of input data different

  ## No NA values for x_with_na
  x <- 1:144
  expect_error(ggplot_na_imputations(x, tsAirgapComplete))
})

test_that("Works with tsNH4", {
  expect_is(
    ggplot_na_imputations(tsNH4, na_mean(tsNH4), tsNH4Complete),
    "ggplot"
  )
})

test_that("Plot with x_axis_labels works and yearly data works", {
  skip_on_cran()
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Pkg ggplot2 needed for this test.",
      call. = FALSE
    )
  }
  else if (!requireNamespace("zoo", quietly = TRUE)) {
    warning("Pkg zoo needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("zoo")
    require("ggplot2")
    # Yearly data
    nh <- structure(c(
      NA, NA, 49.4, 51.1, 49.4, 47.9, 49.8, 50.9, 49.3,
      51.9, 50.8, 49.6, 49.3, 50.6, 48.4, 50.7, 50.9, 50.6, 51.5, 52.8,
      51.8, 51.1, 49.8, 50.2, 50.4, 51.6, 51.8, 50.9, 48.8, 51.7, 51,
      50.6, 51.7, 51.5, 52.1, 51.3, 51, 54, 51.4, 52.7, 53.1, 54.6,
      52, 52, 50.9, 52.6, 50.2, 52.6, 51.6, 51.9, 50.5, 50.9, 51.7,
      51.4, 51.7, 50.8, 51.9, 51.8, NA, NA
    ), .Tsp = c(
      1912, 1971,
      1
    ), class = "ts")

    # Use zoo to change ts time information to yearmon vector
    # Afterwards create Date vector and from this Date vector POSIXct
    nh_yearmon <- zoo::as.yearmon(time(nh))
    nh_date <- zoo::as.Date(nh_yearmon)
    nh_posix <- as.POSIXct(nh_date)

    expect_is(
      ggplot_na_imputations(nh, na_mean(nh)),
      "ggplot"
    )

    expect_is(
      ggplot_na_imputations(nh, na_mean(nh), x_axis_labels = nh_date),
      "ggplot"
    )

    expect_is(
      ggplot_na_imputations(nh, na_mean(nh), x_axis_labels = nh_posix),
      "ggplot"
    )

    expect_is(
      ggplot_na_imputations(nh, na_mean(nh), x_axis_labels = nh_posix, title = "test"),
      "ggplot"
    )

    expect_is(
      ggplot_na_imputations(nh, na_mean(nh), x_axis_labels = nh_posix, title = "test") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)),
      "ggplot"
    )
  }
})




test_that("Non standard input - data.frame, tsibble, tibble, zoo", {
  skip_on_cran()
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Pkg ggplot2 needed for this test.",
      call. = FALSE
    )
  }
  else if (!requireNamespace("zoo", quietly = TRUE)) {
    warning("Pkg zoo needed for this test.",
      call. = FALSE
    )
  }
  else if (!requireNamespace("tibble", quietly = TRUE)) {
    warning("Pkg tibble needed for this test.",
      call. = FALSE
    )
  }
  else if (!requireNamespace("zoo", quietly = TRUE)) {
    warning("Pkg zoo needed for this test.",
      call. = FALSE
    )
  }
  else if (!requireNamespace("tsibble", quietly = TRUE)) {
    warning("Pkg tsibble needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("zoo")
    require("ggplot2")
    require("tibble")
    require("tsibble")
    # data.frame
    tsAirgap_df <- data.frame(tsAirgap)

    expect_is(
      ggplot_na_imputations(tsAirgap_df, na_mean(tsAirgap_df)),
      "ggplot"
    )



    # data.frame - multivariate - supposed to give Error
    tsAirgap_df2 <- data.frame(tsAirgap, tsAirgap)

    expect_error(
      ggplot_na_imputations(tsAirgap_df2, na_mean(tsAirgap_df2))
    )


    # zoo and theme adjustment
    tsAirgap_zoo <- zoo::as.zoo(tsAirgap)

    expect_is(
      ggplot_na_imputations(tsAirgap_zoo, na_mean(tsAirgap_zoo)) + ggplot2::theme_minimal(),
      "ggplot"
    )



    # zoo - multivariate - supposed to give Error
    tsAirgap_zoo2 <- zoo(cbind(tsAirgap, tsAirgap), zoo::as.Date(zoo::as.yearmon(time(tsAirgap))))

    expect_error(
      ggplot_na_imputations(tsAirgap_zoo2, na_mean(tsAirgap_zoo2))
    )




    # tsibble
    tsAirgap_tsibble <- tsibble::as_tsibble(tsAirgap)

    expect_is(
      ggplot_na_imputations(tsAirgap_tsibble, na_mean(tsAirgap_tsibble)),
      "ggplot"
    )



    # tsibble just value, theme adjustment
    tsAirgap_tsibble <- tsibble::as_tsibble(tsAirgap)


    expect_is(
      ggplot_na_imputations(tsAirgap_tsibble$value, na_mean(tsAirgap_tsibble$value)) +
        ggplot2::theme_minimal(),
      "ggplot"
    )



    # tsibble multivariate - plots first non index variable (maybe error would be better)
    tsAirgap_tsibble2 <- tsibble::as_tsibble(tsAirgap)
    tsAirgap_tsibble2$var2 <- tsAirgap
    expect_is(
      ggplot_na_imputations(tsAirgap_tsibble2, na_mean(tsAirgap_tsibble2)),
      "ggplot"
    )



    # tibble
    tsAirgap_tibble <- tibble::as_tibble(tsAirgap)
    expect_is(
      ggplot_na_imputations(tsAirgap_tibble, na_mean(tsAirgap_tibble)),
      "ggplot"
    )



    # tibble multivariate -  plots first variable (maybe error would be better)
    tsAirgap_tibble2 <- tibble::as_tibble(data.frame(tsAirgap, tsAirgap))
    expect_is(
      ggplot_na_imputations(tsAirgap_tibble2, na_mean(tsAirgap_tibble2)),
      "ggplot"
    )
  }
})
