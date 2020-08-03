context("ggplot_na_distribution")

test_that("Old functions give error", {
  expect_error(plotNA.distribution(tsAirgap))
})

test_that("Check that all parameters of plot run without error", {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Pkg ggplot2 needed for this test.",
      call. = FALSE
    )
  }
  {
    require("ggplot2")
    expect_true(is.list(ggplot_na_distribution(tsAirgap)))
    expect_true(is.list(ggplot_na_distribution(tsAirgap,
      color_points = "blue",
      color_lines = "gold",
      color_missing = "darkgreen",
      color_missing_border = "green",
      alpha_missing = 0.1,
      title = "test",
      subtitle = "test",
      xlab = "test",
      ylab = "test",
      shape_points = 15,
      size_points = 2,
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
  expect_error(ggplot_na_distribution(x))

  ## input not numeric
  x <- c("a", 1, 2, 3)
  expect_error(ggplot_na_distribution(x))

  # input only NA
  all_na <- as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA))
  expect_error(ggplot_na_distribution(all_na))
})

test_that("Works with tsNH4", {
  expect_is(
    ggplot_na_distribution(tsNH4),
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
      ggplot_na_distribution(nh),
      "ggplot"
    )

    expect_is(
      ggplot_na_distribution(nh, x_axis_labels = nh_date),
      "ggplot"
    )

    expect_is(
      ggplot_na_distribution(nh, x_axis_labels = nh_posix),
      "ggplot"
    )

    expect_is(
      ggplot_na_distribution(nh, x_axis_labels = nh_posix, title = "test"),
      "ggplot"
    )

    expect_is(
      ggplot_na_distribution(nh, x_axis_labels = nh_posix, title = "test") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)),
      "ggplot"
    )
  }
})




test_that("Plot with x_axis_labels works and tsAirgap data works", {
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

    # Use zoo to change ts time information to yearmon vector
    # Afterwards create Date vector and from this Date vector POSIXct
    airgap_yearmon <- zoo::as.yearmon(time(tsAirgap))
    airgap_date <- zoo::as.Date(airgap_yearmon)
    airgap_posix <- as.POSIXct(airgap_yearmon)

    expect_is(
      ggplot_na_distribution(tsAirgap),
      "ggplot"
    )


    expect_is(
      ggplot_na_distribution(tsAirgap, x_axis_labels = airgap_date),
      "ggplot"
    )


    expect_is(
      ggplot_na_distribution(tsAirgap, x_axis_labels = airgap_posix),
      "ggplot"
    )


    expect_is(
      ggplot_na_distribution(tsAirgap, x_axis_labels = airgap_date) +
        ggplot2::scale_x_date(date_breaks = "6 month", date_labels = "%m-%Y") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
        ggtitle("hjsdhs"),
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
      ggplot_na_distribution(tsAirgap_df),
      "ggplot"
    )



    # data.frame - multivariate - supposed to give Error
    tsAirgap_df2 <- data.frame(tsAirgap, tsAirgap)

    expect_error(
      ggplot_na_distribution(tsAirgap_df2)
    )


    # zoo and theme adjustment
    tsAirgap_zoo <- zoo::as.zoo(tsAirgap)

    expect_is(
      ggplot_na_distribution(tsAirgap_zoo) + ggplot2::theme_minimal(),
      "ggplot"
    )



    # zoo - multivariate - supposed to give Error
    tsAirgap_zoo2 <- zoo(cbind(tsAirgap, tsAirgap), zoo::as.Date(zoo::as.yearmon(time(tsAirgap))))

    expect_error(
      ggplot_na_distribution(tsAirgap_zoo2)
    )




    # tsibble
    tsAirgap_tsibble <- tsibble::as_tsibble(tsAirgap)

    expect_is(
      ggplot_na_distribution(tsAirgap_tsibble),
      "ggplot"
    )



    # tsibble just value, theme adjustment
    tsAirgap_tsibble <- tsibble::as_tsibble(tsAirgap)


    expect_is(
      ggplot_na_distribution(tsAirgap_tsibble$value) + ggplot2::theme_minimal(),
      "ggplot"
    )



    # tsibble multivariate - plots first non index variable (maybe error would be better)
    tsAirgap_tsibble2 <- tsibble::as_tsibble(tsAirgap)
    tsAirgap_tsibble2$var2 <- tsAirgap
    expect_is(
      ggplot_na_distribution(tsAirgap_tsibble2),
      "ggplot"
    )



    # tibble
    tsAirgap_tibble <- tibble::as_tibble(tsAirgap)
    expect_is(
      ggplot_na_distribution(tsAirgap_tibble),
      "ggplot"
    )



    # tibble multivariate -  plots first variable (maybe error would be better)
    tsAirgap_tibble2 <- tibble::as_tibble(data.frame(tsAirgap, tsAirgap))
    expect_is(
      ggplot_na_distribution(tsAirgap_tibble2),
      "ggplot"
    )
  }
})
