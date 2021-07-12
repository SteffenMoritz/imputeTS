context("ggplot_na_gapsize2")


test_that("Check that all parameters of plot  run without error", {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Pkg ggplot2 needed for this test.",
      call. = FALSE
    )
  }
  else {
    require("ggplot2")
    expect_true(is.recursive(ggplot_na_gapsize2(tsAirgap)))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4)))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, colors_bubbles = c("#FCFBFF", "#EFEEFA", "#DDDAEF", "#C8C3E2"))))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, color_border = "red")))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, alpha_bubbles = 0.1)))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, labels_bubbles = "gap-occurrence")))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, labels_bubbles = "gap")))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, labels_bubbles = "total")))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, labels_bubbles = "none")))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, labels_bubbles = "occurrence")))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, size_bubbles = 50)))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, size_bubbles = 3)))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, min_totals = 400)))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, min_occurrence = 1000)))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, min_gap = 10)))
    expect_true(is.list(ggplot_na_gapsize2(tsNH4, max_gap = 200)))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, title = "test")))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, subtitle = "test2")))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, legend_breaks  = 4)))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, legend_breaks  = 10)))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, legend_point_sizes  = "default")))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, legend_point_sizes  = "actual")))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, legend_point_sizes  = c(1,2,3,4))))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, legend_title  = "Legend")))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, legend_position  = "left")))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, ylab = "test")))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, xlab = "test")))
    expect_true(is.list(ggplot_na_gapsize2(tsAirgap, theme = ggplot2::theme_classic())))
  }
})

test_that("Errors for wrong input", {

  ## input not univariate
  x <- data.frame(
    x = runif(10, 0, 10),
    y = runif(10, 0, 10)
  )
  expect_error(ggplot_na_gapsize2(x))

  ## input not numeric
  x <- c("a", 1, NA, 3)
  expect_error(ggplot_na_gapsize2(x))

  ## No NA values
  x <- 1:10
  expect_error(ggplot_na_gapsize2(x))


  all_na <- as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA))
  expect_error(ggplot_na_gapsize2(all_na))
  
  
  ## Wrong legend input for custom legend_point_sizes
  expect_error(ggplot_na_gapsize2(tsAirgap, legend_point_sizes = c(1,2)))
  
  
  ## Wrong legend input for custom legend_point_sizes
  expect_error(ggplot_na_gapsize2(tsAirgap, legend_point_sizes = "falsch"))
  
})




test_that("Plot works with test ts", {
  skip_on_cran()
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Pkg ggplot2 needed for this test.",
      call. = FALSE
    )
  }

  else {
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

    expect_is(
      ggplot_na_gapsize2(nh),
      "ggplot"
    )

    expect_is(
      ggplot_na_gapsize2(nh, title = "test"),
      "ggplot"
    )

    expect_is(
      ggplot_na_gapsize2(nh, title = "test") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)),
      "ggplot"
    )
  }
})




test_that(" tsAirgap data works", {
  skip_on_cran()
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Pkg ggplot2 needed for this test.",
      call. = FALSE
    )
  }

  else {
    require("ggplot2")

    expect_is(
      ggplot_na_gapsize2(tsAirgap),
      "ggplot"
    )

    expect_is(
      ggplot_na_gapsize2(tsAirgap) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::ggtitle("hjsdhs"),
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
      ggplot_na_gapsize2(tsAirgap_df),
      "ggplot"
    )



    # data.frame - multivariate - supposed to give Error
    tsAirgap_df2 <- data.frame(tsAirgap, tsAirgap)

    expect_error(
      ggplot_na_gapsize2(tsAirgap_df2)
    )


    # zoo and theme adjustment
    tsAirgap_zoo <- zoo::as.zoo(tsAirgap)

    expect_is(
      ggplot_na_gapsize2(tsAirgap_zoo) + ggplot2::theme_minimal(),
      "ggplot"
    )



    # zoo - multivariate - supposed to give Error
    tsAirgap_zoo2 <- zoo(cbind(tsAirgap, tsAirgap), zoo::as.Date(zoo::as.yearmon(time(tsAirgap))))

    expect_error(
      ggplot_na_gapsize2(tsAirgap_zoo2)
    )




    # tsibble
    tsAirgap_tsibble <- tsibble::as_tsibble(tsAirgap)

    expect_is(
      ggplot_na_gapsize2(tsAirgap_tsibble),
      "ggplot"
    )



    # tsibble just value, theme adjustment
    tsAirgap_tsibble <- tsibble::as_tsibble(tsAirgap)


    expect_is(
      ggplot_na_gapsize2(tsAirgap_tsibble$value) + ggplot2::theme_minimal(),
      "ggplot"
    )



    # tsibble multivariate - plots first non index variable (maybe error would be better)
    tsAirgap_tsibble2 <- tsibble::as_tsibble(tsAirgap)
    tsAirgap_tsibble2$var2 <- tsAirgap
    expect_is(
      ggplot_na_gapsize2(tsAirgap_tsibble2),
      "ggplot"
    )



    # tibble
    tsAirgap_tibble <- tibble::as_tibble(tsAirgap)
    expect_is(
      ggplot_na_gapsize2(tsAirgap_tibble),
      "ggplot"
    )



    # tibble multivariate -  plots first variable (maybe error would be better)
    tsAirgap_tibble2 <- tibble::as_tibble(data.frame(tsAirgap, tsAirgap))
    expect_is(
      ggplot_na_gapsize2(tsAirgap_tibble2),
      "ggplot"
    )
  }
})
