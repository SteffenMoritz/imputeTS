context("ggplot_na_distribution")

test_that("Old functions give error",
          {
            expect_error(plotNA.distribution(tsAirgap))
          })

test_that("Check that plot is running without error",
          {
            
            expect_true( is.recursive(ggplot_na_distribution(tsAirgap)))
            
            ## input not univariate
            x <- data.frame(
              x = runif(10,0,10),
              y = runif(10,0,10)
            )
            expect_error(ggplot_na_distribution(x))
            
            ## input not numeric
            x <- c("a", 1, 2, 3)
            expect_error(ggplot_na_distribution(x))
            
          })



test_that("Plot with x_axis_labels works and yearly data works",
          {
            skip_on_cran()
            
            if ((!requireNamespace(c("zoo"), quietly = TRUE))&&(!requireNamespace(c("ggplot2"), quietly = TRUE)) ) {
              warning("Packages zoo, ggplot2 needed for this test.",
                      call. = FALSE)
            }
            else {
              require("zoo")
              require("ggplot2")
              # Yearly data
              nh <- structure(c(NA, NA, 49.4, 51.1, 49.4, 47.9, 49.8, 50.9, 49.3, 
                                51.9, 50.8, 49.6, 49.3, 50.6, 48.4, 50.7, 50.9, 50.6, 51.5, 52.8, 
                                51.8, 51.1, 49.8, 50.2, 50.4, 51.6, 51.8, 50.9, 48.8, 51.7, 51, 
                                50.6, 51.7, 51.5, 52.1, 51.3, 51, 54, 51.4, 52.7, 53.1, 54.6, 
                                52, 52, 50.9, 52.6, 50.2, 52.6, 51.6, 51.9, 50.5, 50.9, 51.7, 
                                51.4, 51.7, 50.8, 51.9, 51.8, NA, NA), .Tsp = c(1912, 1971, 
                                                                                1), class = "ts")
              
              # Use zoo to change ts time information to yearmon vector
              # Afterwards create Date vector and from this Date vector POSIXct
              nh_yearmon <- zoo::as.yearmon(time(nh))
              nh_date <- zoo::as.Date(nh_yearmon)
              nh_posix <-  as.POSIXct(nh_date)
              
              expect_is(
                ggplot_na_distribution(nh), 
              "ggplot")
              
              expect_is(
                ggplot_na_distribution(nh, x_axis_labels = nh_date),
                "ggplot")
              
              expect_is(
                ggplot_na_distribution(nh, x_axis_labels = nh_posix),
                "ggplot")
              
              expect_is(
                ggplot_na_distribution(nh, x_axis_labels = nh_posix, title = "test"),
                "ggplot")
              
              expect_is(
                ggplot_na_distribution(nh, x_axis_labels = nh_posix, title = "test"),
                "ggplot") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))
            }
          })
