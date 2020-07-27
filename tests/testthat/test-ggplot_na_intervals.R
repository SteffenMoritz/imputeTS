context("ggplot_na_intervals")

test_that("Check that plot is running without error",
          {

            expect_true( is.recursive(ggplot_na_intervals(tsAirgap)))
            expect_true( is.list(ggplot_na_intervals(tsAirgap)))
            expect_true( is.list(ggplot_na_intervals(tsAirgap, number_intervals = 5)))
            expect_true( is.list(ggplot_na_intervals(tsAirgap, measure = "count")))
            expect_true( is.list(ggplot_na_intervals(tsAirgap, interval_size = 30)))

            ## input not univariate
            x <- data.frame(
              x = runif(10,0,10),
              y = runif(10,0,10)
            )
            expect_error(ggplot_na_intervals(x))
            
            ## input not numeric
            x <- c("a", 1, 2, 3)
            expect_error(ggplot_na_intervals(x))
            
            
          })
