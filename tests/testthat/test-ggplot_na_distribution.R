context("ggplot_na_distribution")

test_that("Check that plot is running without error",
          {

            expect_that( is.recursive(ggplot_na_distribution(tsAirgap)), is_true())

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

