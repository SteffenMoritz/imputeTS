context("plotNA_distribution")

test_that("Check that plot is running without error",
          {

            expect_that( is.recursive(plotNA_distribution(tsAirgap)), is_true())
            expect_warning(plotNA.distribution(tsAirgap))
            
            ## input not univariate
            x <- data.frame(
              x = runif(10,0,10),
              y = runif(10,0,10)
            )
            expect_error(plotNA_distribution(x))
            
            ## input not numeric
            x <- c("a", 1, 2, 3)
            expect_error(plotNA_distribution(x))
            
          })

