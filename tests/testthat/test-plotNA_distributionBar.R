context("plotNA_distributionBar")

test_that("Check that plot is running without error",
          {

            expect_that( is.recursive(plotNA_distributionBar(tsAirgap)), is_true())
            expect_that( is.list(plotNA_distributionBar(tsAirgap)), is_true())
            expect_that( is.list(plotNA_distributionBar(tsAirgap, percentage = FALSE)), is_true())
            expect_that( is.list(plotNA_distributionBar(tsAirgap, axes = FALSE)), is_true())
            expect_that( is.list(plotNA.distributionBar(tsAirgap)), is_true())
            expect_warning(plotNA.distributionBar(tsAirgap))
            
            ## input not univariate
            x <- data.frame(
              x = runif(10,0,10),
              y = runif(10,0,10)
            )
            expect_error(plotNA_distributionBar(x))
            
            ## input not numeric
            x <- c("a", 1, 2, 3)
            expect_error(plotNA_distributionBar(x))
            
            
          })
