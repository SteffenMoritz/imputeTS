context("plotNA_gapsize")

test_that("Check that plot is running without error",
          {
            expect_that( is.recursive(plotNA_gapsize(tsAirgap)), is_true())
            expect_that( is.list(plotNA_gapsize(tsAirgap)), is_true())
            expect_that( is.list(plotNA_gapsize(tsAirgap, byTotalNA = T)), is_true())
            expect_that( is.list(plotNA_gapsize(tsNH4, limit = 2)), is_true())
            expect_that( is.list(plotNA_gapsize(tsAirgap, beside = F)), is_true())
            expect_that( is.list(plotNA_gapsize(tsAirgap, horiz = T)), is_true())
            expect_that( is.list(plotNA_gapsize(tsAirgap, axes = F)), is_true())
            expect_that( is.list(plotNA.gapsize(tsAirgap)), is_true())
            expect_warning( plotNA.gapsize(tsAirgap))
            
            ## input not univariate
            x <- data.frame(
              x = runif(10,0,10),
              y = runif(10,0,10)
            )
            expect_error(plotNA_gapsize(x))
            
            ## input not numeric
            x <- c("a", 1, 2, 3)
            expect_error(plotNA_gapsize(x))
            
            ## No NA values
            x <- 1:10
            expect_error(plotNA_gapsize(x))
            
          })

