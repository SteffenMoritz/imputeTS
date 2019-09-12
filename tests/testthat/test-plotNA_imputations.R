context("plotNA_imputations")

test_that("Check that plot is running without error",
          {

            expect_that( is.list(plotNA_imputations(tsAirgap,tsAirgapComplete)), is_true())
            expect_that( is.list(plotNA.imputations(tsAirgap,tsAirgapComplete)), is_true())
            expect_warning( plotNA.imputations(tsAirgap,tsAirgapComplete))
            expect_that( is.list(plotNA_imputations(tsAirgap,na_mean(tsAirgap), tsAirgapComplete)), is_true())
            expect_that( is.recursive(plotNA_imputations(tsAirgap,tsAirgapComplete)), is_true())
            expect_that( is.recursive(plotNA_imputations(tsAirgap,na_mean(tsAirgap), tsAirgapComplete)), is_true())

            
            ## input not univariate
            x <- data.frame(
              x = runif(10,0,10),
              y = runif(10,0,10)
            )
            expect_error(plotNA_imputations(x, tsAirgapComplete))
            expect_error(plotNA_imputations(tsAirgapComplete, x))
            expect_error(plotNA_imputations(tsAirgap, tsAirgapComplete, x))
            
            ## input not numeric
            x <- c("a", 1, 2, 3)
            expect_error(plotNA_imputations(x, tsAirgapComplete))
            expect_error(plotNA_imputations(tsAirgapComplete, x))
            expect_error(plotNA_imputations(tsAirgap, tsAirgapComplete, x))
            
          })
