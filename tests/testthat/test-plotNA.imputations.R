context("plotNA.imputations")

test_that("Check that plot is running without error",
          {
            expect_that( is.list(plotNA.imputations(tsAirgap,tsAirgapComplete)), is_true())
            expect_that( is.list(plotNA.imputations(tsAirgap,na.mean(tsAirgap), tsAirgapComplete)), is_true())
            
          })