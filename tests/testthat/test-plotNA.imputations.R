context("plotNA.imputations")

test_that("Check that plot is running without error",
          {
            expect_that( is.recursive(plotNA_imputations(tsAirgap,tsAirgapComplete)), is_true())
            expect_that( is.recursive(plotNA_imputations(tsAirgap,na_mean(tsAirgap), tsAirgapComplete)), is_true())
            
          })