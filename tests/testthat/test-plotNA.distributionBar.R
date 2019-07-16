context("plotNA.distributionBar")

test_that("Check that plot is running without error",
          {
            expect_that( is.list(plotNA_distributionBar(tsAirgap)), is_true())
          })