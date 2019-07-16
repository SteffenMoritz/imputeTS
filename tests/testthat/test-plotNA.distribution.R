context("plotNA.distribution")

test_that("Check that plot is running without error",
          {
            expect_that( is.recursive(plotNA_distribution(tsAirgap)), is_true())
          })