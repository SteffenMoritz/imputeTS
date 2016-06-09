context("plotNA.distribution")

test_that("Check that plot is running without error",
          {
            expect_that( is.atomic(plotNA.distribution(tsAirgap)), is_true())
          })