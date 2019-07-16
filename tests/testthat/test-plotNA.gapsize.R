context("plotNA.gapsize")

test_that("Check that plot is running without error",
          {
            expect_that( is.recursive(plotNA_gapsize(tsAirgap)), is_true())
          })