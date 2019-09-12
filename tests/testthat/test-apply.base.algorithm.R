context("apply.base.algorithm")

test_that("Error for wrong algorithm choice",
          {
            expect_that(
              apply.base.algorithm(tsAirgap, algorithm = "wrongAlgorithm"), throws_error())
          })