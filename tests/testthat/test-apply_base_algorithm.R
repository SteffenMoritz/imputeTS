context("apply_base_algorithm")

test_that("Error for wrong algorithm choice",
          {
            expect_that(
              apply_base_algorithm(tsAirgap, algorithm = "wrongAlgorithm"), throws_error())
          })