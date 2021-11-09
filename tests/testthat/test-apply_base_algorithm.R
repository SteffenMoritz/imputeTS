context("apply_base_algorithm")

test_that("Warning for wrong algorithm choice", {
  expect_error(
    apply_base_algorithm(tsAirgap, algorithm = "wrongAlgorithm")
  )
})
