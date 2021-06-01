context("apply_base_algorithm")

test_that("Warning for wrong algorithm choice", {
  expect_warning(
    apply_base_algorithm(tsAirgap, algorithm = "wrongAlgorithm")
  )
})
