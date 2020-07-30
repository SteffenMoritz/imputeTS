context("apply_base_algorithm")

test_that("Error for wrong algorithm choice", {
  expect_error(
    apply_base_algorithm(tsAirgap, algorithm = "wrongAlgorithm")
  )
})
