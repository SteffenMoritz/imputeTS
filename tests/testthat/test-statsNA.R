context("statsNA")


test_that("Test that function works and prints output", {
  expect_output(statsNA(tsAirgap, print_only = T))
})


test_that("Test results of function", {
  expect_equal(statsNA(tsAirgap, print_only = F)$number_NAs, 13)
  expect_equal(statsNA(tsAirgap, print_only = F)$longest_na_gap, 3)
  expect_equal(statsNA(tsAirgap, print_only = F)$most_weighty_na_gap, 1)
  expect_equal(statsNA(tsAirgap, print_only = F)$most_frequent_na_gap, 1)
  expect_equal(statsNA(tsAirgap, print_only = F)$length_series, 144)
  expect_equal(statsNA(tsAirgap, print_only = F)$number_na_gaps, 11)
  expect_true(statsNA(tsNH4, print_only = F)$average_size_na_gaps > 5)
})
