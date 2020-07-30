context("Defunct and Depreciated Functions")



test_that("Correct error for old, defunct plotting functions", {
  expect_error(plotNA.distribution(tsAirgap), class = "defunctError")
  expect_error(plotNA.distributionBar(tsAirgap), class = "defunctError")
  expect_error(plotNA.gapsize(tsAirgap), class = "defunctError")
  expect_error(plotNA.imputations(tsAirgap), class = "defunctError")
})


test_that("Correct warning for old, deprieciated imputation functions", {
  expect_warning(na.interpolation(tsAirgap), regexp = "replaced by")
  expect_warning(na.kalman(tsAirgap), regexp = "replaced by")
  expect_warning(na.locf(tsAirgap), regexp = "replaced by")
  expect_warning(na.ma(tsAirgap), regexp = "replaced by")
  expect_warning(na.mean(tsAirgap), regexp = "replaced by")
  expect_warning(na.random(tsAirgap), regexp = "replaced by")
  expect_warning(na.remove(tsAirgap), regexp = "replaced by")
  expect_warning(na.replace(tsAirgap), regexp = "replaced by")
  expect_warning(na.seadec(tsAirgap), regexp = "replaced by")
  expect_warning(na.seasplit(tsAirgap), regexp = "replaced by")
})
