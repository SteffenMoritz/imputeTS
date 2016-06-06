context("na.mean")

## TODO: Rename context
## TODO: Add more tests

test_that("multiplication works", {
  expect_equal(na.mean(tsAirgap)[1], 112)
})

context("Tests on input")

test_that("tests for a non-forecast object",
{
  expect_that(
    na.mean("someString"), throws_error())
})