context("int.preCheck")

test_that("Error for string input",
          {
              x <- tsAirgap
              x[3] <- "SomeString"
              expect_that(precheck(x), throws_error())
          })

test_that("Error for char input",
          {
            x <- tsAirgap
            x[3] <- 'someChars'
            expect_that(precheck(x), throws_error())
          })

test_that("Error for data.frame input",
          {
            x <- data.frame(tsAirgap,tsAirgap)
            expect_that(precheck(x), throws_error())
          })

test_that("Error for null input",
          {
            x <- NULL
            expect_that(precheck(x), throws_error())
          })

test_that("Error for single numeric input",
          {
            x <- 1
            expect_that(precheck(x), throws_error())
          })