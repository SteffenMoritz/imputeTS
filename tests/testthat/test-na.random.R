context("na.random")


test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na.random(x)), is_false())
          })


test_that("Error for lowerBound > UpperBound",
          {
            expect_that( na.random(tsAirgap, lowerBound = 300, upperBound = 100 ), throws_error())
          })




test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.random(x, lowerBound = 100, upperBound = 200 )), is_false())
            expect_that(anyNA(na.random(x, lowerBound = -10, upperBound = 5000 )), is_false())
            expect_that(anyNA(na.random(x)), is_false())
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.random(x, lowerBound = 100, upperBound = 200 )), is_false())
            expect_that(anyNA(na.random(x, lowerBound = -10, upperBound = 5000 )), is_false())
            expect_that(anyNA(na.random(x)), is_false())
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.random(x, lowerBound = 100, upperBound = 200 )), is_false())
            expect_that(anyNA(na.random(x, lowerBound = -10, upperBound = 5000 )), is_false())
            expect_that(anyNA(na.random(x)), is_false())
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.random(x, lowerBound = 100, upperBound = 200 )), is_false())
            expect_that(anyNA(na.random(x, lowerBound = -10, upperBound = 5000 )), is_false())
            expect_that(anyNA(na.random(x)), is_false())
          })