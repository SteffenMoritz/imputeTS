context("na.ma")


test_that("Error for wrong input for k parameter",
          {
            expect_that( na.ma(tsAirgap, k = -1), throws_error())
          })

test_that("Error for wrong input for weighting parameter",
          {
            expect_that( na.ma(tsAirgap, weighting ="Wrong"), throws_error())
          })


test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.ma(x, k = 4, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.ma(x, k = 4, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.ma(x, k = 4, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.ma(x, k = 4, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            
          })