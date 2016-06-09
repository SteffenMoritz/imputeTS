context("na.mean")


test_that("Error for wrong input for option parameter",
          {
            expect_that( na.mean(tsAirgap, option ="Wrong"), throws_error())
          })


test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.mean(x, option ="mean")), is_false())
            expect_that(anyNA(na.mean(x, option ="mode")), is_false())
            expect_that(anyNA(na.mean(x, option ="median")), is_false())
            expect_that(anyNA(na.mean(x)), is_false())
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.mean(x, option ="mean")), is_false())
            expect_that(anyNA(na.mean(x, option ="mode")), is_false())
            expect_that(anyNA(na.mean(x, option ="median")), is_false())
            expect_that(anyNA(na.mean(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.mean(x, option ="mean")), is_false())
            expect_that(anyNA(na.mean(x, option ="mode")), is_false())
            expect_that(anyNA(na.mean(x, option ="median")), is_false())
            expect_that(anyNA(na.mean(x)), is_false())
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.mean(x, option ="mean")), is_false())
            expect_that(anyNA(na.mean(x, option ="mode")), is_false())
            expect_that(anyNA(na.mean(x, option ="median")), is_false())
            expect_that(anyNA(na.mean(x)), is_false())
            
          })