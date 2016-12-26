context("na.locf")

test_that("Error for wrong input for option parameter",
          {
            expect_that( na.locf(tsAirgap, option="wrongOption"), throws_error())
          })

test_that("Error for wrong input for na.remaining parameter",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that( na.locf(x, na.remaining ="Wrong"), throws_error())
          })


test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "rev")), is_false())
            expect_that(length(na.locf(x, option="nocb", na.remaining = "rm")), equals(144))
            expect_that(length(na.locf(x, option="locf", na.remaining = "rm")), equals(142))
            expect_that(length(na.locf(x, option="nocb", na.remaining = "keep")), equals(144))
            
            expect_that(anyNA(na.locf(x)), is_false())
            
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "rev")), is_false())
            expect_that(length(na.locf(x, option="nocb", na.remaining = "rm")), equals(142))
            expect_that(length(na.locf(x, option="locf", na.remaining = "rm")), equals(144))
            expect_that(anyNA(na.locf(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "rev")), is_false())
            expect_that(length(na.locf(x, option="nocb", na.remaining = "rm")), equals(144))
            expect_that(length(na.locf(x, option="locf", na.remaining = "rm")), equals(144))
            expect_that(anyNA(na.locf(x)), is_false())
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x)), is_false())
            
          })