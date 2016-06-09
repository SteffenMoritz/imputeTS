context("na.replace")


test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.replace(x, fill = -5)), is_false())
            expect_that(anyNA(na.replace(x, fill = 1000)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
            
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.replace(x, fill = -5)), is_false())
            expect_that(anyNA(na.replace(x, fill = 1000)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.replace(x, fill = -5)), is_false())
            expect_that(anyNA(na.replace(x, fill = 1000)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.replace(x, fill = -5)), is_false())
            expect_that(anyNA(na.replace(x, fill = 1000)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
            
          })

test_that("Handling for no NAs",
          {
            x <- tsAirgapComplete
            expect_that(anyNA(na.replace(x)), is_false())
            
          })