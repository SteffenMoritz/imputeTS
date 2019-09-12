context("na_random")

test_that("All NA vector throws error",
          {
            expect_error(na_random(c(NA,NA,NA,NA,NA)))
          })

test_that("Wrong input",
          {
            x <- data.frame(tsAirgap)
            expect_that(na_random(x, lower_bound = 1, upper_bound = -1), throws_error())
            
            x <- rep("string",144)
            x[3] <- NA
            expect_that(na_random(x), throws_error())
            
            x <- rep(NA,144)
            expect_that(na_random(x), throws_error())
            
          })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na_random(x)), is_false())
          })


test_that("Error for lower_bound > upper_bound",
          {
            expect_that( na_random(tsAirgap, lower_bound = 300, upper_bound = 100 ), throws_error())
          })




test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na_random(x, lower_bound = 100, upper_bound = 200 )), is_false())
            expect_that(anyNA(na_random(x, lower_bound = -10, upper_bound = 5000 )), is_false())
            expect_that(anyNA(na_random(x)), is_false())
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na_random(x, lower_bound = 100, upper_bound = 200 )), is_false())
            expect_that(anyNA(na_random(x, lower_bound = -10, upper_bound = 5000 )), is_false())
            expect_that(anyNA(na_random(x)), is_false())
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na_random(x, lower_bound = 100, upper_bound = 200 )), is_false())
            expect_that(anyNA(na_random(x, lower_bound = -10, upper_bound = 5000 )), is_false())
            expect_that(anyNA(na_random(x)), is_false())
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na_random(x, lower_bound = 100, upper_bound = 200 )), is_false())
            expect_that(anyNA(na_random(x, lower_bound = -10, upper_bound = 5000 )), is_false())
            expect_that(anyNA(na_random(x)), is_false())
          })