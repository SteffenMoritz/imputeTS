context("na.seadec")


test_that("Error for wrong input for algorithm parameter",
          {
            expect_that( na.seadec(tsAirgap, algorithm = "wrong"), throws_error())
          })



test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.seadec(x, algorithm ="interpolation")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="kalman")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="locf")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="ma")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="mean")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="random")), is_false())
            expect_that(anyNA(na.seadec(x)), is_false())
            
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.seadec(x, algorithm ="interpolation")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="kalman")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="locf")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="ma")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="mean")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="random")), is_false())
            expect_that(anyNA(na.seadec(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.seadec(x, algorithm ="interpolation")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="kalman")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="locf")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="ma")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="mean")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="random")), is_false())
            expect_that(anyNA(na.seadec(x)), is_false())
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.seadec(x, algorithm ="interpolation")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="kalman")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="locf")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="ma")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="mean")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="random")), is_false())
            expect_that(anyNA(na.seadec(x)), is_false())
            
          })
test_that("No Seasonality in series",
          {
            x <- ts(c(3,5,6,7,8,4,5,6,NA,NA,5,7,4,2,NA,NA,5,7,8))
            expect_that(anyNA(na.seadec(x, algorithm ="interpolation")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="kalman")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="locf")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="ma")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="mean")), is_false())
            expect_that(anyNA(na.seadec(x, algorithm ="random")), is_false())
            expect_that(anyNA(na.seadec(x)), is_false())
            
          })

test_that("Handling for no NAs",
          {
            x <- tsAirgapComplete
            expect_that(anyNA(na.seadec(x)), is_false())
            
          })