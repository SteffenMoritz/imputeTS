context("na.kalman")

test_that("Error for wrong input for model parameter",
          {
            expect_that( na.kalman(tsAirgap, model="wrongModel"), throws_error())
          })

test_that("Error for wrong input for smooth parameter",
          {
            expect_that( na.kalman(tsAirgap, smooth="Wrong"), throws_error())
          })


test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.kalman(x, model="auto.arima", smooth = T)), is_false())
            expect_that(anyNA(na.kalman(x, model="auto.arima", smooth = F)), is_false())
            expect_that(anyNA(na.kalman(x, model="StructTS", smooth = T)), is_false())
            expect_that(anyNA(na.kalman(x, model="StructTS", smooth = F)), is_false())
            expect_that(anyNA(na.kalman(x)), is_false())
            
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.kalman(x, model="auto.arima", smooth = T)), is_false())
            expect_that(anyNA(na.kalman(x, model="auto.arima", smooth = F)), is_false())
            expect_that(anyNA(na.kalman(x, model="StructTS", smooth = T)), is_false())
            expect_that(anyNA(na.kalman(x, model="StructTS", smooth = F)), is_false())
            expect_that(anyNA(na.kalman(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.kalman(x, model="auto.arima", smooth = T)), is_false())
            expect_that(anyNA(na.kalman(x, model="auto.arima", smooth = F)), is_false())
            expect_that(anyNA(na.kalman(x, model="StructTS", smooth = T)), is_false())
            expect_that(anyNA(na.kalman(x, model="StructTS", smooth = F)), is_false()) 
            expect_that(anyNA(na.kalman(x)), is_false())
            
          })

test_that("Over 50% NAs",
          {
            x <- tsAirgap
            x[30:100] <- NA
            expect_that(anyNA(na.kalman(x, model="auto.arima", smooth = T)), is_false())
            expect_that(anyNA(na.kalman(x, model="auto.arima", smooth = F)), is_false())
            expect_that(anyNA(na.kalman(x, model="StructTS", smooth = T)), is_false())
            expect_that(anyNA(na.kalman(x, model="StructTS", smooth = F)), is_false()) 
            expect_that(anyNA(na.kalman(x)), is_false())
            
          })