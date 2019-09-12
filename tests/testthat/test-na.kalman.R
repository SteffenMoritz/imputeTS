context("na.kalman")

test_that("All NA vector throws error",
          {
            expect_error(na.kalman(c(NA,NA,NA,NA,NA)))
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            skip_on_cran()
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = T )), digits = 1),  is_identical_to(280.3) )
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = F )), digits = 1),  is_identical_to(279.2) )
           
             })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            skip_on_cran()
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = T )), digits = 1),  is_identical_to(284.8) )
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = F )), digits = 1),  is_identical_to(291.6) )
             })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            skip_on_cran()
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = T )), digits = 1),  is_identical_to(280.2) )
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = F )), digits = 1),  is_identical_to(279.8) )
           })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na.kalman(x, model="auto.arima", smooth = T )), is_false())
            expect_that(anyNA(na.kalman(x, model="auto.arima", smooth = F)), is_false())
           })

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