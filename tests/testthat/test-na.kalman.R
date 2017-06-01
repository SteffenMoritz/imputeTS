context("na.kalman")



test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = T )), digits = 1),  is_identical_to(279.9) )
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = F )), digits = 1),  is_identical_to(278.8) )
           
             })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = T )), digits = 1),  is_identical_to(285.1) )
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = F )), digits = 1),  is_identical_to(291.3) )
             })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = T )), digits = 1),  is_identical_to(280.1) )
            expect_that( round(mean(na.kalman( x, model="auto.arima", smooth = F )), digits = 1),  is_identical_to(279.4) )
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