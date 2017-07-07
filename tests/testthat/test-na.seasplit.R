context("na.seasplit")



test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            skip_on_cran()
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na.seasplit( x, algorithm = "interpolation" )), digits = 1),  is_identical_to(276.6) )
            expect_that( round(mean(na.seasplit( x, algorithm = "locf" )), digits = 1),  is_identical_to(274.8) )
            expect_that( round(mean(na.seasplit( x, algorithm = "mean" )), digits = 1),  is_identical_to(264.0) )
            expect_that( round(mean(na.seasplit( x, algorithm = "kalman",model = "auto.arima" )), digits = 1),  is_identical_to(275.7) )
            expect_that( round(mean(na.seasplit( x, algorithm = "ma" )), digits = 1),  is_identical_to(275.0) )
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            skip_on_cran()
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na.seasplit( x, algorithm = "interpolation" )), digits = 1),  is_identical_to(280.5) )
            expect_that( round(mean(na.seasplit( x, algorithm = "locf" )), digits = 1),  is_identical_to(278.4) )
            expect_that( round(mean(na.seasplit( x, algorithm = "mean" )), digits = 1),  is_identical_to(283.4) )
            expect_that( round(mean(na.seasplit( x, algorithm = "kalman", model = "auto.arima"  )), digits = 1),  is_identical_to(278.5) )
            expect_that( round(mean(na.seasplit( x, algorithm = "ma" )), digits = 1),  is_identical_to(281.1) )
          })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            skip_on_cran()
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na.seasplit( x, algorithm = "interpolation" )), digits = 1),  is_identical_to(280.3) )
            expect_that( round(mean(na.seasplit( x, algorithm = "locf" )), digits = 1),  is_identical_to(278.2) )
            expect_that( round(mean(na.seasplit( x, algorithm = "mean" )), digits = 1),  is_identical_to(279.3) )
            expect_that( round(mean(na.seasplit( x, algorithm = "kalman", model = "auto.arima"  )), digits = 1),  is_identical_to(279.2) )
            expect_that( round(mean(na.seasplit( x, algorithm = "ma" )), digits = 1),  is_identical_to(280.3) )
          })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na.seasplit( x, algorithm = "interpolation" )), is_false())
            expect_that(anyNA(na.seasplit( x, algorithm = "locf" )), is_false())
            expect_that(anyNA(na.seasplit( x, algorithm = "mean" )), is_false())
            expect_that(anyNA(na.seasplit( x, algorithm = "random" )), is_false())
            expect_that(anyNA(na.seasplit( x, algorithm = "kalman" )), is_false())
            expect_that(anyNA(na.seasplit( x, algorithm = "ma" )), is_false())
          })



test_that("Error for wrong input for algorithm parameter",
          {
            expect_that( na.seasplit(tsAirgap, algorithm = "wrong"), throws_error())
          })



test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.seasplit(x, algorithm ="interpolation")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="kalman")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="locf")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="ma")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="mean")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="random")), is_false())
            expect_that(anyNA(na.seasplit(x)), is_false())
            
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.seasplit(x, algorithm ="interpolation")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="kalman")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="locf")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="ma")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="mean")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="random")), is_false())
            expect_that(anyNA(na.seasplit(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.seasplit(x, algorithm ="interpolation")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="kalman")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="locf")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="ma")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="mean")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="random")), is_false())
            expect_that(anyNA(na.seasplit(x)), is_false())
            
          })

test_that("Over 50% NAs",
          {
            x <- tsAirgap
            x[30:100] <- NA
            expect_that(anyNA(na.seasplit(x, algorithm ="interpolation")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="kalman")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="locf")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="ma")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="mean")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="random")), is_false())
            expect_that(anyNA(na.seasplit(x)), is_false())
            
          })

test_that("No Seasonality in series",
          {
            x <- ts(c(3,5,6,7,8,4,5,6,NA,NA,5,7,4,2,NA,NA,5,7,8))
            expect_that(anyNA(na.seasplit(x, algorithm ="interpolation")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="kalman")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="locf")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="ma")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="mean")), is_false())
            expect_that(anyNA(na.seasplit(x, algorithm ="random")), is_false())
            expect_that(anyNA(na.seasplit(x)), is_false())
            
          })

test_that("Handling for no NAs",
          {
            x <- tsAirgapComplete
            expect_that(anyNA(na.seasplit(x)), is_false())
            
          })