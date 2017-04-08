context("na.seadec")


test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na.seadec( x, algorithm = "interpolation" )), digits = 4),  is_identical_to(276.6858) )
            expect_that( round(mean(na.seadec( x, algorithm = "locf" )), digits = 4),  is_identical_to(275.9714) )
            expect_that( round(mean(na.seadec( x, algorithm = "mean" )), digits = 4),  is_identical_to(264.3163) )
            expect_that( round(mean(na.seadec( x, algorithm = "kalman" )), digits = 4),  is_identical_to(278.3177) )
            expect_that( round(mean(na.seadec( x, algorithm = "ma" )), digits = 4),  is_identical_to(277.0747) )
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na.seadec( x, algorithm = "interpolation" )), digits = 4),  is_identical_to(279.9277) )
            expect_that( round(mean(na.seadec( x, algorithm = "locf" )), digits = 4),  is_identical_to(279.1958) )
            expect_that( round(mean(na.seadec( x, algorithm = "mean" )), digits = 4),  is_identical_to(284.1453) )
            expect_that( round(mean(na.seadec( x, algorithm = "kalman" )), digits = 4),  is_identical_to(279.9879) )
            expect_that( round(mean(na.seadec( x, algorithm = "ma" )), digits = 4),  is_identical_to(279.9752) )
          })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na.seadec( x, algorithm = "interpolation" )), digits = 4),  is_identical_to(280.4278) )
            expect_that( round(mean(na.seadec( x, algorithm = "locf" )), digits = 4),  is_identical_to(279.7245) )
            expect_that( round(mean(na.seadec( x, algorithm = "mean" )), digits = 4),  is_identical_to(279.4758) )
            expect_that( round(mean(na.seadec( x, algorithm = "kalman" )), digits = 4),  is_identical_to(280.2776) )
            expect_that( round(mean(na.seadec( x, algorithm = "ma" )), digits = 4),  is_identical_to(280.567) )
          })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na.seadec( x, algorithm = "interpolation" )), is_false())
            expect_that(anyNA(na.seadec( x, algorithm = "locf" )), is_false())
            expect_that(anyNA(na.seadec( x, algorithm = "mean" )), is_false())
            expect_that(anyNA(na.seadec( x, algorithm = "random" )), is_false())
            expect_that(anyNA(na.seadec( x, algorithm = "kalman" )), is_false())
            expect_that(anyNA(na.seadec( x, algorithm = "ma" )), is_false())
          })


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