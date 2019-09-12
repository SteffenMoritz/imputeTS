context("na.ma")

test_that("All NA vector throws error",
          {
            expect_error(na.ma(c(NA,NA,NA,NA,NA)))
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na.ma( x, weighting="simple", k = 4 )), digits = 1),  is_identical_to(275.2) )
            expect_that( round(mean(na.ma( x, weighting="simple", k = 20 )), digits = 1),  is_identical_to(276.1) )
            expect_that( round(mean(na.ma( x, weighting="linear", k = 4 )), digits = 1),  is_identical_to(275.0) )
            expect_that( round(mean(na.ma( x, weighting="linear", k = 20 )), digits = 1),  is_identical_to(276.1) )
            expect_that( round(mean(na.ma( x, weighting="exponential", k = 4 )), digits = 1),  is_identical_to(274.6) )
            expect_that( round(mean(na.ma( x, weighting="exponential", k = 20 )), digits = 1),  is_identical_to(274.7) )
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na.ma( x, weighting="simple", k = 4 )), digits = 1),  is_identical_to(282.1) )
            expect_that( round(mean(na.ma( x, weighting="simple", k = 20 )), digits = 1),  is_identical_to(281.1) )
            expect_that( round(mean(na.ma( x, weighting="linear", k = 4 )), digits = 1),  is_identical_to(281.9) )
            expect_that( round(mean(na.ma( x, weighting="linear", k = 20 )), digits = 1),  is_identical_to(281.3) )
            expect_that( round(mean(na.ma( x, weighting="exponential", k = 4 )), digits = 1),  is_identical_to(281.7) )
            expect_that( round(mean(na.ma( x, weighting="exponential", k = 20 )), digits = 1),  is_identical_to(281.6) )
          })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na.ma( x, weighting="simple", k = 4 )), digits = 1),  is_identical_to(281.5) )
            expect_that( round(mean(na.ma( x, weighting="simple", k = 20 )), digits = 1),  is_identical_to(280.7) )
            expect_that( round(mean(na.ma( x, weighting="linear", k = 4 )), digits = 1),  is_identical_to(281.3) )
            expect_that( round(mean(na.ma( x, weighting="linear", k = 20 )), digits = 1),  is_identical_to(280.9) )
            expect_that( round(mean(na.ma( x, weighting="exponential", k = 4 )), digits = 1),  is_identical_to(281.2) )
            expect_that( round(mean(na.ma( x, weighting="exponential", k = 20 )), digits = 1),  is_identical_to(281.1) )
          })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na.ma(x, weighting="simple", k = 4  )), is_false())
            expect_that(anyNA(na.ma(x, weighting="simple", k = 20)), is_false())
            expect_that(anyNA(na.ma(x, weighting="linear", k = 4  )), is_false())
            expect_that(anyNA(na.ma(x, weighting="linear", k = 20)), is_false())
            expect_that(anyNA(na.ma(x, weighting="exponential", k = 4 )), is_false())
            expect_that(anyNA(na.ma(x, weighting="exponential", k = 20)), is_false())
          })


test_that("Error for wrong input for k parameter",
          {
            expect_that( na.ma(tsAirgap, k = -1), throws_error())
          })

test_that("Error for wrong input for weighting parameter",
          {
            expect_that( na.ma(tsAirgap, weighting ="Wrong"), throws_error())
          })


test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.ma(x, k = 4, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.ma(x, k = 4, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.ma(x, k = 4, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.ma(x, k = 4, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "simple")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "linear")), is_false())
            expect_that(anyNA(na.ma(x, k = 4, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 1, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x, k = 20, weighting = "exponential")), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            
          })