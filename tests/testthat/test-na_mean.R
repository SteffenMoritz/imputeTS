context("na_mean")
            
test_that("All NA vector throws error",
          {
            expect_error(na_mean(c(NA,NA,NA,NA,NA)))
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na_mean( x, option="median" )), digits = 1),  is_identical_to(260.0) )
            expect_that( round(mean(na_mean( x, option="mean" )), digits = 1),  is_identical_to(264.1) )
            expect_that( round(mean(na_mean( x, option="mode" )), digits = 1),  is_identical_to(258.8) )
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na_mean( x, option="median" )), digits = 1),  is_identical_to(282.7) )
            expect_that( round(mean(na_mean( x, option="mean" )), digits = 1),  is_identical_to(284.8) )
            expect_that( round(mean(na_mean( x, option="mode" )), digits = 1),  is_identical_to(278.2) )
          })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na_mean( x, option="median" )), digits = 1),  is_identical_to(277.9) )
            expect_that( round(mean(na_mean( x, option="mean" )), digits = 1),  is_identical_to(279.8) )
            expect_that( round(mean(na_mean( x, option="mode" )), digits = 1),  is_identical_to(275.2) )
          })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na_mean(x, option ="mean")), is_false())
            expect_that(anyNA(na_mean(x, option ="mode")), is_false())
            expect_that(anyNA(na_mean(x, option ="median")), is_false())
          })

test_that("Error for wrong input for option parameter",
          {
            expect_that( na_mean(tsAirgap, option ="Wrong"), throws_error())
          })


test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:4] <- NA
            expect_that(anyNA(na_mean(x, option ="mean")), is_false())
            expect_that(anyNA(na_mean(x, option ="mode")), is_false())
            expect_that(anyNA(na_mean(x, option ="median")), is_false())
            expect_that(anyNA(na_mean(x)), is_false())
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[140:144] <- NA
            expect_that(anyNA(na_mean(x, option ="mean")), is_false())
            expect_that(anyNA(na_mean(x, option ="mode")), is_false())
            expect_that(anyNA(na_mean(x, option ="median")), is_false())
            expect_that(anyNA(na_mean(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na_mean(x, option ="mean")), is_false())
            expect_that(anyNA(na_mean(x, option ="mode")), is_false())
            expect_that(anyNA(na_mean(x, option ="median")), is_false())
            expect_that(anyNA(na_mean(x)), is_false())
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na_mean(x, option ="mean")), is_false())
            expect_that(anyNA(na_mean(x, option ="mode")), is_false())
            expect_that(anyNA(na_mean(x, option ="median")), is_false())
            expect_that(anyNA(na_mean(x)), is_false())
            
          })