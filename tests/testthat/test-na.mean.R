context("na.mean")
            
test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na.mean( x, option="median" )), digits = 4),  is_identical_to(259.9722) )
            expect_that( round(mean(na.mean( x, option="mean" )), digits = 4),  is_identical_to(264.1148) )
            expect_that( round(mean(na.mean( x, option="mode" )), digits = 4),  is_identical_to(258.75) )
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na.mean( x, option="median" )), digits = 4),  is_identical_to(282.6667) )
            expect_that( round(mean(na.mean( x, option="mean" )), digits = 4),  is_identical_to(284.7638) )
            expect_that( round(mean(na.mean( x, option="mode" )), digits = 4),  is_identical_to(278.1806) )
          })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na.mean( x, option="median" )), digits = 4),  is_identical_to(277.9375) )
            expect_that( round(mean(na.mean( x, option="mean" )), digits = 4),  is_identical_to(279.8168) )
            expect_that( round(mean(na.mean( x, option="mode" )), digits = 4),  is_identical_to(275.2292) )
          })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na.mean(x, option ="mean")), is_false())
            expect_that(anyNA(na.mean(x, option ="mode")), is_false())
            expect_that(anyNA(na.mean(x, option ="median")), is_false())
          })

test_that("Error for wrong input for option parameter",
          {
            expect_that( na.mean(tsAirgap, option ="Wrong"), throws_error())
          })


test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:4] <- NA
            expect_that(anyNA(na.mean(x, option ="mean")), is_false())
            expect_that(anyNA(na.mean(x, option ="mode")), is_false())
            expect_that(anyNA(na.mean(x, option ="median")), is_false())
            expect_that(anyNA(na.mean(x)), is_false())
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[140:144] <- NA
            expect_that(anyNA(na.mean(x, option ="mean")), is_false())
            expect_that(anyNA(na.mean(x, option ="mode")), is_false())
            expect_that(anyNA(na.mean(x, option ="median")), is_false())
            expect_that(anyNA(na.mean(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.mean(x, option ="mean")), is_false())
            expect_that(anyNA(na.mean(x, option ="mode")), is_false())
            expect_that(anyNA(na.mean(x, option ="median")), is_false())
            expect_that(anyNA(na.mean(x)), is_false())
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.mean(x, option ="mean")), is_false())
            expect_that(anyNA(na.mean(x, option ="mode")), is_false())
            expect_that(anyNA(na.mean(x, option ="median")), is_false())
            expect_that(anyNA(na.mean(x)), is_false())
            
          })