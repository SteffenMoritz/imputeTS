context("na.locf")

test_that("All NA vector throws error",
          {
            expect_error(na.locf(c(NA,NA,NA,NA,NA)))
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "mean")), digits = 1),  is_identical_to(271.9) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "mean")), digits = 1),  is_identical_to(266.7) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rev")), digits = 1),  is_identical_to(271.9) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rev")), digits = 1),  is_identical_to(275.3) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rm")), digits = 1),  is_identical_to(271.9) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rm")), digits = 1),  is_identical_to(266.7) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "keep"), na.rm=T), digits = 1),  is_identical_to(271.9) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "keep"), na.rm=T), digits = 1),  is_identical_to(266.7) )
            
               })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "mean")), digits = 1),  is_identical_to(284.3) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "mean")), digits = 1),  is_identical_to(283.0) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rev")), digits = 1),  is_identical_to(279.2) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rev")), digits = 1),  is_identical_to(283.0) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rm")), digits = 1),  is_identical_to(284.3) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rm")), digits = 1),  is_identical_to(283.0) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "keep"), na.rm=T), digits = 1),  is_identical_to(284.3) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "keep"), na.rm=T), digits = 1),  is_identical_to(283.0) )
          })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "mean")), digits = 1),  is_identical_to(278.8) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "mean")), digits = 1),  is_identical_to(282.7) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rev")), digits = 1),  is_identical_to(278.8) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rev")), digits = 1),  is_identical_to(282.7) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rm")), digits = 1),  is_identical_to(278.8) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rm")), digits = 1),  is_identical_to(282.7) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "keep"), na.rm=T), digits = 1),  is_identical_to(278.8) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "keep"), na.rm=T), digits = 1),  is_identical_to(282.7) )
          })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "rm")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "rm")), is_false())
          })



test_that("Error for wrong input for option parameter",
          {
            expect_that( na.locf(tsAirgap, option="wrongOption"), throws_error())
          })

test_that("Error for wrong input for na.remaining parameter",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that( na.locf(x, na.remaining ="Wrong"), throws_error())
          })


test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "rev")), is_false())
            expect_that(length(na.locf(x, option="nocb", na.remaining = "rm")), equals(144))
            expect_that(length(na.locf(x, option="locf", na.remaining = "rm")), equals(142))
            expect_that(length(na.locf(x, option="nocb", na.remaining = "keep")), equals(144))
            
            expect_that(anyNA(na.locf(x)), is_false())
            
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "rev")), is_false())
            expect_that(length(na.locf(x, option="nocb", na.remaining = "rm")), equals(142))
            expect_that(length(na.locf(x, option="locf", na.remaining = "rm")), equals(144))
            expect_that(anyNA(na.locf(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "rev")), is_false())
            expect_that(length(na.locf(x, option="nocb", na.remaining = "rm")), equals(144))
            expect_that(length(na.locf(x, option="locf", na.remaining = "rm")), equals(144))
            expect_that(anyNA(na.locf(x)), is_false())
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="locf", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "mean")), is_false())
            expect_that(anyNA(na.locf(x, option="nocb", na.remaining = "rev")), is_false())
            expect_that(anyNA(na.locf(x)), is_false())
            
          })