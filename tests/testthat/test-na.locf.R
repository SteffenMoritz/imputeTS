context("na.locf")



test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "mean")), digits = 4),  is_identical_to(271.9375) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "mean")), digits = 4),  is_identical_to(266.709) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rev")), digits = 4),  is_identical_to(271.9375) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rev")), digits = 4),  is_identical_to(275.3403) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rm")), digits = 4),  is_identical_to(271.9375) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rm")), digits = 4),  is_identical_to(266.709) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "keep"), na.rm=T), digits = 4),  is_identical_to(271.9375) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "keep"), na.rm=T), digits = 4),  is_identical_to(266.709) )
            
               })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "mean")), digits = 4),  is_identical_to(284.3453) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "mean")), digits = 4),  is_identical_to(283.0347) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rev")), digits = 4),  is_identical_to(279.1597) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rev")), digits = 4),  is_identical_to(283.0347) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rm")), digits = 4),  is_identical_to(284.3453) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rm")), digits = 4),  is_identical_to(283.0347) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "keep"), na.rm=T), digits = 4),  is_identical_to(284.3453) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "keep"), na.rm=T), digits = 4),  is_identical_to(283.0347) )
          })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "mean")), digits = 4),  is_identical_to(278.7778) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "mean")), digits = 4),  is_identical_to(282.6944) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rev")), digits = 4),  is_identical_to(278.7778) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rev")), digits = 4),  is_identical_to(282.6944) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "rm")), digits = 4),  is_identical_to(278.7778) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "rm")), digits = 4),  is_identical_to(282.6944) )
            expect_that( round(mean(na.locf(x, option="locf", na.remaining = "keep"), na.rm=T), digits = 4),  is_identical_to(278.7778) )
            expect_that( round(mean(na.locf(x, option="nocb", na.remaining = "keep"), na.rm=T), digits = 4),  is_identical_to(282.6944) )
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