context("na.replace")


test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na.replace( x, fill = -1)), digits = 4),  is_identical_to(223.6111) )
            expect_that( round(mean(na.replace( x, fill = 200 )), digits = 4),  is_identical_to(254.3194) )
            })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na.replace( x, fill = -1)), digits = 4),  is_identical_to(251.0278) )
            expect_that( round(mean(na.replace( x, fill = 200)), digits = 4),  is_identical_to(274.7569) )
            })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na.replace( x, fill = -1)), digits = 4),  is_identical_to(254.4653) )
            expect_that( round(mean(na.replace( x, fill = 200 )), digits = 4),  is_identical_to(272.6111) )
            })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na.replace(x, fill = -1 )), is_false())
            expect_that(anyNA(na.replace(x, fill = 200)), is_false())
              })

test_that("Test NA at beginning",
          {
            x <- tsAirgap
            x[1:2] <- NA
            expect_that(anyNA(na.replace(x, fill = -5)), is_false())
            expect_that(anyNA(na.replace(x, fill = 1000)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
            
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.replace(x, fill = -5)), is_false())
            expect_that(anyNA(na.replace(x, fill = 1000)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.replace(x, fill = -5)), is_false())
            expect_that(anyNA(na.replace(x, fill = 1000)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.replace(x, fill = -5)), is_false())
            expect_that(anyNA(na.replace(x, fill = 1000)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
            
          })

test_that("Handling for no NAs",
          {
            x <- tsAirgapComplete
            expect_that(anyNA(na.replace(x)), is_false())
            
          })