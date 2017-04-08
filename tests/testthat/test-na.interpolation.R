context("na.interpolation")



test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na.interpolation( x, option="linear" )), digits = 4),  is_identical_to(273.6389) )
            expect_that( round(mean(na.interpolation( x, option="spline" )), digits = 4),  is_identical_to(276.1791) )
            expect_that( round(mean(na.interpolation( x, option="stine" )), digits = 4),  is_identical_to(273.3682) )
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na.interpolation( x, option="linear" )), digits = 4),  is_identical_to(281.0972) )
            expect_that( round(mean(na.interpolation( x, option="spline" )), digits = 4),  is_identical_to(282.9646) )
            expect_that( round(mean(na.interpolation( x, option="stine" )), digits = 4),  is_identical_to(280.8348) )
          })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na.interpolation( x, option="linear" )), digits = 4),  is_identical_to(280.7361) )
            expect_that( round(mean(na.interpolation( x, option="spline" )), digits = 4),  is_identical_to(280.0778) )
            expect_that( round(mean(na.interpolation( x, option="stine" )), digits = 4),  is_identical_to(280.4648) )
          })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na.interpolation(x, option ="linear")), is_false())
            expect_that(anyNA(na.interpolation(x, option ="spline")), is_false())
            expect_that(anyNA(na.interpolation(x, option ="stine")), is_false())
          })

test_that("Error for wrong input for option parameter",
          {
            expect_that( na.interpolation(tsAirgap, option="wrongOption"), throws_error())
          })



test_that("Test NA at beginning",
          {
          x <- tsAirgap
          x[1:2] <- NA
          expect_that(anyNA(na.interpolation(x, option="linear")), is_false())
          expect_that(anyNA(na.interpolation(x, option="spline")), is_false())
          expect_that(anyNA(na.interpolation(x, option="stine")), is_false())
          expect_that(anyNA(na.interpolation(x)), is_false())   
          
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na.interpolation(x, option="linear")), is_false())
            expect_that(anyNA(na.interpolation(x, option="spline")), is_false())
            expect_that(anyNA(na.interpolation(x, option="stine")), is_false())
            expect_that(anyNA(na.interpolation(x)), is_false())   
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na.interpolation(x, option="linear")), is_false())
            expect_that(anyNA(na.interpolation(x, option="spline")), is_false())
            expect_that(anyNA(na.interpolation(x, option="stine")), is_false())   
            expect_that(anyNA(na.interpolation(x)), is_false())   
            
            })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na.interpolation(x, option="linear")), is_false())
            expect_that(anyNA(na.interpolation(x, option="spline")), is_false())
            expect_that(anyNA(na.interpolation(x, option="stine")), is_false()) 
            expect_that(anyNA(na.interpolation(x)), is_false())   
            
            })