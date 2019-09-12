context("na_interpolation")

test_that("All NA vector throws error",
          {
            expect_error(na_interpolation(c(NA,NA,NA,NA,NA)))
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at end)",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[135:144] <- NA
            expect_that( round(mean(na_interpolation( x, option="linear" )), digits = 1),  is_identical_to(273.6) )
            expect_that( round(mean(na_interpolation( x, option="spline" )), digits = 1),  is_identical_to(276.2) )
            expect_that( round(mean(na_interpolation( x, option="stine" )), digits = 1),  is_identical_to(273.4) )
          })

test_that("Correct results for all options with a modifed tsAirgap dataset (additionalNAs at start)",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            x[1:5] <- NA
            expect_that( round(mean(na_interpolation( x, option="linear" )), digits = 1),  is_identical_to(281.1) )
            expect_that( round(mean(na_interpolation( x, option="spline" )), digits = 1),  is_identical_to(283.0) )
            expect_that( round(mean(na_interpolation( x, option="stine" )), digits = 1),  is_identical_to(280.8) )
          })


test_that("Correct results for all options with the tsAirgap dataset",
          {
            skip_on_cran()
            
            #Using mean over resulting vector to check correctness
            #In order to avoid writing down the complete resulting vector
            #Using rounded version in order to avoid writing down all decimals
            x <- tsAirgap
            expect_that( round(mean(na_interpolation( x, option="linear" )), digits = 1),  is_identical_to(280.7) )
            expect_that( round(mean(na_interpolation( x, option="spline" )), digits = 1),  is_identical_to(280.1) )
            expect_that( round(mean(na_interpolation( x, option="stine" )), digits = 1),  is_identical_to(280.5) )
          })

test_that("Imputation works for data.frame",
          {
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na_interpolation(x, option ="linear")), is_false())
            expect_that(anyNA(na_interpolation(x, option ="spline")), is_false())
            expect_that(anyNA(na_interpolation(x, option ="stine")), is_false())
          })

test_that("Error for wrong input for option parameter",
          {
            expect_that( na_interpolation(tsAirgap, option="wrongOption"), throws_error())
          })



test_that("Test NA at beginning",
          {
          x <- tsAirgap
          x[1:2] <- NA
          expect_that(anyNA(na_interpolation(x, option="linear")), is_false())
          expect_that(anyNA(na_interpolation(x, option="spline")), is_false())
          expect_that(anyNA(na_interpolation(x, option="stine")), is_false())
          expect_that(anyNA(na_interpolation(x)), is_false())   
          
          })

test_that("Test NA at end",
          {
            x <- tsAirgap
            x[143:144] <- NA
            expect_that(anyNA(na_interpolation(x, option="linear")), is_false())
            expect_that(anyNA(na_interpolation(x, option="spline")), is_false())
            expect_that(anyNA(na_interpolation(x, option="stine")), is_false())
            expect_that(anyNA(na_interpolation(x)), is_false())   
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgap
            x[40:80] <- NA
            expect_that(anyNA(na_interpolation(x, option="linear")), is_false())
            expect_that(anyNA(na_interpolation(x, option="spline")), is_false())
            expect_that(anyNA(na_interpolation(x, option="stine")), is_false())   
            expect_that(anyNA(na_interpolation(x)), is_false())   
            
            })

test_that("Over 90% NAs",
          {
            x <- tsAirgap
            x[10:140] <- NA
            expect_that(anyNA(na_interpolation(x, option="linear")), is_false())
            expect_that(anyNA(na_interpolation(x, option="spline")), is_false())
            expect_that(anyNA(na_interpolation(x, option="stine")), is_false()) 
            expect_that(anyNA(na_interpolation(x)), is_false())   
            
            })