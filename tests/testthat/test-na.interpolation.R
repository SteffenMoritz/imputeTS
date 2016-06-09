context("na.interpolation")

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