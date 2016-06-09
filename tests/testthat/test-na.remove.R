context("na.remove")


test_that("Test NA at beginning",
          {
            x <- tsAirgapComplete
            x[1:2] <- NA
            expect_that(length(na.remove(x)), equals(142))
          })

test_that("Test NA at end",
          {
            x <- tsAirgapComplete
            x[143:144] <- NA
            expect_that(length(na.remove(x)), equals(142))
            
          })

test_that("Multiple NAs in a row",
          {
            x <- tsAirgapComplete
            x[40:80] <- NA
            expect_that(length(na.remove(x)), equals(103))
            
            
          })

test_that("Over 90% NAs",
          {
            x <- tsAirgapComplete
            x[10:140] <- NA
            expect_that(length(na.remove(x)), equals(13))
    
          })