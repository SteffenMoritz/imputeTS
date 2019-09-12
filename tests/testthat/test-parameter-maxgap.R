context("maxgap")


test_that("Test that function works and prints output",
          {
            x <- tsAirgap
            x[4] <- NA
            x[144] <- NA
            x[143] <- NA
            expect_that( sum(is.na(na_mean(x, maxgap = 1))), equals(7))
            expect_that( sum(is.na(na_mean(x, maxgap = 0))), equals(16))
            expect_that( sum(is.na(na_mean(x, maxgap = -1))), equals(0))
            expect_that( sum(is.na(na_mean(x, maxgap = 2))), equals(3))
            expect_that( sum(is.na(na_mean(x, maxgap = 3))), equals(0))
            
            expect_that( sum(is.na(na_locf(x, maxgap = 1))), equals(7))
            expect_that( sum(is.na(na_locf(x, maxgap = 0))), equals(16))
            expect_that( sum(is.na(na_locf(x, maxgap = -1))), equals(0))
            expect_that( sum(is.na(na_locf(x, maxgap = 2))), equals(3))
            expect_that( sum(is.na(na_locf(x, maxgap = 3))), equals(0))
            
            expect_that( sum(is.na(na_random(x, maxgap = 1))), equals(7))
            expect_that( sum(is.na(na_random(x, maxgap = 0))), equals(16))
            expect_that( sum(is.na(na_random(x, maxgap = -1))), equals(0))
            expect_that( sum(is.na(na_random(x, maxgap = 2))), equals(3))
            expect_that( sum(is.na(na_random(x, maxgap = 3))), equals(0))
            
            expect_that( sum(is.na(na_ma(x, maxgap = 1))), equals(7))
            expect_that( sum(is.na(na_ma(x, maxgap = 0))), equals(16))
            expect_that( sum(is.na(na_ma(x, maxgap = -1))), equals(0))
            expect_that( sum(is.na(na_ma(x, maxgap = 2))), equals(3))
            expect_that( sum(is.na(na_ma(x, maxgap = 3))), equals(0))
            
            expect_that( sum(is.na(na_ma(x, maxgap = 1))), equals(7))
            expect_that( sum(is.na(na_ma(x, maxgap = 0))), equals(16))
            expect_that( sum(is.na(na_ma(x, maxgap = -1))), equals(0))
            expect_that( sum(is.na(na_ma(x, maxgap = 2))), equals(3))
            expect_that( sum(is.na(na_ma(x, maxgap = 3))), equals(0))
            
            expect_that( sum(is.na(na_seadec(x, maxgap = 1))), equals(7))
            expect_that( sum(is.na(na_seadec(x, maxgap = 0))), equals(16))
            expect_that( sum(is.na(na_seadec(x, maxgap = -1))), equals(0))
            expect_that( sum(is.na(na_seadec(x, maxgap = 2))), equals(3))
            expect_that( sum(is.na(na_seadec(x, maxgap = 3))), equals(0))
            
            expect_that( sum(is.na(na_seasplit(x, maxgap = 1))), equals(7))
            expect_that( sum(is.na(na_seasplit(x, maxgap = 0))), equals(16))
            expect_that( sum(is.na(na_seasplit(x, maxgap = -1))), equals(0))
            expect_that( sum(is.na(na_seasplit(x, maxgap = 2))), equals(3))
            expect_that( sum(is.na(na_seasplit(x, maxgap = 3))), equals(0))
            
            expect_that( sum(is.na(na_kalman(x, maxgap = 1))), equals(7))
            expect_that( sum(is.na(na_kalman(x, maxgap = 0))), equals(16))
            expect_that( sum(is.na(na_kalman(x, maxgap = -1))), equals(0))
            expect_that( sum(is.na(na_kalman(x, maxgap = 2))), equals(3))
            expect_that( sum(is.na(na_kalman(x, maxgap = 3))), equals(0))
            
            expect_that( sum(is.na(na_interpolation(x, maxgap = 1))), equals(7))
            expect_that( sum(is.na(na_interpolation(x, maxgap = 0))), equals(16))
            expect_that( sum(is.na(na_interpolation(x, maxgap = -1))), equals(0))
            expect_that( sum(is.na(na_interpolation(x, maxgap = 2))), equals(3))
            expect_that( sum(is.na(na_interpolation(x, maxgap = 3))), equals(0))

          })
