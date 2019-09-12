context("Advanced Time Series Objects Input")


test_that("tsibble objects",
          {
            skip_on_cran()
            if (!requireNamespace("tsibble", quietly = TRUE)) {
              warning("Pkg tsibble needed for this test.",
                      call. = FALSE)
            }
            else {
              require("tsibble")
              x <- as_tsibble(tsAirgap)
              expect_that(anyNA(na.mean(x)), is_false())
              expect_that(anyNA(na.kalman(x)), is_false())
              expect_that(anyNA(na.interpolation(x)), is_false())
              expect_that(anyNA(na.locf(x)), is_false())
              expect_that(anyNA(na.ma(x)), is_false())
              expect_that(anyNA(na.random(x)), is_false())
              expect_that(anyNA(na.seadec(x)), is_false())
              expect_that(anyNA(na.seasplit(x)), is_false())
              expect_that(anyNA(na.replace(x)), is_false())
            }
            
          })

test_that("Multivariate tsibble ",
          {
            skip_on_cran()
            if (!requireNamespace("tsibble", quietly = TRUE)) {
              warning("Pkg tsibble needed for this test.",
                      call. = FALSE)
            }
            else {
              require("tsibble")
              x <- data.frame(tsAirgap,tsAirgap)
              x <- as.ts(x)
              x <- as_tsibble(x)
              expect_that(anyNA(na.mean(x)), is_false())
              expect_that(anyNA(na.kalman(x)), is_false())
              expect_that(anyNA(na.interpolation(x)), is_false())
              expect_that(anyNA(na.locf(x)), is_false())
              expect_that(anyNA(na.ma(x)), is_false())
              expect_that(anyNA(na.random(x)), is_false())
              expect_that(anyNA(na.seadec(x)), is_false())
              expect_that(anyNA(na.seasplit(x)), is_false())
              expect_that(anyNA(na.replace(x)), is_false())
            }
            
          })

test_that("tibble objects",
          {
            skip_on_cran()
            if (!requireNamespace("tibble", quietly = TRUE)) {
              warning("Pkg tibble needed for this test.",
                      call. = FALSE)
            }
            else {
              require("tibble")
              x <- as_tibble(tsAirgap)
              expect_that(anyNA(na.mean(x)), is_false())
              expect_that(anyNA(na.kalman(x)), is_false())
              expect_that(anyNA(na.interpolation(x)), is_false())
              expect_that(anyNA(na.locf(x)), is_false())
              expect_that(anyNA(na.ma(x)), is_false())
              expect_that(anyNA(na.random(x)), is_false())
              expect_that(anyNA(na.seadec(x)), is_false())
              expect_that(anyNA(na.seasplit(x)), is_false())
              expect_that(anyNA(na.replace(x)), is_false())
            }
            
          })


test_that("multivariate tibble objects",
          {
            skip_on_cran()
            if (!requireNamespace("tibble", quietly = TRUE)) {
              warning("Pkg tibble needed for this test.",
                      call. = FALSE)
            }
            else {
              require("tibble")
              
              x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
              z <- as_tibble(x)
              
              expect_that(anyNA(na.mean(z)), is_false())
              expect_that(anyNA(na.kalman(z)), is_false())
              expect_that(anyNA(na.interpolation(z)), is_false())
              expect_that(anyNA(na.locf(z)), is_false())
              expect_that(anyNA(na.ma(z)), is_false())
              expect_that(anyNA(na.random(z)), is_false())
              expect_that(anyNA(na.seadec(z)), is_false())
              expect_that(anyNA(na.seasplit(z)), is_false())
              expect_that(anyNA(na.replace(z)), is_false())
            }
          })


test_that("multivariate tibble objects - non-numeric input",
          {
            skip_on_cran()
            if (!requireNamespace("tibble", quietly = TRUE)) {
              warning("Pkg tibble needed for this test.",
                      call. = FALSE)
            }
            else {
              require("tibble")
           
              x <- data.frame(rep("x",144),tsAirgap, tsAirgap, rep("qq",144), tsAirgapComplete)
              z <- as_tibble(x)
              
              expect_that(anyNA(na.mean(z)), is_false())
              expect_that(anyNA(na.kalman(z)), is_false())
              expect_that(anyNA(na.interpolation(z)), is_false())
              expect_that(anyNA(na.locf(z)), is_false())
              expect_that(anyNA(na.ma(z)), is_false())
              expect_that(anyNA(na.random(z)), is_false())
              expect_that(anyNA(na.seadec(z)), is_false())
              expect_that(anyNA(na.seasplit(z)), is_false())
              expect_that(anyNA(na.replace(z)), is_false())
            }
          })



test_that("zoo objects",
          {
            skip_on_cran()
            if (!requireNamespace("zoo", quietly = TRUE)) {
              warning("Pkg zoo needed for this test.",
                   call. = FALSE)
            }
            else {
              require("zoo")
              x <- as.zoo(tsAirgap)
              expect_that(anyNA(na.mean(x)), is_false())
              expect_that(anyNA(na.kalman(x)), is_false())
              expect_that(anyNA(na.interpolation(x)), is_false())
              expect_that(anyNA(na.locf(x)), is_false())
              expect_that(anyNA(na.ma(x)), is_false())
              expect_that(anyNA(na.random(x)), is_false())
              expect_that(anyNA(na.seadec(x)), is_false())
              expect_that(anyNA(na.seasplit(x)), is_false())
              expect_that(anyNA(na.replace(x)), is_false())
            }
           
          })

test_that("multivariate zoo objects",
          {
          skip_on_cran()
            if (!requireNamespace("zoo", quietly = TRUE)) {
              warning("Pkg zoo needed for this test.",
                   call. = FALSE)
            }
            else {
              require("zoo")
              time <- seq(from = as.Date(as.yearmon(start(tsAirgap)))[1],  by = "month" , 
                          length.out = length(tsAirgap))
              x <- data.frame(time, coredata(tsAirgap), coredata(tsAirgap), coredata(tsAirgapComplete))
              
              z <- read.zoo(x, format = "%Y-%m-%d")
              
              expect_that(anyNA(na.mean(z)), is_false())
              expect_that(anyNA(na.kalman(z)), is_false())
              expect_that(anyNA(na.interpolation(z)), is_false())
              expect_that(anyNA(na.locf(z)), is_false())
              expect_that(anyNA(na.ma(z)), is_false())
              expect_that(anyNA(na.random(z)), is_false())
              expect_that(anyNA(na.seadec(z)), is_false())
              expect_that(anyNA(na.seasplit(z)), is_false())
              expect_that(anyNA(na.replace(z)), is_false())
            }
          })


test_that("xts objects",
          {
            skip_on_cran()
            if (!requireNamespace("xts", quietly = TRUE)) {
              warning("Pkg xts needed for this test.",
                   call. = FALSE)
            }
            else {
              require("xts")
              x <- as.xts(tsAirgap)
              expect_that(anyNA(na.mean(x)), is_false())
              expect_that(anyNA(na.kalman(x)), is_false())
              expect_that(anyNA(na.interpolation(x)), is_false())
              expect_that(anyNA(na.locf(x)), is_false())
              expect_that(anyNA(na.ma(x)), is_false())
              expect_that(anyNA(na.random(x)), is_false())
              expect_that(anyNA(na.seadec(x)), is_false())
              expect_that(anyNA(na.seasplit(x)), is_false())
              expect_that(anyNA(na.replace(x)), is_false())
            }
          })


test_that("timeSeries objects",
          {
            skip_on_cran()
            
            if (!requireNamespace("timeSeries", quietly = TRUE)) {
              warning("Pkg timeSeries needed for this test.",
                   call. = FALSE)
            }
            else {
              require("timeSeries")
              x <- as.timeSeries(tsAirgap)
              expect_that(anyNA(na.mean(x)), is_false())
              expect_that(anyNA(na.kalman(x)), is_false())
              expect_that(anyNA(na.interpolation(x)), is_false())
              expect_that(anyNA(na.locf(x)), is_false())
              expect_that(anyNA(na.ma(x)), is_false())
              expect_that(anyNA(na.random(x)), is_false())
              expect_that(anyNA(na.seadec(x)), is_false())
              expect_that(anyNA(na.seasplit(x)), is_false())
              expect_that(anyNA(na.replace(x)), is_false())
            }
          })



test_that("tis objects",
          {
            skip_on_cran()
            
            if (!requireNamespace("tis", quietly = TRUE)) {
              warning("Pkg timeSeries needed for this test.",
                      call. = FALSE)
            }
            else {
              require("tis")
              x <- as.tis(tsAirgap)
              expect_that(anyNA(na.mean(x)), is_false())
              expect_that(anyNA(na.kalman(x)), is_false())
              expect_that(anyNA(na.interpolation(x)), is_false())
              expect_that(anyNA(na.locf(x)), is_false())
              expect_that(anyNA(na.ma(x)), is_false())
              expect_that(anyNA(na.random(x)), is_false())
              expect_that(anyNA(na.seadec(x)), is_false())
         #     expect_that(anyNA(na.seasplit(x)), is_false())
              expect_that(anyNA(na.replace(x)), is_false())
            }
          })

test_that("Imputation works for mts",
          {
            skip_on_cran()
            
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            x <- ts(x)
            expect_that(anyNA(na.mean(x)), is_false())
            expect_that(anyNA(na.kalman(x)), is_false())
            expect_that(anyNA(na.interpolation(x)), is_false())
            expect_that(anyNA(na.locf(x)), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            expect_that(anyNA(na.random(x)), is_false())
            expect_that(anyNA(na.seadec(x)), is_false())
            expect_that(anyNA(na.seasplit(x)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
          })

test_that("Imputation works for data.frame",
          {
            skip_on_cran()
            
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            expect_that(anyNA(na.mean(x)), is_false())
            expect_that(anyNA(na.kalman(x)), is_false())
            expect_that(anyNA(na.interpolation(x)), is_false())
            expect_that(anyNA(na.locf(x)), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            expect_that(anyNA(na.random(x)), is_false())
            expect_that(anyNA(na.seadec(x)), is_false())
            expect_that(anyNA(na.seasplit(x)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
          })

test_that("Imputation works for data.frame univariate",
          {
            skip_on_cran()
            
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap)
            expect_that(anyNA(na.mean(x)), is_false())
            expect_that(anyNA(na.kalman(x)), is_false())
            expect_that(anyNA(na.interpolation(x)), is_false())
            expect_that(anyNA(na.locf(x)), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            expect_that(anyNA(na.random(x)), is_false())
            expect_that(anyNA(na.seadec(x)), is_false())
            expect_that(anyNA(na.seasplit(x)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
          })


test_that("Imputation works for matrix",
          {
            skip_on_cran()
            
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap, tsAirgap, tsAirgapComplete)
            x <- as.matrix(x)
            expect_that(anyNA(na.mean(x)), is_false())
            expect_that(anyNA(na.kalman(x)), is_false())
            expect_that(anyNA(na.interpolation(x)), is_false())
            expect_that(anyNA(na.locf(x)), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            expect_that(anyNA(na.random(x)), is_false())
            expect_that(anyNA(na.seadec(x)), is_false())
            expect_that(anyNA(na.seasplit(x)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
          })


test_that("Imputation works for matrix univariate",
          {
            skip_on_cran()
            
            #Checking if NAs remain in data.frame
            x <- data.frame(tsAirgap)
            x <- as.matrix(x)
            expect_that(anyNA(na.mean(x)), is_false())
            expect_that(anyNA(na.kalman(x)), is_false())
            expect_that(anyNA(na.interpolation(x)), is_false())
            expect_that(anyNA(na.locf(x)), is_false())
            expect_that(anyNA(na.ma(x)), is_false())
            expect_that(anyNA(na.random(x)), is_false())
            expect_that(anyNA(na.seadec(x)), is_false())
            expect_that(anyNA(na.seasplit(x)), is_false())
            expect_that(anyNA(na.replace(x)), is_false())
          })