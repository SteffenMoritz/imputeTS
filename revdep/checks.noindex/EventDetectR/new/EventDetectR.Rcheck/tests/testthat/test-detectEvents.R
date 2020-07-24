testthat::skip_on_cran()
context("detectEvents")

test_that("wrongInputs lead to errors",
          {
              ## Check for Wrong Data input
              xTests <- list(
                  "SomeString"
                  , data.frame(c(4,5,6,NaN))
                  , NULL
                  , NaN
                  , NA
              )
              xGood <- stationBData[1000:2000,-1]
              wTests <- list(
                  "String",
                  999999999,
                  -1,
                  0,
                  4,
                  NA,
                  NaN,
                  NULL
              )
              wGood <- 100
              nTests <- list(
                  0,
                  -1,
                  "String",
                  NA,
                  NaN,
                  NULL
              )
              nGood <- 50
              vTests <- list(
                  -1,
                  "string",
                  NA,
                  NaN,
                  NULL
              )
              vGood <- 0

              for(x in xTests){
                  expect_error(detectEvents(x,wGood,nGood,vGood, ignoreVarianceWarning = T), regexp = "detectEvents:")
              }
              for(w in wTests){
                  expect_error(detectEvents(xGood,w,nGood,vGood, ignoreVarianceWarning = T), regexp = "detectEvents:")
              }
              for(n in nTests){
                  expect_error(detectEvents(xGood,wGood,n,vGood, ignoreVarianceWarning = T), regexp = "detectEvents:")
              }
              for(v in vTests){
                  expect_error(detectEvents(xGood,wGood,nGood,v, ignoreVarianceWarning = T), regexp = "detectEvents:")
              }
          })

test_that("general functionality",{
    xGood <- stationBData[1000:2000,-1]
    eds <- detectEvents(xGood,windowSize = 100,nIterationsRefit = 50,ignoreVarianceWarning = T)
    expect_equal(nrow(eds$classification), nrow(xGood))
})
