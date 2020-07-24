context("plots")

test_that("check plots",
          {
              ed <- detectEvents(stationBData[1000:2000,-1],nIterationsRefit = 50,
                                                     verbosityLevel = 2,ignoreVarianceWarning = TRUE)
              expect_error(plot(ed),NA)
          })
