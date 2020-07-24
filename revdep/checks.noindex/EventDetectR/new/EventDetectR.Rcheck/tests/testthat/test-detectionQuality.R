context("detectionQuality")

test_that("confusionMatrix works",
          {
              if(require("caret") & require("e1071")){
                  train <- geccoIC2018Train[15000:17000,]
                  edObject <- detectEvents(train[,-c(1,11)],windowSize = 500, nIterationsRefit = 200,verbosityLevel = 2,
                                           postProcessorControl = list(nStandardDeviationseventThreshold = 3))
                  stat <- qualityStatistics(edObject, train$EVENT)
                  expect_equal(typeof(stat), "list")
              }
          })
