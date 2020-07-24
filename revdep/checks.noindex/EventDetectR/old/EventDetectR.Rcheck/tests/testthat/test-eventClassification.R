context("eventClassification")

test_that("wrongInputs lead to errors",
          {
              x <- stationBData[1000:2000,-1]
              m <- buildEDModel(x)

              newDataReal <- stationBData[2001:2020,-1]
              expect_error(predict(m,newDataReal[,-1]),regexp = "Predictions dimensions do not match newData")
          })

test_that("prediction works correct",
          {
              x <- stationBData[1000:2000,-1]
              m <- buildEDModel(x)

              newDataReal <- stationBData[2001:2020,-1]
              p <- predict(m)
              expect_equal(nrow(p),10)
              expect_equal(ncol(p),ncol(stationBData)-1)
              expect_equal(anyNA(p),FALSE)
              expect_equal(is.numeric(p),TRUE)

              p <- predict(m,newDataReal)$lastPredictedEvents
              expect_equal(nrow(p),nrow(newDataReal))
              expect_equal(ncol(p),ncol(stationBData)-1+1)
              expect_equal(typeof(p$Event),"logical")

              m1 <- buildEDModel(x,postProcessorControl = list(nStandardDeviationseventThreshold = 0.1))
              m2 <- buildEDModel(x,postProcessorControl = list(nStandardDeviationseventThreshold = 50))
              p1 <- predict(m1,newDataReal)$lastPredictedEvents
              p2 <- predict(m2,newDataReal)$lastPredictedEvents
              sum(p1$Event)
              expect_true(sum(p1$Event)>sum(p2$Event))
              expect_true(!any(p2$Event))


              #Check that normalization is really deactivated if setting says so
              x <- stationBData[1000:2000,-1]
              m <- buildEDModel(x, dataPreparationControl = list(useNormalization = F))
              newDataReal <- stationBData[2001:2020,-1]
              p <- predict(m)
              expect_true(any(p > 50))
          })

test_that("print works",
          {
              ## Build some ed object
              ed <- detectEvents(stationBData[1000:2000,-1],nIterationsRefit = 50,
                                 verbosityLevel = 0,ignoreVarianceWarning = TRUE,
                                 buildModelAlgo = "ForecastArima")

              printRes <- capture.output(print(ed))
              expect_equal(printRes[1], "Event Detection Object with 11 ARIMA submodels")

              ed$modelList <- ed$modelList[1]
              printRes <- capture.output(print(ed))
              expect_equal(printRes[1], "Event Detection Object with 1 ARIMA submodel")

              ed$modelList <- NULL
              printRes <- capture.output(print(ed))
              expect_equal(printRes[1], "Event Detection Object with no fitted models")
          })

test_that("neural network prediction works",
          {
              x <- stationBData[100:300,-1]
              m <- buildEDModel(x, dataPrepators = "ImputeTSMean", buildModelAlgo = "NeuralNetwork")
              excludedVariable <- length(m$excludedVariables)
              newDataReal <- stationBData[301:320,-1]
              p <- predict.NeuralNetwork(m,newDataReal)
              expect_equal(nrow(p$predictions),20)
              expect_equal(ncol(p$predictions),(ncol(stationBData)-1-excludedVariable))
              expect_equal(anyNA(p),FALSE)


              p <- predict.NeuralNetwork(m,newDataReal)$lastPredictedEvents
              expect_equal(nrow(p),nrow(newDataReal))
              expect_equal(ncol(p),(ncol(stationBData)-1+1))
              expect_equal(typeof(p$Event),"logical")

              m1 <- buildEDModel(x,buildModelAlgo = "NeuralNetwork",postProcessorControl = list(nStandardDeviationseventThreshold = 0.01),buildNeuralNetModelControl = list(algorithm="rprop+",hidden= 2))
              m2 <- buildEDModel(x,buildModelAlgo = "NeuralNetwork",postProcessorControl = list(nStandardDeviationseventThreshold = 50),buildNeuralNetModelControl = list(algorithm="rprop+",hidden= 2))
              p1 <- predict.NeuralNetwork(m1,newDataReal)$lastPredictedEvents
              p2 <- predict.NeuralNetwork(m2,newDataReal)$lastPredictedEvents
              sum(p1$Event)
              expect_true(sum(p1$Event)>sum(p2$Event))
              expect_true(!any(p2$Event))


              #Check that normalization is really deactivated if setting says so
              x <- stationBData[100:200,-1]
              m <- buildEDModel(x, dataPreparationControl = list(useNormalization = F),buildModelAlgo = "NeuralNetwork",ignoreVarianceWarning = TRUE)
              excludedVariable <- length(m$excludedVariables)
              newDataReal <- stationBData[201:220,-1]
              p <- predict.NeuralNetwork(m,newDataReal)
              expect_equal(nrow(p$predictions),nrow(newDataReal))
              expect_equal(ncol(p$predictions),(ncol(stationBData)-1-excludedVariable))

          })


