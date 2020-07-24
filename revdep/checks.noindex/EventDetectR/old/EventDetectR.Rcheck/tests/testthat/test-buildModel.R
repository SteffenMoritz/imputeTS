testthat::skip_on_cran()

context("buildModel Errors")

test_that("Errors wrong inputs",
          {
              ## Check for Wrong Data input
              x <- "SomeString"
              expect_error(buildEDModel(x), regexp = "x has to be a data.frame")
              expect_error(buildEDModel(NULL), regexp = "no data was specified for variable x")
              expect_error(buildEDModel(as.data.frame(c(NaN,NaN)))
                           , regexp = "The specified data for x contained NaNs")


              x <- stationBData[1500:2000,]
              expect_error(buildEDModel(x), regexp = "non-numeric data")

              # no variance in data
              x <- stationBData[1:500,-1]
              expect_warning(m <- buildEDModel(x), regexp = "data set contain no variance")
              expect_equal(ncol(predict(m)),11)

              x <- stationBData[1:500,c(5,5), drop = F]
              expect_error(buildEDModel(x), regexp = "There is no variance in your data set")

              ## Check for wrong model or preparator names
              x <- stationBData[1000:2000,-1]
              expect_error(buildEDModel(x, dataPrepators = "SomeWrongPrep")
                           , regexp = "not supported")
              expect_error(buildEDModel(x, buildModelAlgo = "SomeWrongAlgo")
                           , regexp = "not supported")
              expect_error(buildEDModel(x, buildModelAlgo = 7)
                           , regexp = "not supported")
          })

test_that("General Functionality",
          {
              x <- stationBData[1:500,-1]

              # Check Functionality
              expect_warning(expect_equal(ncol(predict(buildEDModel(x))),11))
              expect_warning(expect_equal(ncol(predict(buildEDModel(x), stationBData[501:510,-1])$lastPredictedEvents),13))

              x <- stationBData[1000:2000,-1]
              expect_equal(class(buildEDModel(x)),"UnivariateForecast")
              expect_equal(length(buildEDModel(x)$modelList),ncol(x))

              ## Building a model should also work without dataPreparation / postprocessing
              expect_equal(class(buildEDModel(x,dataPrepators = NULL)),"UnivariateForecast")
              expect_equal(class(buildEDModel(x,postProcessors = NULL)),"UnivariateForecast")
              expect_equal(class(buildEDModel(x,dataPrepators = NULL, postProcessors = NULL)),"UnivariateForecast")
          })

context("buildModel NA Crashes - prepper")
test_that("NAs dont make you crash - prepper",
          {
              x <- stationBData[1000:2000,-1]

              # test with NAs in Data with each data preparator
              #
              for(prepper in unlist(getSupportedPreparations())){
                  modelWithPrep <- buildEDModel(stationBData[2850:2900,-1], dataPrepators = prepper)

              #    expect_equal(class(modelWithPrep),"UnivariateForecast")
                  expect_equal(length(modelWithPrep$modelList),ncol(x))
              }
          })

test_that("NAs dont make you crash - prepper - start point",
          {
              x <- stationBData[1000:2000,-1]

              # test with NAs in Data with each data preparator at NA start point
              #
              for(prepper in unlist(getSupportedPreparations())){
                  x <- stationBData[2887:2900,-c(1,5)]
                  modelWithPrep <- buildEDModel(x, dataPrepators = prepper)

               #   expect_equal(class(modelWithPrep),"UnivariateForecast")
                  expect_equal(length(modelWithPrep$modelList),ncol(x))
              }
          })

context("buildModel NA Crashes - modelAlgo- NN")
test_that("NAs dont make you crash - modelAlgo-NN",
          {
              x <- stationBData[1000:2000,-1]

              # test with NAs in Data with each modelAlgo
              #
              for(modelAlgo in unlist(getSupportedModels())){

                  x <- stationBData[1:100,-c(1)]
                  model <- buildEDModel(x, buildModelAlgo = modelAlgo, ignoreVarianceWarning = T)
                  if(modelAlgo!="NeuralNetwork")
                  {
                  expect_error(predictionWithoutNewData <- predict(model),regexp = NA)
                  expect_error(prediction <- predict(model, stationBData[501:520,-1]),regexp = NA)
                  expect_equal(nrow(predictionWithoutNewData),10)
                  expect_equal(nrow(prediction$predictions),20)
                  }
                  else if(modelAlgo=="NeuralNetwork")
                  {
                      expect_error(prediction <- predict.NeuralNetwork(model, stationBData[501:520,-1]),regexp = NA)
                  }
                  expect_equal(nrow(prediction$predictions),20)
              }
          })
