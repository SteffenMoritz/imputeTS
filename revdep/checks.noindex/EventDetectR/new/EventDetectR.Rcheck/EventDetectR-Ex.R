pkgname <- "EventDetectR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('EventDetectR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("buildEDModel")
### * buildEDModel

flush(stderr()); flush(stdout())

### Name: buildEDModel
### Title: build Event Detection Model
### Aliases: buildEDModel

### ** Examples


## build a simple event detection model with standard configuration
x <- stationBData[100:200,-1]
buildEDModel(x,ignoreVarianceWarning = TRUE)

## Set up a more complex event detection model defining some additional configuration
buildEDModel(x, buildModelAlgo = "ForecastArima",ignoreVarianceWarning = TRUE)

 ## Set up a multivariate neuralnetwork model
buildEDModel(x, buildModelAlgo = "NeuralNetwork",ignoreVarianceWarning = TRUE)



cleanEx()
nameEx("detectEvents")
### * detectEvents

flush(stderr()); flush(stdout())

### Name: detectEvents
### Title: detectEvents in a given data.frame
### Aliases: detectEvents

### ** Examples

## Run event detection with default settings:
def <- detectEvents(x = stationBData[1:100,-1])




cleanEx()
nameEx("getSupportedModels")
### * getSupportedModels

flush(stderr()); flush(stdout())

### Name: getSupportedModels
### Title: getSupportedModels
### Aliases: getSupportedModels

### ** Examples

models <- getSupportedModels()



cleanEx()
nameEx("getSupportedPostProcessors")
### * getSupportedPostProcessors

flush(stderr()); flush(stdout())

### Name: getSupportedPostProcessors
### Title: getSupportedPostProcessors
### Aliases: getSupportedPostProcessors

### ** Examples

preps <- getSupportedPostProcessors()



cleanEx()
nameEx("getSupportedPreparations")
### * getSupportedPreparations

flush(stderr()); flush(stdout())

### Name: getSupportedPreparations
### Title: getSupportedPreparations
### Aliases: getSupportedPreparations

### ** Examples

preps <- getSupportedPreparations()



cleanEx()
nameEx("qualityStatistics")
### * qualityStatistics

flush(stderr()); flush(stdout())

### Name: qualityStatistics
### Title: qualityStatistics
### Aliases: qualityStatistics

### ** Examples

train <- geccoIC2018Train[15000:17000,]
edObject <- detectEvents(train[,-c(1,11)],windowSize = 1000,
                nIterationsRefit = 500,verbosityLevel = 2,
                postProcessorControl = list(nStandardDeviationseventThreshold = 3))
qualityStatistics(edObject, train$EVENT)



cleanEx()
nameEx("simulateEvents")
### * simulateEvents

flush(stderr()); flush(stdout())

### Name: simulateEvents
### Title: Imposes simulated events on the top of the data
### Aliases: simulateEvents

### ** Examples


#Generate event of type sinusoidal and ramp on two columns of the stationBData data set
simupar<-c("B_PH_VAL","B_TEMP_VAL")
SimulatedEvents<-simulateEvents(stationBData,
                                simupar,Event_type = c("sinusoidal","ramp"),
                                Start_index = 2500)

#When specifiying Event_strength the lenght of the vector needs to match the number
#of elements in Params.
SimulatedEvents<-simulateEvents(stationBData,
                                simupar,Event_type = c("sinusoidal","ramp"),
                                Start_index = 2500,
                                Percentage = 0.2,
                                Event_strength = c(4,1))




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
