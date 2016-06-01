#' @title Missing Value Imputation by Forecast/Backcast combinations
#' 
#' @description Uses a combination of time series forecasts / backcasts for imputation. With multiple different forecasting methods usable. 
#'  
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#'
#' @param weighting Weighting between the forecaste and the backcast. Accepts the following input:
#' \itemize{
#'    \item{"equal" - Forecast and Backcast are weighted equally. The mean of both values for an NA position is used as final result.}
#'    \item{"quantity" - Forecast and Backcast are weighted according to the quantity of data available for model building.(see Details section) }
#'    }
#' @param ... Additional parameters to be passed through to \link{approx} or \link{spline}interpolation functions
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object (dependent on given input at parameter x)
#' 
#' @details Missing values get replaced by values 
#' 
#' The implementation uses the forecast package
#' 
#' @author Steffen Moritz
#' @seealso \code{\link[imputeTS]{na.interpolation}},
#' \code{\link[imputeTS]{na.kalman}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.ma}}, \code{\link[imputeTS]{na.mean}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}},
#'  \code{\link[imputeTS]{na.seadec}}, \code{\link[imputeTS]{na.seasplit}}
#' 
#' @examples
#' #Create Time series with missing values
#' x <- ts(c(2,3,4,5,6,NA,7,8))
#' 
#' #Perform linear interpolation
#' na.interpolation(x)
#' 
#' #Perform spline interpolation
#' na.interpolation(x, option ="spline")
#' 
#' @import stats
#' @import forecast
#' @export

na.forecast <- function(x, weighting = "linear") {
  
  data <- x
  
  #Check for wrong input 
  data <- precheck(data)
  
  #if no missing data, do nothing
  if(!anyNA(data)) {
    return(data)
  }
  
  ##
  ## Imputation Code
  ##
  
  if (length(data)< 25) {
    stop("Time Series too small for na.forecast, more than 32 observations required")
  }
  
  #to get good start...
  interp <- na.interpolation(data)
  
  missindx <- is.na(data)
  if (length(data)> 100) {
    warning("Very large time series with lots of missing value. Algorithm will take a long time.")
  }
  
  
  #Filling starting 10 Positions with na.interpolation,
  #because with 5 values often no model will be build 
  forwardData <- data
  forwardData[1:10] <- interp[1:10]
  
  
  # Forward Model
  for (i in 1:length(forwardData)) 
  {
    if(is.na(forwardData[i]))
    {
      forwardData[i] <-  forecast(ets(forwardData[1:i-1]),h=1)$mean[1]
    }
  }

  #to start with reasonable values
  backwardData <- rev(data)
  backwardData[1:10] <- rev(interp)[1:10]
  
    # Backward Model
  for (i in 1:length(backwardData)) 
  {
    if(is.na(backwardData[i]))
    {
      backwardData[i] <-  forecast(ets(backwardData[1:i-1]),h=1)$mean[1]
    }
  }
 
  back <- rev(backwardData)
  forw <- forwardData
  
  
  #Calculate Weights
  #linear
  if (weighting == "simple")
  {
    weightBackw <- rep(1, length(back))
    weightForw <- rep(1, length(forw))
  }
  else if(weighting == "linear") #
  {
    weightForw <- seq(from =1, by = 1 , length.out = length(data))
    weightBackw <- rev(weightForw)
  }
 
  #Add imputations to the initial dataset
   
  for (i in 1:length(data)) {
    if (is.na(data[i])) {
      data[i] <- mean(c( rep( back[i], weightBackw[i]), rep( forw[i], weightForw[i])), na.rm =T)
      print("i")
      print(i)
      print(rep( back[i], weightBackw[i]))
      print( rep( forw[i], weightForw[i]))
      print(data[i])
    }
  }
  

  return(data)
  
}