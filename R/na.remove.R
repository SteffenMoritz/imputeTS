#' @title Remove Missing Values
#' 
#' @description Removes all missing values from a time series. 
#'  
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' 
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object (dependent on given input at parameter x)
#' 
#' @details Removes all missing values from a time series. This shortens the time series by
#' the number of missing values in the series. Should be handled with care, because this can affect
#' the seasonality of the time series. Seasonal patterns might be destroyed and/or frequency parameter of the 
#' ts object might be no more correct.
#' 
#' @author Steffen Moritz
#' @seealso  \code{\link[imputeTS]{na.interpolation}},
#' \code{\link[imputeTS]{na.kalman}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.ma}}, \code{\link[imputeTS]{na.mean}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}},
#'  \code{\link[imputeTS]{na.seadec}}, \code{\link[imputeTS]{na.seasplit}}
#'  
#' @examples
#' #Example 1: Remove all NAs
#' #Create Time series with missing values
#' x <- ts(c(2,3,NA,5,6,NA,7,8))
#' #Remove all NAs
#' na.remove(x)
#' 
#' 
#' #Example 2: Remove all NAs in tsAirgap
#' na.remove(tsAirgap)
#' 
#' @import stats
#' @export


na.remove <- function(x) {
  
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
  temp <- numeric()
  for (i in 1:length(data)) {
    if ( ! is.na(data[i])) {      
      temp <- c(temp, data[i])
    }
  }
  data <- ts(temp,frequency = frequency(data))
  
  return(data)
}