#' @title Remove Missing Values
#' 
#' @description Removes all missing values from a time series. 
#'  
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' 
#' @return Vector (\code{\link{vector}})
#' 
#' @details Removes all missing values from a input time series. This shortens the time series by
#' the number of missing values in the series. Should be handled with care, because this can affect
#' the seasonality of the time series. Seasonal patterns might be destroyed. Independent from the input, this 
#' function only returns a vector. (the time information of a resulting time series object wouldn't be correct any more).
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
#' 
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
    
    ##
    ## Input check
    ## 
  
  if(!is.null(dim(data)))
  {stop("Input x is not univariate")}
  
  if(!anyNA(data)) {
    return(data)
  }

  if(!is.numeric(data))
  {stop("Input x is not numeric")}
  
 
  
    ##
    ## Imputation Code
    ##
    temp <- numeric()
    for (i in 1:length(data)) {
      if ( ! is.na(data[i])) {      
        temp <- c(temp, data[i])
      }
    }
    #Since all time information of a ts object would be incorrect after removing values
    # only a vector is returned by the function
  
    return(temp)
}
