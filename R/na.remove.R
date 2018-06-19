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
#' #Example 1: Remove all NAs
#' na.remove(x)
#' 
#' 
#' #Example 2: Remove all NAs in tsAirgap
#' na.remove(tsAirgap)
#' 
#' #Example 3: Same as example 1, just written with pipe operator
#' x %>% na.remove
#' 
#' @import stats
#' @export

na.remove <- function(x) {
  
  
  data <- x
    
    ##
    ## Input check
    ## 
  
  missindx <- is.na(data)
  
  #Nothing to impute in the data
  if(!anyNA(data)) {
    return(data)
  }
  
  #Input completly NA
  if (all(missindx)) {
    stop("Input data has only NAs")
  }
    
    if(!is.null(dim(data)[2])&&!dim(data)[2]==1)
    {stop("Input x is not univariate")}
    
    # Altering multivariate objects with 1 column (which are essentially univariate) to be dim = NULL
    if (!is.null(dim(data)[2])) {
      data <- data[,1]
    }
    
    if(!is.numeric(data))
    {stop("Input x is not numeric")}
    
    ## End Input Check
 
  
    ##
    ## Imputation Code
    ##
    temp <- numeric()
    for (i in 1:length(data)) {
      if ( ! is.na(data[i])) {      
        temp <- c(temp, data[i])
      }
    }
    
    ## End Imputation Code
    
    ##
    ## Ouput Formatting
    ##
    
    #Since all time information of a ts object would be incorrect after removing values
    # only the vector is returned by the function
  
    return(temp)
}
