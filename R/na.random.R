
#' @title Missing Value Imputation by Random Sample
#' 
#' @description Replaces each missing value by drawing a random sample between two given bounds.
#'  
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' 
#' @param lowerBound Lower bound for the random samples (default is min(data) )
#' 
#' @param upperBound Upper bound for the random samples (default is max(data) )
#' 
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object (dependent on given input at parameter x)
#' 
#' @details Replaces each missing value by drawing a random sample between two given bounds. The 
#' default bounds are the minimum and the maximum value in the non-NAs from the time series. Function uses
#' \link{runif} function to get the random values.
#' 
#' @author Steffen Moritz
#' @seealso  \code{\link[imputeTS]{na.interpolation}},
#' \code{\link[imputeTS]{na.kalman}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.ma}}, \code{\link[imputeTS]{na.mean}},
#'  \code{\link[imputeTS]{na.replace}},
#'  \code{\link[imputeTS]{na.seadec}}, \code{\link[imputeTS]{na.seasplit}}
#'  
#' @examples
#' #Prerequisite: Create Time series with missing values
#' x <- ts(c(2,3,NA,5,6,NA,7,8))
#' 
#' #Example 1: Replace all NAs by random values that are between min and max of the input time series
#' na.random(x)
#' 
#' #Example 2: Replace all NAs by random values between 1 and 10
#' na.random(x, lowerBound = 1, upperBound = 10)
#' 
#' @import stats
#' @export

na.random <- function(x, lowerBound = min(data, na.rm = TRUE) , upperBound = max(data, na.rm = TRUE)) {
  
  data <- x
  
  
  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2]) && dim(data)[2] != 1  ) {
    
    #Precheck
    if(!is.numeric(data))
    {stop("All columns of input x have to be completely numeric for applying the na.random function")}
    
    for (i in 1:dim(data)[2]) {
      #if imputing a column does not work (mostly because it is not numeric) the column is left unchanged
      tryCatch(data[,i] <- na.random(data[ ,i], lowerBound, upperBound), error=function(cond) {
        warning(paste("imputeTS: No imputation performed for column",i,"because of this",cond), call. = FALSE)
      })
    }
    return(data)
  }
  
  # Univariate Input
  # All imputation code is within this part
  else {
    
    ##
    ## Input check
    ## 
    
    if(!anyNA(data)) {
      return(data)
    }
    
    if(!is.numeric(data))
    {stop("Input x is not numeric")}
    
    if(!is.null(dim(data)))
    {stop("Wrong input type for parameter x")}
    
    
    ##
    ## Imputation Code
    ##
    
    missindx <- is.na(data)  
    
    #Check that lower bound is not higher than upper boun
    if (lowerBound >= upperBound)
      {stop("Error for parameter lowerBound: Lower Bound must be smaller than Upper Bound ")}
    
    data[missindx] <- runif(1,min=lowerBound,max=upperBound)
  
    
    return(data)
  }
}
  