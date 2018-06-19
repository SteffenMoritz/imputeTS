
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
#' #Example 3: Same as example 1, just written with pipe operator
#' x %>% na.random
#' 
#' @importFrom magrittr %>%
#' @import stats
#' @export

na.random <- function(x, lowerBound = min(x, na.rm = TRUE) , upperBound = max(x, na.rm = TRUE)) {
  data <- x
  
  # Indicator if default parameter is used. If default min(x, na.rm = TRUE), missL will be TRUE.
  # This default parameter has to be set individually in multivariate case, hence the marker.
  missU <- missing(upperBound)
  missL <- missing(lowerBound)

  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2]) && dim(data)[2] > 1 ) {
    for (i in 1:dim(data)[2]) {
      
      if (!anyNA(data[,i])) {next}
      
      #In case auf default parameters, set these for each column seperately
      if(missU == TRUE) {upperBound = max(data[ ,i], na.rm = TRUE)}
      if(missL == TRUE) {lowerBound = min(data[ ,i], na.rm = TRUE)}
      
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
    {stop("Wrong input type for parameter x")}
    
    # Altering multivariate objects with 1 column (which are essentially univariate) to be dim = NULL
    if (!is.null(dim(data)[2])) {
      data <- data[,1]
    }
    
    if(!is.numeric(data))
    {stop("Input x is not numeric")}
    
    # Special check - because no element being of type int can be given to funtion KalmanRun/KalmanSmooth
    data[1:length(data)] <- as.numeric(data)
    
    # End Input Check
    
    
    ##
    ## Imputation Code
    ##
    
    
    #Check that lower bound is not higher than upper boun
    if (lowerBound >= upperBound)
      {stop("Error for parameter lowerBound: Lower Bound must be smaller than Upper Bound ")}
    
    data[missindx] <- runif(1,min=lowerBound,max=upperBound)
    
    ## End Imputation Code
    
    
    ##
    ## Ouput Formatting
    ##
    
    
    # Give back the object originally supplied to the function (necessary for multivariate input with only 1 column)
    if (!is.null(dim(x)[2])) {
      x[,1] <- data
      return(x)
    }
    
    return(data)
  }
}
  