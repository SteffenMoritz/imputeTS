#' @title Missing Value Imputation by Mean Value
#' 
#' @description Missing value replacement by mean values. Different means like median, mean, mode possible.
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' @param option Algorithm to be used. Accepts the following input:
#' \itemize{
#'    \item{"mean" - take the mean for imputation}
#'    \item{"median" - take the median for imputation}
#'    \item{"mode" - take the mode for imputation}
#'    }
#'    
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object (dependent on given input at parameter x)
#' 
#' @details Missing values get replaced by overall mean values. The function calculates the mean, median or mode
#' over all the non-NA values and replaces all NAs with this value. Option 'mode' replaces NAs with the 
#' most frequent value in the time series. If two or more values occur equally frequent, the function imputes
#' with the lower value. That's why 'mode' is not the best option for decimal values.
#' 
#' @author Steffen Moritz
#' 
#' @seealso \code{\link[imputeTS]{na.interpolation}},
#' \code{\link[imputeTS]{na.kalman}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.ma}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}},
#'  \code{\link[imputeTS]{na.seadec}}, \code{\link[imputeTS]{na.seasplit}}
#'
#' @examples
#' #Prerequisite: Create Time series with missing values
#' x <- ts(c(2,3,4,5,6,NA,7,8))
#' 
#' #Example 1: Perform imputation with the overall mean
#' na.mean(x)
#' 
#' #Example 2: Perform imputation with overall median
#' na.mean(x, option ="median")
#' @import stats
#' @export na.mean
#' 
na.mean <- function(x, option ="mean") {
  
  data <- x
  
  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2])  ) {
    for (i in 1:dim(data)[2]) {
      #if imputing a column does not work (mostly because it is not numeric) the column is left unchanged
      tryCatch(data[,i] <- na.mean(data[ ,i], option), error=function(cond) {
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
    
    if(option == "median") {
      #Use Median
      median <- median(data, na.rm = TRUE)    
      data[missindx] <- median 
    }
    else if(option == "mode") {
      #Calculate Mode
      temp <- table(as.vector(data))
      mode <- names(temp)[temp == max(temp)]
      mode <- (as.numeric(mode))[1]
      
      data[missindx] <- mode 
      
    }
    else if(option == "mean") {  
      #Use Mean
      mean <- mean(data, na.rm = TRUE)
      data[missindx] <- mean 
    }
    else
    {stop("Wrong option parameter given, must be 'mean', 'mode' or 'median'")}
    
    return(data)
  }
  
}
