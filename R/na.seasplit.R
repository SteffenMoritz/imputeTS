#' @title Seasonally Splitted Missing Value Imputation 
#' 
#' @description Splits the times series into seasons and afterwards performs imputation
#' separately for each of the resulting time series datasets (each containing the data 
#' for one specific season).
#
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' @param algorithm Algorithm to be used after splits. Accepts the following input:
#' \itemize{
#'    \item{"interpolation" - Imputation by Interpolation}
#'    \item{"locf" - Imputation by Last Observation Carried Forward}
#'    \item{"mean" - Imputation by Mean Value}
#'    \item{"random" - Imputation by Random Sample}
#'    \item{"kalman" - Imputation by Kalman Smoothing and State Space Models}
#'    \item{"ma" - Imputation by Weighted Moving Average}
#'    }
#'
#' @param ... Additional parameters for these algorithms that can be passed through. Look at \code{\link[imputeTS]{na.interpolation}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.mean}} for parameter options.
#'    
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object (dependent on given input at parameter x)
#' 
#' 
#' @author Steffen Moritz
#' @seealso  \code{\link[imputeTS]{na.interpolation}},
#' \code{\link[imputeTS]{na.kalman}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.ma}}, \code{\link[imputeTS]{na.mean}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}},
#'  \code{\link[imputeTS]{na.seadec}}
#'  
#' @examples
#' #Example 1: Perform seasonal splitted imputation using algorithm = "interpolation"
#' na.seasplit(tsAirgap, algorithm = "interpolation")
#' 
#' #Example 2: Perform seasonal splitted imputation using algorithm = "mean"
#' na.seasplit(tsAirgap, algorithm = "mean")
#' 
#' @import stats 
#' @export

na.seasplit <- function(x, algorithm="interpolation" , ...) { 
  
  
  data <- x
  
  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2]) && dim(data)[2] != 1  ) {
    for (i in 1:dim(data)[2]) {
      #if imputing a column does not work (mostly because it is not numeric) the column is left unchanged
      tryCatch(data[,i] <- na.seasplit(data[ ,i], algorithm, ...), error=function(cond) {
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
    
    if(frequency(data)==1) {
      warning("No seasonality information for dataset found, going on without decomposition")
      data <- apply.base.algorithm(data, algorithm = algorithm,...)
      return(data)
    }
    
    ##
    ## Imputation Code
    ##
    
    for(i in 1:frequency(data)) {
      
      #get indices for one season
      indices <- seq(from = i, to = length(data), by = frequency(data))
      
      #Create time series just with one season
      ts.temp <- ts(data[indices])
      
      #Apply algorithm on this season
      ts.temp <- apply.base.algorithm(ts.temp, algorithm = algorithm,...)
      
      #Write result back into original time series
      data[indices] <- as.vector(ts.temp)
    }
    
    return(data)
  }
}
