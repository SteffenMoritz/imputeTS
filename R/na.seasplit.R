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
#' #Example 3: Same as example 1, just written with pipe operator
#' tsAirgap %>% na.seasplit(algorithm = "interpolation")
#' 
#' @importFrom stats frequency ts
#' @importFrom magrittr %>%
#' @export

na.seasplit <- function(x, algorithm="interpolation" , ...) { 
  
  
  data <- x
  
  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2]) && dim(data)[2] > 1 ) {
    for (i in 1:dim(data)[2]) {
      
      if (!anyNA(data[,i])) {next}
      
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
    
    
    if(stats::frequency(data)==1) {
      warning("No seasonality information for dataset found, going on without decomposition")
      data <- apply.base.algorithm(data, algorithm = algorithm,...)
      return(data)
    }
    
    if(length(data) < stats::frequency(data)*2 ) {
      warning("More than 2 complete periods needed to perform a seasonal split. The algorithm will go on without seasonal split.")
      data <- apply.base.algorithm(data, algorithm = algorithm,...)
      return(data)
    }
    
    ##End Input Check
    
    
    ##
    ## Imputation Code
    ##
    
    for(i in 1:stats::frequency(data)) {
      
      #get indices for one season
      indices <- seq(from = i, to = length(data), by = stats::frequency(data))
      
      #Create time series just with one season
      ts.temp <- stats::ts(data[indices])
      
      #Apply algorithm on this season
      ts.temp <- apply.base.algorithm(ts.temp, algorithm = algorithm,...)
      
      #Write result back into original time series
      data[indices] <- as.vector(ts.temp)
    }
    
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
