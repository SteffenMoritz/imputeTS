#' @title Seasonally Decomposed Missing Value Imputation 
#' 
#' @description Removes the seasonal component from the time series, performs imputation on the deseasonalized series and afterwards adds the seasonal component again.
#'  
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' @param algorithm Algorithm to be used after decomposition. Accepts the following input:
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
#'  \code{\link[imputeTS]{na.seasplit}}
#' 
#' @examples
#' #Example 1: Perform seasonal imputation using algorithm = "interpolation"
#' na.seadec(tsAirgap, algorithm = "interpolation")
#' 
#' #Example 2: Perform seasonal imputation using algorithm = "mean"
#' na.seadec(tsAirgap, algorithm = "mean")
#' 
#' @import stats
#' @export

na.seadec <- function(x, algorithm = "interpolation" , ...) { 
  
  
  data <- x
  
  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2]) && dim(data)[2] != 1  ) {
    for (i in 1:dim(data)[2]) {
      #if imputing a column does not work (mostly because it is not numeric) the column is left unchanged
      tryCatch(data[,i] <- na.seadec(data[ ,i], algorithm, ...), error=function(cond) {
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
  
    missindx <- is.na(data)  
    
    #approx NAs, to get complete series, because stl does not work with NAs
    temp <- na.interpolation(data)
    
    stl <- stl(temp,robust=TRUE, s.window = 11)
    # just take trend component + irregular  (remove seasonality)
    ts.noSeasonality <- stl$time.series[,2]+stl$time.series[,3]
    
    #Fill in NAs again
    ts.noSeasonality[missindx] <- NA
    
    #Perform imputation
    ts.imputed <- apply.base.algorithm(ts.noSeasonality, algorithm = algorithm,...)
    
    # add seasonality 
    data <- ts.imputed + stl$time.series[,1]
    
    
    return(data)
  }
}
