#' @title Seasonally Splitted Missing Value Imputation 
#' 
#' @description Splits the times series into seasons and afterwards performs imputation
#' seperatly for each of the resulting time series datasets (each containing the data 
#' for one specific season).
#
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' @param algorithm Algorithm to be used after splits. Accepts the following input:
#' \itemize{
#'    \item{"interpolation" - Imputation by Interpolation}
#'    \item{"locf" - Imputation by Last Observation Carried Forward}
#'    \item{"mean" - Imputation by Mean Value}
#'    \item{"random" - Imputation by Random Sample}
#'    \item{"kalman" - Imputation by Kalman Smooting and State Space Models}
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
#' #Prerequisite: Load a time series with missing values
#' x <- tsAirgap
#' 
#' #Example 1: Perform seasonal splitted imputation using algortihm = "interpolation"
#' na.seasplit(x, algorithm = "interpolation")
#' 
#' #Example 2: Perform seasonal splitted imputation using algortihm = "mean"
#' na.seasplit(x, algorithm = "mean")
#' 
#' @import stats 
#' @export


na.seasplit <- function(x, algorithm="interpolation" , ...) { 
  
  data <- x
  
  #Check for wrong input 
  data <- precheck(data)
  
  #if no missing data, do nothing
  if(!anyNA(data)) {
    return(data)
  }
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