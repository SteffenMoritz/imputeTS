
#' @title Replace Missing Values by a Defined Value
#' 
#' @description Replaces all missing values with a given value.
#'  
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' 
#' @param fill Value used to replace the missing values
#' 
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object (dependent on given input at parameter x)
#' 
#' @author Steffen Moritz
#' @seealso  \code{\link[imputeTS]{na.interpolation}},
#' \code{\link[imputeTS]{na.kalman}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.ma}}, \code{\link[imputeTS]{na.mean}},
#'  \code{\link[imputeTS]{na.random}},
#'  \code{\link[imputeTS]{na.seadec}}, \code{\link[imputeTS]{na.seasplit}}
#'  
#' @examples
#' #Prerequisite: Create Time series with missing values
#' x <- ts(c(2,3,NA,5,6,NA,7,8))
#' 
#' #Example 1: Replace all NAs with 3.5
#' na.replace(x, fill = 3.5 )
#' 
#' #Example 2: Replace all NAs with 0
#' na.replace(x, fill = 0 ) 
#' 
#' @import stats
#' @export


na.replace <- function(x, fill = 0) {
  
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
  
  missindx <- is.na(data)   
  data[missindx] <- fill
  
  return(data)
}