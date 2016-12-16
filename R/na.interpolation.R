#' @title Missing Value Imputation by Interpolation
#' 
#' @description Uses either linear, spline or stineman interpolation to replace missing values.
#'  
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' @param option Algorithm to be used. Accepts the following input:
#' \itemize{
#'    \item{"linear" - for linear interpolation using \link{approx} }
#'    \item{"spline" - for spline interpolation using \link{spline}}
#'    \item{"stine" - for Stineman interpolation using \link[stinepack]{stinterp}}
#'    }
#' @param ... Additional parameters to be passed through to \link{approx} or \link{spline} interpolation functions
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object (dependent on given input at parameter x)
#' 
#' @details Missing values get replaced by values of a \link{approx}, \link{spline} or \link[stinepack]{stinterp} interpolation. 
#'  
#'  
#' @author Steffen Moritz
#' @seealso  \code{\link[imputeTS]{na.kalman}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.ma}}, \code{\link[imputeTS]{na.mean}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}},
#'  \code{\link[imputeTS]{na.seadec}}, \code{\link[imputeTS]{na.seasplit}}
#'  
#' @examples
#' #Prerequisite: Create Time series with missing values
#' x <- ts(c(2,3,4,5,6,NA,7,8))
#' 
#' #Example 1: Perform linear interpolation
#' na.interpolation(x)
#' 
#' #Example 2: Perform spline interpolation
#' na.interpolation(x, option ="spline")
#' 
#' #Example 3: Perform stine interpolation
#' na.interpolation(x, option ="stine")
#' 
#' @references Johannesson, Tomas, et al. (2015). "Package stinepack". 
#' 
#' @import stats
#' @import stinepack
#' @export

na.interpolation <- function(x, option = "linear", ...) { 
  
  
  data <- x
  
  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2]) && dim(data)[2] != 1  ) {
    for (i in 1:dim(data)[2]) {
      #if imputing a column does not work (mostly because it is not numeric) the column is left unchanged
      tryCatch(data[,i] <- na.interpolation(data[ ,i], option), error=function(cond) {
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
    
    #if no missing data, do nothing
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
    
    n <- length(data)
    
    allindx <- 1:n
    indx <- allindx[!missindx]

    data.vec <- as.vector(data)
    
    if(option =="linear") {
      interp <- approx(indx,data.vec[indx],1:n, rule=2, ...)$y
    }
    else if(option == "spline") {
      interp <- spline(indx,data.vec[indx],n = n, ... )$y
    }
    else if(option == "stine") {
      interp <- stinterp(indx,data.vec[indx],1:n, ...)$y
      #avoid NAs at the beginning and end of series // same behavior like for approx with rule = 2.
      if(any(is.na(interp))) {interp <- na.locf(interp, na.remaining= "rev")}
    }
    else {
      stop("Wrong parameter 'option' given. Value must be either 'linear' or 'spline'.")
    }
    
    #Merge interpolated values back into original time series
    data[missindx] <- interp[missindx]

    return(data)
  }
}


