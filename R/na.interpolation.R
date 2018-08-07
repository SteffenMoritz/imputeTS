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
#' na.interpolation(x, option = "spline")
#' 
#' #Example 3: Perform stine interpolation
#' na.interpolation(x, option = "stine")
#' 
#' #Example 4: Same as example 1, just written with pipe operator
#' x %>% na.interpolation
#' 
#' #Example 5: Same as example 2, just written with pipe operator
#' x %>% na.interpolation(option = "spline")
#' 
#' @references Johannesson, Tomas, et al. (2015). "Package stinepack". 
#' 
#' @importFrom stinepack stinterp
#' @importFrom stats ts approx spline
#' @importFrom magrittr %>%
#' @export

na.interpolation <- function(x, option = "linear", ...) { 
  
  
  data <- x
  
  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2]) && dim(data)[2] > 1 ) {
    for (i in 1:dim(data)[2]) {
      
      if (!anyNA(data[,i])) {next}
      
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
    
    missindx <- is.na(data)
    
    #Nothing to impute in the data
    if(!anyNA(data)) {
      return(data)
    }
    
    #Minimum amount of non-NA values
    if (sum(!missindx) < 2) {
      stop("Input data needs at least 2 non-NA data point for applying na.interpolation")
    }

    if(!is.null(dim(data)[2])&&!dim(data)[2]==1)
    {stop("Wrong input type for parameter x")}
    
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
    
    n <- length(data)
    
    allindx <- 1:n
    indx <- allindx[!missindx]

    data.vec <- as.vector(data)
    
    if(option =="linear") {
      interp <- stats::approx(indx,data.vec[indx],1:n, rule=2, ...)$y
    }
    else if(option == "spline") {
      interp <- stats::spline(indx,data.vec[indx],n = n, ... )$y
    }
    else if(option == "stine") {
      if (!requireNamespace("stinepack", quietly = TRUE)) {
        stop("Package \"stinepack\" needed for na.interpolation(x, option =\"stine\") to work. Please install it and run again.",
             call. = FALSE)
      }
      interp <- stinepack::stinterp(indx,data.vec[indx],1:n, ...)$y
      #avoid NAs at the beginning and end of series // same behavior like for approx with rule = 2.
      if(any(is.na(interp))) {interp <- na.locf(interp, na.remaining= "rev")}
    }
    else {
      stop("Wrong parameter 'option' given. Value must be either 'linear' or 'spline'.")
    }
    
    #Merge interpolated values back into original time series
    data[missindx] <- interp[missindx]
    
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


