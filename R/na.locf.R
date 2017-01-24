#' @title Missing Value Imputation by Last Observation Carried Forward
#' 
#' @description Replaces each missing value with the most recent present value prior to it
#'  (Last Observation Carried Forward- LOCF). Optionally this can also be done starting from the back of the series
#'  (Next Observation Carried Backward - NOCB).
#'  
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' @param option Algorithm to be used. Accepts the following input:
#' \itemize{
#'    \item{"locf" - for Last Observation Carried Forward}
#'    \item{"nocb" - for Next Observation Carried Backward}
#'    }
#'    
#' @param na.remaining Method to be used for remaining NAs.
#' \itemize{
#'    \item{"keep" - to return the series with NAs}
#'    \item{"rm" - to remove remaining NAs}
#'    \item{"mean" - to replace remaining NAs by overall mean}
#'    \item{"rev" - to perform nocb / locf from the reverse direction}
#'    }
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object (dependent on given input at parameter x)
#' 
#' @details Replaces each missing value with the most recent present value prior to it
#'  (Last Observation Carried Forward- LOCF). This can also be done from the reverse direction -starting from the back
#'  (Next Observation Carried Backward - NOCB). Both options have the issue, that NAs at the beginning 
#'  (or for nocb at the end) of the time series cannot be imputed (since there is no last value to 
#'  be carried forward present yet). In this case there are remaining NAs in the imputed time series.
#'  Since this only concerns very few values at the beginning of the series,
#'   na.remaining offers some quick solutions to get a series without NAs back.
#' 
#' @author Steffen Moritz
#' @seealso  \code{\link[imputeTS]{na.interpolation}},
#' \code{\link[imputeTS]{na.kalman}},
#'  \code{\link[imputeTS]{na.ma}}, \code{\link[imputeTS]{na.mean}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}},
#'  \code{\link[imputeTS]{na.seadec}}, \code{\link[imputeTS]{na.seasplit}}
#'  
#' @examples
#' #Prerequisite: Create Time series with missing values
#' x <- ts(c(NA,3,4,5,6,NA,7,8))
#' 
#' #Example 1: Perform LOCF
#' na.locf(x)
#' 
#' #Example 2: Perform NOCF
#' na.locf(x, option = "nocb")
#' 
#' #Example 3: Perform LOCF and remove remaining NAs
#' na.locf(x, na.remaining = "rm")
#' 
#' @import stats
#' @export

na.locf <- function(x, option ="locf",  na.remaining = "rev" ) {
  
  data <- x
  
  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2])  ) {
    for (i in 1:dim(data)[2]) {
      #if imputing a column does not work (mostly because it is not numeric) the column is left unchanged
      tryCatch(data[,i] <- na.locf(data[ ,i], option, na.remaining), error=function(cond) {
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

    #Input as vector
    data.vec <- as.vector(data)
    
    #Get needed variables
    n <- length(data.vec)
    allindx <- 1:n
    indx <- allindx[!missindx]
    
    ifelse(na.remaining == "rev", z <- 2, z <- 1) 
    
    ## locf and nocb is realized via approx (over 10 times faster than a loop)
    #Last observation carried forward // f = 0
    if (option == "locf") {
      interp <- approx(indx,data.vec[indx],1:n, rule=z:2,method = "constant", f = 0)$y
    }
    #Next observation carried backward // f = 1
    else if (option == "nocb") {
      interp <- approx(indx,data.vec[indx],1:n, rule=2:z,method = "constant", f = 1)$y
    }
    #Wrong input
    else {
      stop("Wrong parameter 'option' given. Value must be either 'locf' or 'nocb'.")
    }
    
    data[missindx] <- interp[missindx]
    

    ## na.remaining - what to do with remaining NAs after imputation
    
    # Return if no remaining NAs 
    if(!anyNA(data)) {
      return(data)
    }
    
    #keep NAs untouched
    if (na.remaining == "keep") {
      return(data)
    }
    #rev is already checked in the beginning through variable z
    if (na.remaining == "rev") {
      return(data)
    }
    #Remove all NAs
    else if(na.remaining == "rm"){
      return(na.remove(data))
    }
    #Replace NAs with overall mean
    else if(na.remaining == "mean") {
      return(na.mean(data))
    }
    #Wrong Input
    else {
      stop("Wrong parameter 'na.remaining' given. Value must be either 'keep', 'rm', 'mean' or 'rev'.") 
    }
  }
}
