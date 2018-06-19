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
#' #Example 4: Same as example 1, just written with pipe operator
#' x %>% na.locf
#' 
#' @import stats
#' @export

na.locf <- function(x, option ="locf",  na.remaining = "rev" ) {
  
  data <- x
  
  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2]) && dim(data)[2] > 1 ) {
    for (i in 1:dim(data)[2]) {
      
      if (!anyNA(data[,i])) {next}
      
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
    
    missindx <- is.na(data)
    
    #Nothing to impute in the data
    if(!anyNA(data)) {
      return(data)
    }
    
    #Input completly NA
    if (all(missindx)) {
      stop("Input data has only NAs. Input data needs at least 1 non-NA data point for applying na.locf")
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
    
    #Input as vector
    data.vec <- as.vector(data)
    
    
    #Get needed variables
    n <- length(data.vec)
    allindx <- 1:n
    indx <- allindx[!missindx]
    
    #Last observation carried forward // f = 0
    if (option == "locf") {
      imputed <- locf(data.vec,FALSE)
    }
    #Next observation carried backward // f = 1
    else if (option == "nocb") {
      imputed <- locf(data.vec, TRUE)
    }
    #Wrong input
    else {
      stop("Wrong parameter 'option' given. Value must be either 'locf' or 'nocb'.")
    }

    data[missindx] <- imputed[missindx]
    
    ##
    ## na.remaining - what to do with remaining NAs after imputation

    #no NAs or keep NAs -> do nothing keep NAs untouched
    if (!anyNA(data)||na.remaining == "keep") {
        #do nothing
    }
    #Replace NAs through locf/nocb from the other direction
    else if (na.remaining == "rev") {
      if (option == "locf") {
        data <- na.locf(data, option = "nocb")
      }
      else if (option == "nocb") {
        data <- na.locf(data, option = "locf")
      }
    }
    #Remove all NAs
    else if(na.remaining == "rm"){
      data <- na.remove(data)
    }
    #Replace NAs with overall mean
    else if(na.remaining == "mean") {
      data <- na.mean(data)
    }
    #Wrong Input
    else {
      stop("Wrong parameter 'na.remaining' given. Value must be either 'keep', 'rm', 'mean' or 'rev'.") 
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
