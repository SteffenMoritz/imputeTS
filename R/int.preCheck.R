##De-Roxygenized to avoid appearance in the package documentation

# @title Pre-Check Imputation Input (Internal function)
# @description Internal function for pre-checking the input of imputation functions
# @param x  Supposed to be a univariate time series 
# If another value than NA indicates missing values this is specified with this parameter. 
# @details Checks, if data is really a univariate time series or numeric vector.
# @return Time Series (\code{\link{ts}}) object that fulfills the requirements
# @author Steffen Moritz
#' @import stats


precheck <- function(x) { 
  
  data <- x
  
  #checking for false input
  if(!(is.ts(data)|is.vector(data)))
  {stop("Input x has to be a numeric time series or a numeric vector")}
  
  if(!is.null(dim(data)))
  {stop("The input object is not univariate.")}
  
  if(!is.numeric(data))
  {stop("Input x has to be completely numeric")}
  
  if(length(data) < 2)
  {stop("Input x may not be of length = 1")}
  
  return(data)
  
}