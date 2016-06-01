##De-Roxygenized to avoid appearance in the package documentation

# @title Algorithm selection (Internal function)
# @description Internal function for choosing between the basic univariate imputation algortihms
# @param x  Supposed to be a univariate time series 
# @param na.identifier Supposed to be a character string or numeric value. 
# If another value than NA indicates missing values this is specified with this parameter. 
# @details Checks, if data is really a univariate time series and checks that na.identifier is no list or vector.
# @return Time Series (\code{\link{ts}}) object that fulfills the requirements
# @author Steffen Moritz
#' @import stats


apply.base.algorithm <- function(x, algorithm, ...) { 
  
  data <- x
  #checking for false input
  if(algorithm == "locf")
  { data <- na.locf(data, ...) }
  
  else if(algorithm == "mean")
  { data <- na.mean(data, ...) }
  
  else if(algorithm == "random")
  { data <- na.random(data,  ...) }
  
  else if(algorithm == "interpolation")
  { data <- na.interpolation(data,  ...) }
  
  else if(algorithm == "kalman")
  { data <- na.kalman(data,  ...) }
  
  else if(algorithm == "ma")
  { data <- na.ma(data,  ...) }
  
  else 
  {stop("Wrong parameter for option algorithm choosen.")}
  
  
  
  return(data)
  
}