##De-Roxygenized to avoid appearance in the package documentation

# @title Algorithm selection (Internal function)
# @description Internal function for choosing between the basic univariate imputation algortihms
# @param x  Supposed to be a univariate time series 
# @return Time Series (\code{\link{ts}}) object that fulfills the requirements
# @author Steffen Moritz
#' @import stats


apply_base_algorithm <- function(x, algorithm, ...) { 
  
  data <- x
  
  #checking for false input
  if(algorithm == "locf")
  { data <- na_locf(data, ...) }
  
  else if(algorithm == "mean")
  { data <- na_mean(data, ...) }
  
  else if(algorithm == "random")
  { data <- na_random(data,  ...) }
  
  else if(algorithm == "interpolation")
  { data <- na_interpolation(data,  ...) }
  
  else if(algorithm == "kalman")
  { data <- na_kalman(data,  ...) }
  
  else if(algorithm == "ma")
  { data <- na_ma(data,  ...) }
  
  else 
  {stop("Wrong parameter for option algorithm choosen.")}
  
  
  
  return(data)
  
}