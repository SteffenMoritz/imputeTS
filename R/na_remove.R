#' @title Remove Missing Values
#'
#' @description Removes all missing values from a time series.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object in which missing values shall be replaced
#'
#' @return Vector (\code{\link{vector}})
#'
#' @details Removes all missing values from a input time series. This shortens
#' the time series by the number of missing values in the series. Should be
#' handled with care, because this can affect the seasonality of the time
#' series. Seasonal patterns might be destroyed. Independent from the input,
#' this function only returns a vector. (the time information of a resulting
#' time series object wouldn't be correct any more).
#'
#' @author Steffen Moritz
#'
#' @seealso  \code{\link[imputeTS]{na_interpolation}},
#' \code{\link[imputeTS]{na_kalman}}, \code{\link[imputeTS]{na_locf}},
#'  \code{\link[imputeTS]{na_ma}}, \code{\link[imputeTS]{na_mean}},
#'  \code{\link[imputeTS]{na_random}}, \code{\link[imputeTS]{na_replace}},
#'  \code{\link[imputeTS]{na_seadec}}, \code{\link[imputeTS]{na_seasplit}}
#'
#' @examples
#' # Example 1: Remove all NAs
#' # Create Time series with missing values
#' x <- ts(c(2, 3, NA, 5, 6, NA, 7, 8))
#' 
#' # Example 1: Remove all NAs
#' na_remove(x)
#' 
#' # Example 2: Remove all NAs in tsAirgap
#' na_remove(tsAirgap)
#' 
#' # Example 3: Same as example 1, just written with pipe operator
#' x %>% na_remove()
#' 
#' @importFrom stats ts
#' @importFrom magrittr %>%
#' @export

na_remove <- function(x) {
  data <- x


  #----------------------------------------------------------
  # Mulivariate Input
  # The next 20 lines are just for checking and handling multivariate input.
  #----------------------------------------------------------

  # Check if the input is multivariate
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    stop("na_remove only works with univariate input")
  }


  #----------------------------------------------------------
  # Univariate Input
  # All relveant imputation / pre- postprocessing  code is within this part
  #----------------------------------------------------------

  else {
    missindx <- is.na(data)

    ##
    ## 1. Input Check and Transformation
    ##


    # 1.1 Check if NAs are present
    if (!anyNA(data)) {
      return(data)
    }

    # 1.2 special handling data types
    if (any(class(data) == "tbl")) {
      data <- as.vector(as.data.frame(data)[, 1])
    }

    # 1.3 Check for algorithm specific minimum amount of non-NA values
    if (all(missindx)) {
      stop("Input data has only NAs")
    }


    # 1.4 Checks and corrections for wrong data dimension

    # Check if input dimensionality is not as expected
    if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
      stop("Wrong input type for parameter x")
    }

    # Altering multivariate objects with 1 column (which are essentially
    # univariate) to be dim = NULL
    if (!is.null(dim(data)[2])) {
      data <- data[, 1]
    }

    # 1.5 Check if input is numeric
    if (!is.numeric(data)) {
      stop("Input x is not numeric")
    }

    ##
    ## End Input Check and Transformation
    ##


    ##
    ## 2. Imputation Code
    ##

    temp <- numeric()
    for (i in 1:length(data)) {
      if (!is.na(data[i])) {
        temp <- c(temp, data[i])
      }
    }

    ##
    ## End Imputation Code
    ##


    ##
    ## 3. Post Processing
    ##

    # No Post Processing needed for na_remove

    ##
    ## End Post Processing
    ##


    ##
    ## 4. Final Output Formatting
    ##

    # Since all time information of a ts object would be incorrect
    # after removing values only the vector is returned by the function

    ##
    ## End Final Output Formatting
    ##

    return(temp)
  }
}