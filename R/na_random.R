#' @title Missing Value Imputation by Random Sample
#'
#' @description Replaces each missing value by drawing a random sample
#' between two given bounds.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object in which missing values shall be replaced
#'
#' @param lower_bound Lower bound for the random samples. 
#' If nothing or NULL is set min(x) will be used.
#'
#' @param upper_bound Upper bound for the random samples. 
#' If nothing or NULL is set man(x) will be used.
#'
#' @param maxgap Maximum number of successive NAs to still perform imputation on.
#'  Default setting is to replace all NAs without restrictions. With this
#'  option set, consecutive NAs runs, that are longer than 'maxgap' will
#'  be left NA. This option mostly makes sense if you want to
#'  treat long runs of NA afterwards separately.
#'
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object (dependent on given input at parameter x)
#'
#' @details Replaces each missing value by drawing a random sample between two
#' given bounds. The default bounds are the minimum and the maximum value in
#' the non-NAs from the time series. Function uses \link{runif} function to get
#' the random values.
#'
#' @author Steffen Moritz
#'
#' @seealso  \code{\link[imputeTS]{na_interpolation}},
#' \code{\link[imputeTS]{na_kalman}}, \code{\link[imputeTS]{na_locf}},
#'  \code{\link[imputeTS]{na_ma}}, \code{\link[imputeTS]{na_mean}},
#'  \code{\link[imputeTS]{na_replace}},
#'  \code{\link[imputeTS]{na_seadec}}, \code{\link[imputeTS]{na_seasplit}}
#'
#' @examples
#' # Prerequisite: Create Time series with missing values
#' x <- ts(c(2, 3, NA, 5, 6, NA, 7, 8))
#' 
#' # Example 1: Replace all NAs by random values that are between min and max of the input time series
#' na_random(x)
#' 
#' # Example 2: Replace all NAs by random values between 1 and 10
#' na_random(x, lower_bound = 1, upper_bound = 10)
#' 
#' # Example 3: Same as example 1, just written with pipe operator
#' x %>% na_random()
#' @importFrom stats runif ts
#' @importFrom magrittr %>%
#' @export

na_random <- function(x, lower_bound = NULL, upper_bound = NULL, maxgap = Inf) {
  data <- x



  #----------------------------------------------------------
  # Mulivariate Input
  # The next 20 lines are just for checking and handling multivariate input.
  #----------------------------------------------------------

  # Check if the input is multivariate
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    # Go through columns and impute them by calling this function with univariate input
    for (i in 1:dim(data)[2]) {
      if (!anyNA(data[, i])) {
        next
      }
      # if imputing a column does not work - mostly because it is not numeric - the column is left unchanged
      tryCatch(data[, i] <- na_random(data[, i], lower_bound, upper_bound, maxgap), error = function(cond) {
        warning(paste("imputeTS: No imputation performed for column", i, "because of this", cond), call. = FALSE)
      })
    }
    return(data)
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
    if (sum(!missindx) < 2 && !(!is.null(upper_bound) && !is.null(lower_bound) )) {
      stop("Input data needs at least 2 non-NA data point for applying na_random 
           with default lower_bound and upper_bound settings.")
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
    
    # Combined with check if all NA present, since an all NA vector returns FALSE for is.numeric
    if (!is.numeric(data) & !all(is.na(data))) {
      stop("Input x is not numeric")
    }

    # 1.6 Check and set values for param lower_bound and upper_bound

    # If lower or upper bound is NULL, which is the funtion default usw min/max
    if (is.null(lower_bound)) {
      lower_bound <- min(data, na.rm = TRUE)
    }
    if (is.null(upper_bound)) {
      upper_bound <- max(data, na.rm = TRUE)
    }
    
    if (!is.numeric(lower_bound)) {
      stop("Error for parameter lower_bound: Has to be a numeric value or NULL")
    }
    
    if (!is.numeric(upper_bound)) {
      stop("Error for parameter upper_bound: Has to be a numeric value or NULL")
    }
    
    # For user set upper and lower bounds check if they make sense
    if (lower_bound >= upper_bound) {
      stop("Error for parameter lower_bound: lower_bound must be smaller than upper_bound.

           In case you are using the default settings for these two parameters 
           (which use the min and max of the input series as bounds for the random numbers)
           appearance of this error message means all values of your time series have the same 
           unique value. In this case try to set the bounds manually."
      )
    }



    ##
    ## End Input Check and Transformation
    ##


    ##
    ## 2. Imputation Code
    ##

    data[missindx] <- stats::runif(length(data[missindx]),
      min = lower_bound, max = upper_bound
    )

    ##
    ## End Imputation Code
    ##


    ##
    ## 3. Post Processing
    ##

    # 3.1 Check for Maxgap option

    # If maxgap = Inf then do nothing and when maxgap is lower than 0
    if (is.finite(maxgap) && maxgap >= 0) {

      # Get logical vector of the time series via is.na() and then get the
      # run-length encoding of it. The run-length encoding describes how long
      # the runs of FALSE and TRUE are
      rlencoding <- rle(is.na(x))

      # Runs smaller than maxgap (which shall still be imputed) are set FALSE
      rlencoding$values[rlencoding$lengths <= maxgap] <- FALSE

      # The original vector is being reconstructed by reverse.rls, only now the
      # longer runs are replaced now in the logical vector derived from is.na()
      # in the beginning all former NAs that are > maxgap are also FALSE
      en <- inverse.rle(rlencoding)

      # Set all positions in the imputed series with gaps > maxgap to NA
      # (info from en vector)
      data[en == TRUE] <- NA
    }

    ##
    ## End Post Processing
    ##


    ##
    ## 4. Final Output Formatting
    ##

    # Give back the object originally supplied to the function
    # (necessary for multivariate input with only 1 column)
    if (!is.null(dim(x)[2])) {
      x[, 1] <- data
      return(x)
    }

    ##
    ## End Final Output Formatting
    ##

    return(data)
  }
}