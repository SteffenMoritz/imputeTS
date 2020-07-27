#' @title Missing Value Imputation by Weighted Moving Average
#'
#' @description Missing value replacement by weighted moving average.
#' Uses semi-adaptive window size to ensure all NAs are replaced.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object in which missing values shall be replaced
#'
#' @param weighting Weighting to be used. Accepts the following input:
#' \itemize{
#'    \item{"simple" - Simple Moving Average (SMA)}
#'    \item{"linear" - Linear Weighted Moving Average (LWMA)}
#'    \item{"exponential" - Exponential Weighted Moving Average (EWMA)}
#'    }
#'
#' @param k integer width of the moving average window. Expands to both sides
#' of the center element e.g. k=2 means 4 observations (2 left, 2 right) are
#' taken into account. If all observations in the current window are NA, the
#' window size is automatically increased until there are at least 2 non-NA
#' values present.
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
#' @details In this function missing values get replaced by moving average
#' values. Moving Averages are also sometimes referred to as "moving mean",
#' "rolling mean", "rolling average" or "running average".
#'
#' The mean in this implementation taken from an equal number of observations
#' on either side of a central value. This means for an NA value at position
#' \code{i} of a time series, the observations i-1,i+1 and i+1, i+2 (assuming
#' a window size of k=2) are used to calculate the mean.
#'
#' Since it can in case of long NA gaps also occur, that all values next to the
#' central value are also NA, the algorithm has a semi-adaptive window size.
#' Whenever there are less than 2 non-NA values in the complete window available,
#' the window size is incrementally increased, till at least 2 non-NA values are
#' there. In all other cases the algorithm sticks to the pre-set window size.
#'
#' There are options for using Simple Moving Average (SMA), Linear Weighted
#' Moving Average (LWMA) and Exponential Weighted Moving Average (EWMA).
#'
#' SMA: all observations in the window are equally weighted for calculating the mean.
#'
#' LWMA: weights decrease in arithmetical progression. The observations
#' directly next to a central value i, have weight 1/2, the observations
#' one further away (i-2,i+2) have weight 1/3, the next (i-3,i+3) have
#' weight 1/4, ...
#'
#' EWMA: uses weighting factors which decrease exponentially. The observations
#' directly next to a central value i, have weight 1/2^1, the observations one
#' further away (i-2,i+2) have weight 1/2^2, the next (i-3,i+3) have weight 1/2^3, ...
#'
#'
#' @author Steffen Moritz
#'
#' @seealso  \code{\link[imputeTS]{na_interpolation}},
#' \code{\link[imputeTS]{na_kalman}}, \code{\link[imputeTS]{na_locf}},
#' \code{\link[imputeTS]{na_mean}},
#'  \code{\link[imputeTS]{na_random}}, \code{\link[imputeTS]{na_replace}},
#'  \code{\link[imputeTS]{na_seadec}}, \code{\link[imputeTS]{na_seasplit}}
#'
#' @examples
#' # Example 1: Perform imputation with simple moving average
#' na_ma(tsAirgap, weighting = "simple")
#' 
#' # Example 2: Perform imputation with exponential weighted moving average
#' na_ma(tsAirgap)
#' 
#' # Example 3: Perform imputation with exponential weighted moving average, window size 6
#' na_ma(tsAirgap, k = 6)
#' 
#' # Example 4: Same as example 1, just written with pipe operator
#' tsAirgap %>% na_ma(weighting = "simple")
#' 
#' @importFrom magrittr %>%
#' @export

na_ma <- function(x, k = 4, weighting = "exponential", maxgap = Inf) {
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
      tryCatch(data[, i] <- na_ma(data[, i], k, weighting, maxgap), error = function(cond) {
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
    if (sum(!missindx) < 2) {
      stop("Input data needs at least 2 non-NA data point for applying na_ma")
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

    # 1.6 Check for wrong values of param k
    if (k < 1) {
      stop("Parameter k has  to be larger than 0")
    }

    ##
    ## End Input Check and Transformation
    ##


    ##
    ## 2. Imputation Code
    ##

    # Imputation is performed i C++ code na_ma.cpp
    data <- ma(data, k, weighting)

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