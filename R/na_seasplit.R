#' @title Seasonally Splitted Missing Value Imputation
#'
#' @description Splits the times series into seasons and afterwards performs
#' imputation separately for each of the resulting time series datasets
#' (each containing the data for one specific season).
#
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object in which missing values shall be replaced
#' @param algorithm Algorithm to be used after splits.
#' Accepts the following input:
#' \itemize{
#'    \item{"interpolation" - Imputation by Interpolation}
#'    \item{"locf" - Imputation by Last Observation Carried Forward}
#'    \item{"mean" - Imputation by Mean Value}
#'    \item{"random" - Imputation by Random Sample}
#'    \item{"kalman" - Imputation by Kalman Smoothing and State Space Models}
#'    \item{"ma" - Imputation by Weighted Moving Average}
#'    }
#'
#' @param find_frequency If TRUE the algorithm will try to estimate the frequency 
#' of the time-series automatically.
#'
#' @param maxgap Maximum number of successive NAs to still perform imputation on.
#'  Default setting is to replace all NAs without restrictions. With this
#'  option set, consecutive NAs runs, that are longer than 'maxgap' will
#'  be left NA. This option mostly makes sense if you want to
#'  treat long runs of NA afterwards separately.
#'
#' @param ... Additional parameters for these algorithms that can be
#' passed through. Look at \code{\link[imputeTS]{na_interpolation}},
#' \code{\link[imputeTS]{na_locf}}, \code{\link[imputeTS]{na_random}},
#' \code{\link[imputeTS]{na_mean}} for parameter options.
#'
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object (dependent on given input at parameter x)
#'
#' @author Steffen Moritz
#'
#' @seealso  \code{\link[imputeTS]{na_interpolation}},
#' \code{\link[imputeTS]{na_kalman}}, \code{\link[imputeTS]{na_locf}},
#'  \code{\link[imputeTS]{na_ma}}, \code{\link[imputeTS]{na_mean}},
#'  \code{\link[imputeTS]{na_random}}, \code{\link[imputeTS]{na_replace}},
#'  \code{\link[imputeTS]{na_seadec}}
#'
#' @examples
#' # Example 1: Perform seasonal splitted imputation using algorithm = "interpolation"
#' na_seasplit(tsAirgap, algorithm = "interpolation")
#' 
#' # Example 2: Perform seasonal splitted imputation using algorithm = "mean"
#' na_seasplit(tsAirgap, algorithm = "mean")
#' 
#' # Example 3: Same as example 1, just written with pipe operator
#' tsAirgap %>% na_seasplit(algorithm = "interpolation")
#' 
#' @importFrom stats frequency ts
#' @importFrom magrittr %>%
#' @export
#' @name na_seasplit

na_seasplit <- function(x, algorithm = "interpolation", find_frequency = FALSE, maxgap = Inf, ...) {
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
      tryCatch(data[, i] <- na_seasplit(data[, i], algorithm, find_frequency, maxgap, ...), error = function(cond) {
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
    if (sum(!missindx) < 3) {
      stop("Input data needs at least 3 non-NA data point for applying na_seasplit")
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

    
    # 1.6 Checks and corrections for time series frequency
    
    # Try to findFrequency
    if (find_frequency == TRUE) {
      t <- as.vector(data)
      freq <- forecast::findfrequency(na_interpolation(t))
      if (freq > 1) {
        data <- ts(t, frequency = freq)
      }
    }
    
    if (stats::frequency(data) == 1) {
      warning("No seasonality information for dataset could be found, going on without decomposition.
              Setting find_frequency=TRUE might be an option.")
      data <- apply_base_algorithm(data, algorithm = algorithm, ...)
      return(data)
    }
    
    if (length(data) < stats::frequency(data) * 2) {
      warning("More than 2 complete periods needed to perform a seasonal split. The algorithm will go on without seasonal split.")
      data <- apply_base_algorithm(data, algorithm = algorithm, ...)
      return(data)
    }

    ##
    ## End Input Check and Transformation
    ##


    ##
    ## 2. Imputation Code
    ##

    for (i in 1:stats::frequency(data)) {

      # get indices for one season
      indices <- seq(from = i, to = length(data), by = stats::frequency(data))

      # Create time series just with one season
      ts_temp <- stats::ts(data[indices])

      # Apply algorithm on this season
      ts_temp <- apply_base_algorithm(ts_temp, algorithm = algorithm, ...)

      # Write result back into original time series
      data[indices] <- as.vector(ts_temp)
    }

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