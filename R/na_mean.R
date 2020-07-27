#' @title Missing Value Imputation by Mean Value
#'
#' @description Missing value replacement by mean values. Different means
#' like median, mean, mode possible.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object in which missing values shall be replaced
#'
#' @param option Algorithm to be used. Accepts the following input:
#' \itemize{
#'    \item{"mean" - take the mean for imputation (default)}
#'    \item{"median" - take the median for imputation}
#'    \item{"mode" - take the mode for imputation}
#'    \item{"harmonic" - take the harmonic mean}
#'    \item{"geometric" - take the geometric mean}
#'    }
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
#' @details Missing values get replaced by overall mean values. The function
#' calculates the mean, median or mode over all the non-NA values and replaces
#' all NAs with this value. Option 'mode' replaces NAs with the most frequent
#' value in the time series. If two or more values occur equally frequent, the
#' function imputes with the lower value. That's why 'mode' is not the best
#' option for decimal values.
#'
#' @author Steffen Moritz
#'
#' @seealso \code{\link[imputeTS]{na_interpolation}},
#' \code{\link[imputeTS]{na_kalman}}, \code{\link[imputeTS]{na_locf}},
#'  \code{\link[imputeTS]{na_ma}},
#'  \code{\link[imputeTS]{na_random}}, \code{\link[imputeTS]{na_replace}},
#'  \code{\link[imputeTS]{na_seadec}}, \code{\link[imputeTS]{na_seasplit}}
#'
#' @examples
#' # Prerequisite: Create Time series with missing values
#' x <- ts(c(2, 3, 4, 5, 6, NA, 7, 8))
#'
#' # Example 1: Perform imputation with the overall mean
#' na_mean(x)
#'
#' # Example 2: Perform imputation with overall median
#' na_mean(x, option = "median")
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' x %>% na_mean()
#' @importFrom magrittr %>%
#' @importFrom stats median ts
#' @export
#'
na_mean <- function(x, option = "mean", maxgap = Inf) {
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
      tryCatch(data[, i] <- na_mean(data[, i], option, maxgap), error = function(cond) {
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
    if (all(missindx)) {
      stop("Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean")
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

    if (option == "median") {
      # Use Median
      median <- stats::median(data, na.rm = TRUE)
      data[missindx] <- median
    }
    else if (option == "mode") {
      # Calculate Mode
      temp <- table(as.vector(data))
      mode <- names(temp)[temp == max(temp)]
      mode <- (as.numeric(mode))[1]
      data[missindx] <- mode
    }
    else if (option == "mean") {
      # Use arithmeic Mean
      mean <- mean(data, na.rm = TRUE)
      data[missindx] <- mean
    }
    else if (option == "geometric") {
      # Use geometric Mean
      if (any(data == 0 | data < 0, na.rm = T)) {
        stop(
          "The input data contains 0 and/or negative values.\n",
          "The geometric and harmonic mean are not well defined for these cases.\n",
          "Please another option like e.g. option = 'mean' in this case."
        )
      }
      mean <- exp(mean(log(data), na.rm = TRUE))
      data[missindx] <- mean
    }
    else if (option == "harmonic") {
      # Use harmonic Mean
      if (any(data == 0 | data < 0, na.rm = T)) {
        stop(
          "The input data contains 0 and/or negative values.\n",
          "The geometric and harmonic mean are not well defined for these cases.\n",
          "Please another option like e.g. option = 'mean' in this case."
        )
      }
      mean <- 1 / mean(1 / data, na.rm = TRUE)
      data[missindx] <- mean
    }
    else {
      stop("Wrong 'option' parameter given, must be either: \n'mean', 'mode', 'median', 'harmonic' or 'geometric'")
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