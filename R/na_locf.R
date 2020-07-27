#' @title Missing Value Imputation by Last Observation Carried Forward
#'
#' @description Replaces each missing value with the most recent present value
#'  prior to it (Last Observation Carried Forward- LOCF). Optionally this can
#'  also be done starting from the back of the series (Next Observation Carried
#'  Backward - NOCB).
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object in which missing values shall be replaced
#'
#' @param option Algorithm to be used. Accepts the following input:
#' \itemize{
#'    \item{"locf" - for Last Observation Carried Forward}
#'    \item{"nocb" - for Next Observation Carried Backward}
#'    }
#'
#' @param na_remaining Method to be used for remaining NAs.
#' \itemize{
#'    \item{"keep" - to return the series with NAs}
#'    \item{"rm" - to remove remaining NAs}
#'    \item{"mean" - to replace remaining NAs by overall mean}
#'    \item{"rev" - to perform nocb / locf from the reverse direction}
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
#' @details Replaces each missing value with the most recent present value
#' prior to it (Last Observation Carried Forward- LOCF). This can also be
#' done from the reverse direction -starting from the back (Next Observation
#' Carried Backward - NOCB). Both options have the issue, that NAs at the
#' beginning (or for nocb at the end) of the time series cannot be imputed
#' (since there is no last value to be carried forward present yet). In this
#' case there are remaining NAs in the imputed time series. Since this only
#' concerns very few values at the beginning of the series, na_remaining
#' offers some quick solutions to get a series without NAs back.
#'
#' @author Steffen Moritz
#'
#' @seealso  \code{\link[imputeTS]{na_interpolation}},
#' \code{\link[imputeTS]{na_kalman}},
#'  \code{\link[imputeTS]{na_ma}}, \code{\link[imputeTS]{na_mean}},
#'  \code{\link[imputeTS]{na_random}}, \code{\link[imputeTS]{na_replace}},
#'  \code{\link[imputeTS]{na_seadec}}, \code{\link[imputeTS]{na_seasplit}}
#'
#' @examples
#' # Prerequisite: Create Time series with missing values
#' x <- ts(c(NA, 3, 4, 5, 6, NA, 7, 8))
#' 
#' # Example 1: Perform LOCF
#' na_locf(x)
#' 
#' # Example 2: Perform NOCF
#' na_locf(x, option = "nocb")
#' 
#' # Example 3: Perform LOCF and remove remaining NAs
#' na_locf(x, na_remaining = "rm")
#' 
#' # Example 4: Same as example 1, just written with pipe operator
#' x %>% na_locf()
#' @importFrom stats ts
#' @importFrom magrittr %>%
#' @export

na_locf <- function(x, option = "locf", na_remaining = "rev", maxgap = Inf) {
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
      tryCatch(data[, i] <- na_locf(data[, i], option, na_remaining, maxgap), error = function(cond) {
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
      stop("Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_locf")
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

    # 2.1 Perform locf or nocb

    # Input as vector
    data_vec <- as.vector(data)

    # Last observation carried forward // f = 0
    if (option == "locf") {
      imputed <- locf(data_vec, FALSE)
    }
    # Next observation carried backward // f = 1
    else if (option == "nocb") {
      imputed <- locf(data_vec, TRUE)
    }
    # Wrong input
    else {
      stop("Wrong parameter 'option' given. Value must be either 'locf' or 'nocb'.")
    }

    data[missindx] <- imputed[missindx]


    # 2.2 Handle remaining NAs - na_remaining param

    # no remaining NAs or keep NAs selected -> do nothing
    if (!anyNA(data) || na_remaining == "keep") {
      # do nothing
    }
    # Replace NAs through locf/nocb from the other direction
    else if (na_remaining == "rev") {
      if (option == "locf") {
        data <- na_locf(data, option = "nocb")
      }
      else if (option == "nocb") {
        data <- na_locf(data, option = "locf")
      }
    }
    # Remove all NAs
    else if (na_remaining == "rm") {
      data <- na_remove(data)
    }
    # Replace NAs with overall mean
    else if (na_remaining == "mean") {
      data <- na_mean(data)
    }
    # Wrong Input
    else {
      stop("Wrong parameter 'na_remaining' given. Value must be either 'keep', 'rm', 'mean' or 'rev'.")
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