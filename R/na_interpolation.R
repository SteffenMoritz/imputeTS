#' @title Missing Value Imputation by Interpolation
#'
#' @description Uses either linear, spline or stineman interpolation
#' to replace missing values.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#'  object in which missing values shall be replaced
#'
#' @param option Algorithm to be used. Accepts the following input:
#' \itemize{
#'    \item{"linear" - for linear interpolation using \link{approx} } (default choice)
#'    \item{"spline" - for spline interpolation using \link{spline}}
#'    \item{"stine" - for Stineman interpolation using \link[stinepack]{stinterp}}
#'    }
#'
#' @param maxgap Maximum number of successive NAs to still perform imputation on.
#'  Default setting is to replace all NAs without restrictions. With this
#'  option set, consecutive NAs runs, that are longer than 'maxgap' will
#'  be left NA. This option mostly makes sense if you want to
#'  treat long runs of NA afterwards separately.
#'
#' @param ... Additional parameters to be passed through to \link{approx} or
#'  \link{spline} interpolation functions
#'
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#'  object (dependent on given input at parameter x)
#'
#' @details Missing values get replaced by values of \link{approx}, \link{spline}
#' or \link[stinepack]{stinterp} interpolation.
#' 
#'  The na_interpolation function also supports the use of additional parameters from the respective
#'  underlying interpolation functions. While usually not really needed, it is useful to know that 
#'  this advanced use is in principle possible. These additional parameters are not specified explicitly 
#'  in the na_interpolation function documentation. Take a look into the documentation of the \link[stinepack]{stinterp}, \link{approx} and \link{spline} functions to get an overview about these additional parameters.
#'  
#'  An example for such a parameter is the 'method' argument of spline, which can be used to
#'  further specify the type of spline to be used. Possible values are "fmm", "natural", 
#'  "periodic", "monoH.FC" and "hyman" (as can be seen in the \link{spline}
#'   documentation). The respective function call using this additional parameter would  
#'   look like this: 
#'   \code{na_interpolation(x, option ="spline", method ="natural")}
#'   
#'  Like in this example other additional detail parameters (gained from \link{approx}, 
#'  \link{spline}, \link[stinepack]{stinterp} documentation) can be used by just including 
#'  them in the na_interpolation function call. As already mentioned, these advanced possibilities
#'  for settings parameters are only helpful for specific use cases. For regular use
#'  the standard parameters provided directly in the na_interpolation documentation should be
#'  more than enough.
#'  
#'
#' @author Steffen Moritz, Ron Hause
#'
#' @seealso  \code{\link[imputeTS]{na_kalman}}, \code{\link[imputeTS]{na_locf}},
#'  \code{\link[imputeTS]{na_ma}}, \code{\link[imputeTS]{na_mean}},
#'  \code{\link[imputeTS]{na_random}}, \code{\link[imputeTS]{na_replace}},
#'  \code{\link[imputeTS]{na_seadec}}, \code{\link[imputeTS]{na_seasplit}}
#'
#' @examples
#' # Prerequisite: Create Time series with missing values
#' x <- ts(c(2, 3, 4, 5, 6, NA, 7, 8))
#'
#' # Example 1: Perform linear interpolation
#' na_interpolation(x)
#'
#' # Example 2: Perform spline interpolation
#' na_interpolation(x, option = "spline")
#'
#' # Example 3: Perform stine interpolation
#' na_interpolation(x, option = "stine")
#' 
#' # Example 4: Perform linear interpolation, with additional parameter pass through from spline()
#' # Take a look at the 'Details' section of the na_interpolation documentation for more information about advanced parameter pass through options
#' na_interpolation(x, option ="spline", method ="natural")
#' 
#' # Example 5: Same as example 1, just written with pipe operator
#' x %>% na_interpolation()
#'
#' # Example 6: Same as example 2, just written with pipe operator
#' x %>% na_interpolation(option = "spline")
#' @references Johannesson, Tomas, et al. (2015). "Package stinepack".
#' @importFrom stats ts approx spline
#' @importFrom methods hasArg
#' @importFrom stinepack stinterp
#' @importFrom magrittr %>%
#' @export

na_interpolation <- function(x, option = "linear", maxgap = Inf, ...) {

  # Variable 'data' is used for all transformations to the time series
  # 'x' needs to stay unchanged to be able to return the same ts class in the end
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
      tryCatch(
        data[, i] <- na_interpolation(data[, i], option, maxgap),
        error = function(cond) {
          warning(paste(
            "na_interpolation: No imputation performed for column", i, "of the input dataset.
                Reason:", cond[1]
          ), call. = FALSE)
        }
      )
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
      return(x)
    }

    # 1.2 special handling data types
    if (any(class(data) == "tbl")) {
      data <- as.vector(as.data.frame(data)[, 1])
    }

    # 1.3 Check for algorithm specific minimum amount of non-NA values
    if (sum(!missindx) < 2) {
      stop("At least 2 non-NA data points required in the time series to apply na_interpolation.")
    }

    # 1.4 Checks and corrections for wrong data dimension

    # Check if input dimensionality is not as expected
    if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
      stop("Wrong input type for parameter x.")
    }

    # Altering multivariate objects with 1 column (which are essentially
    # univariate) to be dim = NULL
    if (!is.null(dim(data)[2])) {
      data <- data[, 1]
    }

    # 1.5 Check if input is numeric
    if (!is.numeric(data)) {
      stop("Input x is not numeric.")
    }

    ##
    ## End Input Check
    ##


    ##
    ## 2. Imputation Code
    ##

    n <- length(data)

    allindx <- 1:n
    indx <- allindx[!missindx]

    data_vec <- as.vector(data)

    # Linear Interpolation
    if (option == "linear") {
      # Check if 'rule' is used in function call, to allow parameter pass through for rule
      # Needed since parameter pass through via (...) to approx does not work, when value for 'rule' is also set in the code. 
      if (methods::hasArg(rule)) {
        interp <- stats::approx(indx, data_vec[indx], 1:n, ...)$y
      }
      else {
        interp <- stats::approx(indx, data_vec[indx], 1:n, rule = 2, ...)$y
      }
    }
    # Spline Interpolation
    else if (option == "spline") {
      interp <- stats::spline(indx, data_vec[indx], n = n, ...)$y
    }
    # Stineman Interpolation
    else if (option == "stine") {
      interp <- stinepack::stinterp(indx, data_vec[indx], 1:n, ...)$y
      # avoid NAs at the beginning and end of series // same behavior like
      # for approx with rule = 2.
      if (any(is.na(interp))) {
        interp <- na_locf(interp, na_remaining = "rev")
      }
    }
    # Wrong parameter option
    else {
      stop("Wrong parameter 'option' given. Value must be either 'linear', 'spline' or 'stine'.")
    }

    # Merge interpolated values back into original time series
    data[missindx] <- interp[missindx]

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
