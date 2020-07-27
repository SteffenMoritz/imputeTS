#' @title Missing Value Imputation by Kalman Smoothing and State Space Models
#'
#' @description Uses Kalman Smoothing on structural time series models
#' (or on the state space representation of an arima model) for imputation.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object in which missing values shall be replaced
#'
#' @param model Model to be used. With this parameter the State Space Model
#' (on which KalmanSmooth is performed) can be chosen. Accepts the following input:
#'
#' \itemize{
#'    \item{"auto.arima" - For using the state space representation of
#'    arima model (using \link[forecast]{auto.arima})}
#'
#'    \item{"StructTS" - For using a structural model fitted by maximum
#'     likelihood (using \link[stats]{StructTS}) }
#'    }
#'
#'  For both auto.arima and StructTS additional parameters for model building can
#'  be given with the \dots parameter
#'
#'  Additionally it is also possible to use a user created state space model
#'  (See code Example 5). This state space model could for example be
#'  obtained from another R package for structural time series modeling.
#'  Furthermore providing the state space representation of a arima model
#'  from \link[stats]{arima} is also possible. But it is important to note,
#'  that user created state space models must meet the requirements specified
#'  under \link[stats]{KalmanLike}. This means the user supplied state space
#'  model has to be in form of a list with at least components T, Z, h , V, a, P, Pn.
#'  (more details under \link[stats]{KalmanLike})
#'
#' @param smooth if \code{TRUE} - \code{\link[stats]{KalmanSmooth}} is used for
#' estimation, if \code{FALSE} - \code{\link[stats]{KalmanRun}} is used.
#' Since KalmanRun is often considered extrapolation KalmanSmooth is usually
#' the better choice for imputation.
#'
#' @param nit Parameter from Kalman Filtering (see \link[stats]{KalmanLike}).
#' Usually no need to change from default.
#'
#' @param maxgap Maximum number of successive NAs to still perform imputation on.
#'  Default setting is to replace all NAs without restrictions. With this
#'  option set, consecutive NAs runs, that are longer than 'maxgap' will
#'  be left NA. This option mostly makes sense if you want to
#'  treat long runs of NA afterwards separately.
#'
#' @param ... Additional parameters to be passed through to the functions that
#' build the State Space Models (\link[stats]{StructTS} or \link[forecast]{auto.arima}).
#'
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object (dependent on given input at parameter x)
#'
#' @details The KalmanSmoother used in this function is \code{\link[stats]{KalmanSmooth}}.
#' It operates either on a \code{Basic Structural Model} obtained by
#' \code{\link[stats]{StructTS}} or the state space representation of a ARMA model
#' obtained by \code{\link[forecast]{auto.arima}}.
#'
#' For an detailed explanation of Kalman Filtering and Space Space Models the
#' following literature is a good starting point:
#' \itemize{
#'    \item{\cite{G. Welch, G. Bishop, An Introduction to the Kalman Filter. SIGGRAPH 2001 Course 8, 1995}}
#'    \item{\cite{Harvey, Andrew C. Forecasting, structural time series models and the Kalman filter. Cambridge university press, 1990} }
#'    \item{\cite{Grewal, Mohinder S. Kalman filtering. Springer Berlin Heidelberg, 2011}}
#'    }
#'
#' @author Steffen Moritz
#' @seealso \code{\link[imputeTS]{na_interpolation}},
#'  \code{\link[imputeTS]{na_locf}},
#'  \code{\link[imputeTS]{na_ma}}, \code{\link[imputeTS]{na_mean}},
#'  \code{\link[imputeTS]{na_random}}, \code{\link[imputeTS]{na_replace}},
#'  \code{\link[imputeTS]{na_seadec}}, \code{\link[imputeTS]{na_seasplit}}
#'
#' @examples
#' # Example 1: Perform imputation with KalmanSmoother and state space representation of arima model
#' na_kalman(tsAirgap)
#' 
#' # Example 2: Perform imputation with KalmanRun and state space representation of arima model
#' na_kalman(tsAirgap, smooth = FALSE)
#' 
#' # Example 3: Perform imputation with KalmanSmooth and StructTS model
#' na_kalman(tsAirgap, model = "StructTS", smooth = TRUE)
#' 
#' # Example 4: Perform imputation with KalmanSmooth and StructTS model with additional parameters
#' na_kalman(tsAirgap, model = "StructTS", smooth = TRUE, type = "trend")
#' 
#' # Example 5:  Perform imputation with KalmanSmooth and user created model
#' usermodel <- arima(tsAirgap, order = c(1, 0, 1))$model
#' na_kalman(tsAirgap, model = usermodel)
#' 
#' # Example 6: Same as example 1, just written with pipe operator
#' tsAirgap %>% na_kalman()
#' 
#' @references Hyndman RJ and Khandakar Y (2008). "Automatic time series forecasting: the forecast package for R". Journal of Statistical Software, 26(3).
#' @importFrom stats StructTS KalmanSmooth KalmanRun arima
#' @importFrom forecast auto.arima
#' @importFrom magrittr %>%
#' @export

na_kalman <- function(x, model = "StructTS", smooth = TRUE, nit = -1, maxgap = Inf, ...) {
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
      tryCatch(data[, i] <- na_kalman(data[, i], model, smooth, nit, maxgap,...), error = function(cond) {
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
      stop("Input data needs at least 3 non-NA data point for applying na_kalman")
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

    # 1.6 Check if type of parameter smooth is correct
    if (!is.logical(smooth)) {
      stop("Parameter smooth must be of type logical ( TRUE / FALSE)")
    }

    # 1.7 Transformation to numeric as 'int' can't be given to KalmanRun
    data[1:length(data)] <- as.numeric(data)

    ##
    ## End Input Check and Transformation
    ##


    ##
    ## 2. Imputation Code
    ##

    # 2.1 Selection of state space model

    # State space representation of a arima model
    if (model[1] == "auto.arima") {
      mod <- forecast::auto.arima(data, ...)$model
    }
    # State space model, default is BSM - basic structural model
    else if (model[1] == "StructTS") {
      # Fallback, because for StructTS first value is not allowed to be NA
      if (is.na(data[1])) {
        data[1] <- na_locf(data, option = "nocb", na_remaining = "rev")[1]
      }
      mod <- stats::StructTS(data, ...)$model0
    }
    # User supplied model e.g. created with arima() or other state space models from other packages
    else {
      mod <- model
      if (length(mod) < 7) {
        stop("Parameter model has either to be \"StructTS\"/\"auto.arima\" or a user supplied model in
            form of a list with at least components T, Z, h , V, a, P, Pn specified")
      }

      if (is.null(mod$Z)) {
        stop("Something is wrong with the user supplied model. Either choose \"auto.arima\" or \"StructTS\"
             or supply a state space model with at least components T, Z, h , V, a, P, Pn as specified
             under Details on help page for KalmanLike")
      }
    }


    # 2.2 Selection if KalmanSmooth or KalmanRun

    if (smooth == TRUE) {
      kal <- stats::KalmanSmooth(data, mod, nit)
      erg <- kal$smooth # for kalmanSmooth
    }
    else {
      kal <- stats::KalmanRun(data, mod, nit)
      erg <- kal$states # for kalmanrun
    }

    # Check if everything is right with the model
    if (dim(erg)[2] != length(mod$Z)) {
      stop("Error with number of components $Z")
    }

    # 2.3 Getting Results

    # Out of all components in $states or$smooth only the ones
    # which have 1 or -1 in $Z are in the model
    # Therefore matrix multiplication is done
    karima <- erg[missindx, , drop = FALSE] %*% as.matrix(mod$Z)

    # Add imputations to the initial dataset
    data[missindx] <- karima

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