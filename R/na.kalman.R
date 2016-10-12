#' @title Missing Value Imputation by Kalman Smoothing and State Space Models
#' 
#' @description Uses Kalman Smoothing on structural time series models (or on the state space representation of an arima model) for imputation.
#'  
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' 
#' @param model Model to be used. With this parameter the State Space Model (on which KalmanSmooth is performed) can be chosen. Accepts the following input:
#' \itemize{
#'    \item{"auto.arima" - For using the state space representation of arima model (using \link[forecast]{auto.arima})}
#'    \item{"StructTS" - For using a structural model fitted by maximum likelihood (using \link[stats]{StructTS}) }
#'    }
#'  
#'  For both auto.arima and StructTS additional parameters for model building can be given with the \dots parameter  
#'    
#'  Additionally it is also possible to use a user created state space model (See code Example 5). This state space model could for example be obtained from another
#'  R package for structural time series modeling. Furthermore providing the state space representation of a arima model from \link[stats]{arima}
#'  is also possible. But it is important to note, that user created state space models must meet the requirements specified under \link[stats]{KalmanLike}. This means the user supplied state space model has to be in form of a list with at least components T, Z, h , V, a, P, Pn. 
#'  (more details under \link[stats]{KalmanLike})
#'  
#' @param smooth if \code{TRUE} - \code{\link[stats]{KalmanSmooth}} is used for estimation, if \code{FALSE} - \code{\link[stats]{KalmanRun}} is used. Since KalmanRun is often considered extrapolation KalmanSmooth is usually the better choice for imputation.
#'
#' @param nit Parameter from Kalman Filtering (see \link[stats]{KalmanLike}). Usually no need to change from default.
#' 
#' @param ... Additional parameters to be passed through to the functions that build the State Space Models (\link[stats]{StructTS} or \link[forecast]{auto.arima}).
#' 
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object (dependent on given input at parameter x)
#' 
#' @details The KalmanSmoother used in this function is \code{\link[stats]{KalmanSmooth}}.
#' It operates either on a \code{Basic Structural Model} obtained by \code{\link[stats]{StructTS}} or
#' the state space representation of a ARMA model obtained by \code{\link[forecast]{auto.arima}}.
#' 
#' For an detailed explanation of Kalman Filtering and Space Space Models the following literature is a good starting point:
#' \itemize{
#'    \item{\cite{G. Welch, G. Bishop, An Introduction to the Kalman Filter. SIGGRAPH 2001 Course 8, 1995}}
#'    \item{\cite{Harvey, Andrew C. Forecasting, structural time series models and the Kalman filter. Cambridge university press, 1990} }
#'    \item{\cite{Grewal, Mohinder S. Kalman filtering. Springer Berlin Heidelberg, 2011}}
#'    }
#'    
#' @author Steffen Moritz
#' @seealso \code{\link[imputeTS]{na.interpolation}},
#'  \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.ma}}, \code{\link[imputeTS]{na.mean}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}},
#'  \code{\link[imputeTS]{na.seadec}}, \code{\link[imputeTS]{na.seasplit}}
#'  
#' @examples
#' #Example 1: Perform imputation with KalmanSmoother and state space representation of arima model
#' na.kalman(tsAirgap)
#' 
#' #Example 2: Perform imputation with KalmanRun and state space representation of arima model
#' na.kalman(tsAirgap, smooth = FALSE)
#'
#' #Example 3: Perform imputation with KalmanSmooth and StructTS model
#' na.kalman(tsAirgap, model ="StructTS", smooth = TRUE) 
#' 
#' #Example 4: Perform imputation with KalmanSmooth and StructTS model with additional parameters 
#' na.kalman(tsAirgap, model ="StructTS", smooth = TRUE, type ="trend") 
#' 
#' #Example 5:  Perform imputation with KalmanSmooth and user created model
#' usermodel <- arima(tsAirgap,order = c(1,0,1))$model
#' na.kalman(tsAirgap,model = usermodel)
#' 
#' @references Hyndman RJ and Khandakar Y (2008). "Automatic time series forecasting: the forecast package for R". Journal of Statistical Software, 26(3).
#' @import stats 
#' @import forecast
#' @export

na.kalman <- function(x, model = "StructTS" , smooth =TRUE,nit=-1, ...) { 
  
  
  data <- x
  
  # Multivariate Input Handling (loop through all columns)
  # No imputation code in this part. 
  if (!is.null( dim(data)[2]) && dim(data)[2] != 1  ) {
    for (i in 1:dim(data)[2]) {
      #if imputing a column does not work (mostly because it is not numeric) the column is left unchanged
      tryCatch(data[,i] <- na.kalman(data[ ,i], model, smooth, nit, ...), error=function(cond) {
        warning(paste("imputeTS: No imputation performed for column",i,"because of this",cond), call. = FALSE)
      })
    }
    return(data)
  }
  
  # Univariate Input
  # All imputation code is within this part
  else {
    
    ##
    ## Input check
    ## 
    
    if(!anyNA(data)) {
      return(data)
    }
    
    if(!is.numeric(data))
    {stop("Input x is not numeric")}
    
    if(!is.null(dim(data)))
    {stop("Wrong input type for parameter x")}
    
    if(!is.logical(smooth)) {
      stop("Parameter smooth must be of type logical ( TRUE / FALSE)")
    }
  
  
    ##
    ## Imputation Code
    ##
    
    missindx <- is.na(data)
    
    ##Selection of state space model
    
    #State space representation of a arima model 
    if (model =="auto.arima") {
      mod <- auto.arima(data,...)$model
    }
    #State space model, default is BSM - basic structural model
    else if(model == "StructTS") {
      #Fallback, because for StructTS first value is not allowed to be NA
      if(is.na(data[1])) {data[1] <- na.locf(data,option = "nocb",na.remaining = "rev")[1]}
      mod <- StructTS(data,...)$model0
    }
    #User supplied model e.g. created with arima() or other state space models from other packages
    else {
      mod <- model
      if (length(mod) < 7)
          {stop("Parameter model has either to be \"StructTS\"/\"auto.arima\" or a user supplied model in 
            form of a list with at least components T, Z, h , V, a, P, Pn specified")
      }
      
      if(is.null(mod$Z)) {
        stop("Something is wrong with the user supplied model. Either choose \"auto.arima\" or \"StructTS\"
             or supply a state space model with at least components T, Z, h , V, a, P, Pn as specified 
             under Details on help page for KalmanLike")
      }
    }
    
    #Selection if KalmanSmooth or KalmanRun
    if (smooth ==TRUE) {
      kal <- KalmanSmooth(data, mod, nit )
      erg <- kal$smooth  #for kalmanSmooth
    }
    else {
      kal <- KalmanRun(data, mod, nit )
      erg <- kal$states #for kalmanrun
    }
    
    #Check if everything is right with the model
    if(dim(erg)[2]!= length(mod$Z)){
      stop("Error with number of components $Z")
    }
    
    #Out of all components in $states or$smooth only the ones
    #which have 1 or -1 in $Z are in the model
    #Therefore matrix multiplication is done
    for ( i in 1:length(mod$Z)) {
      erg[,i] = erg[,i] * mod$Z[i]
    }
    #Add remaining values in the rows
    karima <-rowSums(erg)
    
    #Add imputations to the initial dataset
    for (i in 1:length(data)) {
      if (is.na(data[i])) {
        data[i] <- karima[i]
      }
    }
    return(data)
  }
}