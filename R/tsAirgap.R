#' @title Time series of monthly airline passengers (with NAs)
#'
#' @description Monthly totals of international airline passengers, 1949 to 1960.
#' This time series contains missing values. In the package included is also the \code{\link{tsAirgapComplete}} time series providing the true values for the
#' missing values.
#' 
#' @details The dataset originates from Box & Jenkins (see citation) and is a commonly used example in
#' time series analysis literature.
#' 
#' It characteristics (strong trend, strong seasonal behavior) make it also a great 
#' example for time series imputation.
#' Thus the version with inserted NA gaps was created under the name tsAirgap.
#'
#' In order to use this series for comparing imputation algorithm results,
#' there are two time series provided. One series without missing values, which can
#' be used as ground truth. Another series with NAs, on which the imputation 
#' algorithms can be applied. 
#' 
#' There are the two time series:
#' \itemize{
#'   \item tsAirgap - The time series with NAs.
#'
#'   \item tsAirgapComplete - Time series without NAs.
#' }
#' @docType data
#' @keywords datasets
#' @seealso \code{\link[imputeTS]{tsHeating}}, \code{\link[imputeTS]{tsNH4}}
#' @name tsAirgap
#' @usage tsAirgap
#' 
#' 
#' @source \cite{Box, G. E. P., Jenkins, G. M., Reinsel, G. C. and Ljung, G. M. (2015). Time series analysis: forecasting and control. Fifth Edition. John Wiley \& Sons.}
#' @format Time Series (\code{\link{ts}}) with 144 rows including 13 NAs.
NULL