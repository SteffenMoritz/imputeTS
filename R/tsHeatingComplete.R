#' @title Time series of a heating systems supply temperature (complete)
#'
#' @description Time series of a heating systems supply temperature. Measured from 18.11.2013 - 05:12:00 to 13.01.2015 - 15:08:00 in 1 minute steps.
#' This time series provides the truth for the missing values of the \code{\link{tsHeating}} time series. Thus it is identical
#' to the heating time series except that no value is missing.
#' 
#' @details The time series originates from the GECCO Industrial Challenge 2015.
#' This Challenge was about "Recovering missing information in heating system operating data".
#' Goal was to impute missing values in heating system sensor data as accurate as possible.
#' (\url{http://www.spotseven.de/gecco-challenge/gecco-challenge-2015/})
#'
#' In order to use this series for comparing imputation algorithm results,
#' there are two time series provided. One series without missing values, which can
#' be used as ground truth. Another series with NAs, on which the imputation 
#' algorithms can be applied. The NAs thereby were inserted according to patterns
#' found in similar time series.
#' 
#' There are the two time series:
#' \itemize{
#'   \item tsHeating - The time series with NAs.
#'
#'   \item tsHeatingComplete - Time series without NAs.
#' }
#' @docType data
#' @keywords datasets
#' @seealso \code{\link[imputeTS]{tsAirgap}}, \code{\link[imputeTS]{tsNH4}}
#' @name tsHeatingComplete
#' @usage tsHeatingComplete
#' @source \url{http://www.spotseven.de/gecco-challenge/gecco-challenge-2015/}
#' @format Time Series (\code{\link{ts}}) with 606837 rows.
NULL