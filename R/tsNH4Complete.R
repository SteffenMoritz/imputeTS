#' @title Time series of NH4 concentration in a wastewater system (complete)
#' 
#' @description Time series of NH4 concentration in a wastewater system. Measured from 30.11.2010 - 16:10 to 01.01.2011 - 6:40 in 10 minute steps.
#' This time series provides the truth for the missing values of the \code{\link{tsNH4}} time series. Thus it is identical
#' to the heating time series except that no value is missing.

#' @details The time series is derived from the dataset of the  GECCO Industrial Challenge 2014.
#' 
#' In order to use this series for comparing imputation algorithm results,
#' there are two time series provided. One series without missing values, which can
#' be used as ground truth. Another series with NAs, on which the imputation 
#' algorithms can be applied. The NAs thereby were inserted according to patterns
#' found in similar time series.
#' 
#' There are the two time series:
#' \itemize{
#'   \item tsNH4 - The time series with NAs.
#'
#'   \item tsNH4Complete - Time series without NAs.
#' }
#' @docType data
#' @keywords datasets
#' @seealso \code{\link[imputeTS]{tsAirgap}},\code{\link[imputeTS]{tsHeating}}
#' @name tsNH4Complete
#' @usage tsNH4Complete
#' 
#' @source \cite{Friese, Martina, Fischbach, Andreas, Flasch, Oliver, Mersmann, Olaf,
#' Bartz-Beielstein, Thomas, and Walbeck, Klaus. (2014, July 16). 
#' GECCO Industrial Challenge 2014 Dataset: A water quality dataset for the 
#' 'Active protection against pollution of the surface water' competition at the 
#' Genetic and Evolutionary Computation Conference 2015, Vancouver, Canada. 
#' http://www.spotseven.de/gecco-challenge/gecco-challenge-2014}#' 
#' 
#' @format Time Series (\code{\link{ts}}) with 4552 rows.
NULL