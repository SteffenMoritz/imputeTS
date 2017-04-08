#' @title imputeTS-package description
#' @description 
#' The imputeTS package is a collection of algorithms and tools for univariate time series imputation.
#' 
#' 
#' @details The imputeTS package specializes on (univariate) time series imputation. 
#' It offers several different imputation algorithm implementations. Beyond the imputation algorithms 
#' the package also provides plotting and printing functions of missing data statistics.
#' 
#' The package is easy to use:
#' 
#' - To impute (fill all missing values) in a time series \code{x}, run:\cr
#' > \code{na.interpolation(x)} \cr
#'          
#' - To plot missing data statistics for a time series \code{x}, run:\cr
#' > \code{plotNA.distribution(x)}\cr
#'
#' - To print missing data statistics for a time series \code{x}, run:\cr
#' > \code{statsNA(x)}\cr
#' 
#' Every other imputation function (starting with na.'algorithm name') and plotting
#' function (starting with plotNA.'plot name') work the same way as in this example.
#'   
#' @name imputeTS-package
#' @references Moritz, Steffen, et al. "Comparison of different Methods for Univariate Time Series Imputation in R." arXiv preprint arXiv:1510.03924 (2015).
#' @docType package
#' @import stats
NULL

#' @useDynLib imputeTS
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("imputeTS", libpath)
}
