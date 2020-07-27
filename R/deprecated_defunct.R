#--------------------------------------------------------------#
# Collection of DEPRECATED AND DEFUNCT FUNCTIONS
#--------------------------------------------------------------#



#--------------------------------------------------------------#
# IMPUTATION FUNCTIONS
# Old na. imputation functions, replaced by na_ 
# Deprecated since Version 3.0 (2019-07-01)
#--------------------------------------------------------------#


# na.interpolation()
# replaced by na_interpolation
#--------------------------------------------------------------------------------------#
#' Deprecated use \code{\link[imputeTS]{na_interpolation}} instead.
#' @description na.interpolation is replaced by \code{\link[imputeTS]{na_interpolation}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams na_interpolation
#' @keywords internal
#' @export
na.interpolation <- function(x, option = "linear", maxgap = Inf, ...) {
  .Deprecated(
    new = "na_interpolation",
    old = "na.interpolation",
    msg = "na.interpolation will be replaced by na_interpolation.
           Functionality stays the same.
           The new function name better fits modern R code style guidelines.
           Please adjust your code accordingly."
  )
  na_interpolation(x, option, maxgap, ...)
}
#--------------------------------------------------------------------------------------#



# na.kalman()
# replaced by na_kalman
#--------------------------------------------------------------------------------------#
#' Deprecated use \code{\link[imputeTS]{na_kalman}} instead.
#' @description na.kalman is replaced by \code{\link[imputeTS]{na_kalman}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams na_kalman
#' @keywords internal
#' @export
na.kalman <- function(x, model = "StructTS", smooth = TRUE, nit = -1, maxgap = Inf, ...) {
  .Deprecated(
    new = "na_kalman",
    old = "na.kalman",
    msg = "na.kalman will be replaced by na_kalman.
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  na_kalman(x, model, smooth, nit, maxgap, ...)
}
#--------------------------------------------------------------------------------------#



# na.locf()
# replaced by na_locf
#--------------------------------------------------------------------------------------#
#' Deprecated use \code{\link[imputeTS]{na_locf}} instead.
#' @description na.locf is replaced by \code{\link[imputeTS]{na_locf}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams na_locf
#' @keywords internal
#' @export
na.locf <- function(x, option = "locf", na.remaining = "rev", maxgap = Inf, ...) {
  .Deprecated(
    new = "na_locf",
    old = "na.locf",
    msg = "na.locf will be replaced by na_locf.
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  na_locf(x, option, na.remaining, maxgap, ...)
}
#--------------------------------------------------------------------------------------#



# na.ma()
# replaced by na_ma
#--------------------------------------------------------------------------------------#
#' Deprecated use \code{\link[imputeTS]{na_ma}} instead.
#' @description na.ma is replaced by \code{\link[imputeTS]{na_ma}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams na_ma
#' @keywords internal
#' @export
na.ma <- function(x, k = 4, weighting = "exponential", maxgap = Inf, ...) {
  .Deprecated(
    new = "na_ma",
    old = "na.ma",
    msg = "na.ma will be replaced by na_ma.
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  na_ma(x, k, weighting, maxgap, ...)
}
#--------------------------------------------------------------------------------------#



# na.mean()
# replaced by na_mean
#--------------------------------------------------------------------------------------#
#' Deprecated use \code{\link[imputeTS]{na_mean}} instead.
#' @description na.mean is replaced by \code{\link[imputeTS]{na_mean}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams na_mean
#' @keywords internal
#' @export
na.mean <- function(x, option = "mean", maxgap = Inf, ...) {
  .Deprecated(
    new = "na_mean",
    old = "na.mean",
    msg = "na.mean will be replaced by na_mean.
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  na_mean(x, option, maxgap, ...)
}
#--------------------------------------------------------------------------------------#



# na.random()
# replaced by na_random
#--------------------------------------------------------------------------------------#
#' Deprecated use \code{\link[imputeTS]{na_random}} instead.
#' @description na.random is replaced by \code{\link[imputeTS]{na_random}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams na_random
#' @keywords internal
#' @export
na.random <- function(x, lower_bound = NULL, upper_bound = NULL, maxgap = Inf, ...) {
  .Deprecated(
    new = "na_random",
    old = "na.random",
    msg = "na.random will be replaced by na_random.
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  na_random(x, lower_bound, upper_bound, maxgap, ...)
}
#--------------------------------------------------------------------------------------#



# na.remove()
# replaced by na_remove
#--------------------------------------------------------------------------------------#
#' Deprecated use \code{\link[imputeTS]{na_remove}} instead.
#' @description na.remove is replaced by \code{\link[imputeTS]{na_remove}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams na_remove
#' @keywords internal
#' @export
na.remove <- function(x, ...) {
  .Deprecated(
    new = "na_remove",
    old = "na.remove",
    msg = "na.remove will be replaced by na_remove.
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  na_remove(x, ...)
}
#--------------------------------------------------------------------------------------#



# na.replace()
# replaced by na_replace
#--------------------------------------------------------------------------------------#
#' Deprecated use \code{\link[imputeTS]{na_replace}} instead.
#' @description na.replace is replaced by \code{\link[imputeTS]{na_replace}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams na_replace
#' @keywords internal
#' @export
na.replace <- function(x, fill = 0, maxgap = Inf, ...) {
  .Deprecated(
    new = "na_replace",
    old = "na.replace",
    msg = "na.replace will be replaced by na_replace.
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  na_replace(x, fill, maxgap, ...)
}
#--------------------------------------------------------------------------------------#



# na.seadec()
# replaced by na_seadec
#--------------------------------------------------------------------------------------#
#' Deprecated use \code{\link[imputeTS]{na_seadec}} instead.
#' @description na.seadec is replaced by \code{\link[imputeTS]{na_seadec}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams na_seadec
#' @keywords internal
#' @export
na.seadec <- function(x, algorithm = "interpolation", find_frequency = FALSE, maxgap = Inf, ...) {
  .Deprecated(
    new = "na_seadec",
    old = "na.seadec",
    msg = "na.seadec will be replaced by na_seadec.
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  na_seadec(x, algorithm, find_frequency, maxgap, ...)
}
#--------------------------------------------------------------------------------------#



# na.seasplit()
# replaced by na_seasplit
#--------------------------------------------------------------------------------------#
#' Deprecated use \code{\link[imputeTS]{na_seasplit}} instead.
#' @description na.seasplit is replaced by \code{\link[imputeTS]{na_seasplit}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams na_seasplit
#' @keywords internal
#' @export
na.seasplit <- function(x, algorithm = "interpolation", find_frequency = FALSE, maxgap = Inf, ...) {
  .Deprecated(
    new = "na_seasplit",
    old = "na.seasplit",
    msg = "na.seasplit will be replaced by na_seasplit.
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  na_seasplit(x, algorithm, find_frequency, maxgap, ...)
}
#--------------------------------------------------------------------------------------#






#--------------------------------------------------------------#
# PLOTTING FUNCTIONS
# Old plotNA. visualization functions, replaced by ggplot_na_ 
# Deprecated since Version 3.1 (2020-07-30)
#--------------------------------------------------------------#



# plotNA.distribution()
# replaced by ggplot_na_distribution
#--------------------------------------------------------------------------------------#
#' @title Discontinued - Use \code{\link[imputeTS]{ggplot_na_distribution}} instead.
#' @description plotNA.distribution was replaced by \code{\link[imputeTS]{ggplot_na_distribution}}.
#' The new plotting function provides an improved version of the old plot, e.g. it looks better now and is better adjustable, 
#' because it is based on ggplot2. If you absolutely want to use the old function, 
#' you need to download an older package version. Versions 3.0 and below still have the old functions.
#' @keywords internal
#' @export

plotNA.distribution <- function(x, ... ) {
  
  .Defunct(
    new = "ggplot_na_distribution",
    msg = "
    plotNA.distribution was replaced by ggplot_na_distribution.
    Use this function instead.
    
    The plot itself is the same, but looks better now and is better adjustable, because it is based on ggplot2.
    
    If you absolutely want to use the old function, you need to manually download an older package version.
    (Versions 3.0 and below still have the old functions)"
  )
}
#--------------------------------------------------------------------------------------#




# plotNA.distributionBar()
# replaced by ggplot_na_intervals
#--------------------------------------------------------------------------------------#
#' @title Discontinued - Use \code{\link[imputeTS]{ggplot_na_intervals}} instead.
#' @description plotNA.distributionBar was replaced by \code{\link[imputeTS]{ggplot_na_intervals}}.
#' The new plotting function provides an improved version of the old plot e.g. it looks better now and is better adjustable, 
#' because it is based on ggplot2. If you absolutely want to use the old function, 
#' you need to download an older package version. Versions 3.0 and below still have the old functions.
#' @keywords internal
#' @export

plotNA.distributionBar <- function(x, ... ) {
  
  .Defunct(
    new = "ggplot_na_intervals",
    msg = "
    plotNA.distributionBar was replaced by ggplot_na_intervals.
    Use this function instead.
    
    The plot itself is the same, but looks better now and is better adjustable, because it is based on ggplot2.
    
    If you absolutely want to use the old function, you need to manually download an older package version.
    (Versions 3.0 and below still have the old functions)"
  )
  
}
#--------------------------------------------------------------------------------------#




# plotNA.gapsize()
# replaced by ggplot_na_gapsize
#--------------------------------------------------------------------------------------#
#' @title Discontinued - Use \code{\link[imputeTS]{ggplot_na_gapsize}} instead.
#' @description plotNA.gapsize was replaced by \code{\link[imputeTS]{ggplot_na_gapsize}}.
#' The new plotting function provides an improved version of the old plot e.g. it looks better now and is better adjustable, 
#' because it is based on ggplot2. If you absolutely want to use the old function, 
#' you need to download an older package version. Versions 3.0 and below still have the old functions.
#' @keywords internal
#' @export

plotNA.gapsize <- function(x, ... ) {
  
  .Defunct(
    new = "ggplot_na_gapsize",
    msg = "
    plotNA.gapsize was replaced by ggplot_na_gapsize.
    Use this function instead.
    
    The plot itself is the same, but looks better now and is better adjustable, because it is based on ggplot2.
    
    If you absolutely want to use the old function, you need to manually download an older package version.
    (Versions 3.0 and below still have the old functions)"
  )
  
}
#--------------------------------------------------------------------------------------#



# plotNA.imputations()
# replaced by ggplot_na_imputations
#--------------------------------------------------------------------------------------#
#' @title Discontinued - Use \code{\link[imputeTS]{ggplot_na_imputations}} instead.
#' @description plotNA.imputations was replaced by \code{\link[imputeTS]{ggplot_na_imputations}}.
#' The new plotting function provides an improved version of the old plot e.g. it looks better now and is better adjustable, 
#' because it is based on ggplot2. If you absolutely want to use the old function, 
#' you need to download an older package version. Versions 3.0 and below still have the old functions.
#' @keywords internal
#' @export

plotNA.imputations <- function(x, ... ) {
  
  .Defunct(
    new = "ggplot_na_imputations",
    msg = "
    plotNA.imputations was replaced by ggplot_na_imputations.
    Use this function instead.
    
    The plot itself is the same, but looks better now and is better adjustable, because it is based on ggplot2.
    
    If you absolutely want to use the old function, you need to manually download an older package version.
    (Versions 3.0 and below still have the old functions)"
  )
  
}
#--------------------------------------------------------------------------------------#