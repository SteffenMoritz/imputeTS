#' @title Visualize Imputed Values
#'
#' @description Visualize the imputed values in a time series.
#'
#' @param x_with_na Numeric Vector or Time Series (\code{\link{ts}}) object with NAs before imputation
#' @param x_with_imputations Numeric Vector or Time Series (\code{\link{ts}}) object with NAs replaced by imputed values
#' @param x_with_truth Numeric Vector or Time Series (\code{\link{ts}}) object with the real values. (can be set to NULL if not known)
#'
#' @param legend If TRUE a legend is added at the bottom
#'
#' @param xlab Label for x axis
#' @param ylab Label for y axis
#'
#' @param main Main title
#' 
#' @param color_points Color of normal non-NA observations
#' @param color_lines Color of lines connecting non-NA observations
#' @param color_imputations Color of imputed values
#' @param color_truth Color of real values (truth) for the NA values
#'
#' @param shape_points Either an integer specifying a symbol or a single character to be used as the default in plotting points.
#' @param size_points A numerical value giving the size of points.
#' @param size_lines A numerical value giving the size of lines.
#' @param linetype Linetype used for connecting the points
#' 
#' @param theme Set a theme for ggplot2. Default is \code{\link[ggplot2]{theme_minimal}}
#'
#' @param ... Additional graphical parameters can be passed to \code{\link[ggplot2]{geom_line}}
#'
#' @details This plot can be used, to visualize imputed values for a time series.
#' Imputed values (filled NA gaps) are shown in a different color than the other values.
#' If real values (truth) for the NA gaps are known, they are added in a different color.
#'
#' @author Steffen Moritz
#'
#'
#' @seealso \code{\link[imputeTS]{plotNA_distribution}},\code{\link[imputeTS]{plotNA_distributionBar}},
#'  \code{\link[imputeTS]{plotNA_gapsize}}
#'
#' @examples
#' # Example 1: Visualize the values that were imputed by na.mean in the time series
#' impMean.Airgap <- na_mean(tsAirgap)
#' plotNA_imputations(tsAirgap, impMean.Airgap)
#'
#'
#' # Example 2: Visualize the imputed values by na_locf and the true values in the time series
#' impLOCF.Airgap <- na_locf(tsAirgap)
#' plotNA_imputations(tsAirgap, impLOCF.Airgap, tsAirgapComplete)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' tsAirgap %>%
#'   na_mean() %>%
#'   plotNA_imputations(x_with_na = tsAirgap)
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot geom_line geom_point aes
#' theme_minimal theme element_text element_blank xlab ylab ggtitle scale_color_manual
#' @export
plotNA_imputations <- function(x_with_na, x_with_imputations, x_with_truth = NULL,
                               xlab = "Time", ylab = "Value", legend = TRUE,
                               main = "Visualization Imputed Values",
                               color_lines = "black",
                               color_points = "steelblue2",
                               color_imputations = "indianred2",
                               color_truth = "green3",
                               shape_points = 16,
                               size_points = 2,
                               size_lines = 0.5,
                               linetype = "solid",
                               theme = ggplot2::theme_minimal() , ...) {

  
  ##
  ## 1. Input Check and Transformation
  ##
  
  # 1.1 Check if input is univariate
    if (!is.null(dim(x_with_na)) && dim(x_with_na)[2] != 1) {
    stop("Input x_with_na is not univariate")
  }
  
  if (!is.null(dim(x_with_imputations)) && dim(x_with_imputations)[2] != 1) {
    stop("Input x_with_imputations is not univariate")
  }
  
  if (!is.null(dim(x_with_truth)) && dim(x_with_truth)[2] != 1 && !is.null(x_with_truth)) {
    stop("Input x_with_truth is not univariate")
  }
  
  
  # 1.2 Special handling data types
  if (any(class(x_with_na) == "tbl")) {
    data <- as.vector(as.data.frame(x_with_na)[, 1])
  }
  
  if (any(class(x_with_imputations) == "tbl")) {
    data <- as.vector(as.data.frame(x_with_imputations)[, 1])
  }
  
  if (any(class(x_with_truth) == "tbl") && !is.null(x_with_truth)) {
    data <- as.vector(as.data.frame(x_with_truth)[, 1])
  }
  
  # 1.3 Check if input is numeric
  if (!is.numeric(x_with_na)) {
    stop("Input x_with_na is not numeric")
  }
  
  if (!is.numeric(x_with_imputations)) {
    stop("Input x_with_imputations is not numeric")
  }
  
  if (!is.numeric(x_with_truth) && !is.null(x_with_truth)) {
    stop("Input x_with_truth is not numeric")
  }

  ind <- 
  
  # 1.3 Change all time series objects everything to vector
  x_with_na <- as.vector(x_with_na)
  x_with_imputations <- as.vector(x_with_imputations)
  x_with_truth <- as.vector(x_with_truth)

  ##
  ## 2. Code for Plots
  ##

  # 2.1 Code for Case Ground Truth not given / is.null(x_with_truth = T
  
  if (is.null(x_with_truth) == T) {
    
    # Combine input time series to one data.frame
    series <- x_with_imputations
    indicator = as.numeric(is.na(x_with_na))
    df <- data.frame( series = series, indicator = indicator)
             
  
      gg <- ggplot2::ggplot(data = df, ggplot2::aes(x= 1:nrow(df), y=series)) +
        
        #Add points of series without NAs imputed + linetype
        ggplot2::geom_line(color = color_lines, linetype = linetype, size = size_lines) +
        
        ggplot2::geom_point(data = df, aes(x= 1:nrow(df), y=series ,colour = as.factor(indicator)),
                            shape = shape_points, size = size_points,
                            show.legend = TRUE) +
        
        ggplot2::scale_colour_manual(values=c("1"=color_imputations, "0"=color_points), 
                            labels = c("1"="Imputed Values", "0"= "Known Values" )) + 
        
        ggplot2::ylab(ylab) + ggplot2::xlab(xlab) + ggplot2::ggtitle(main) +
        ggplot2::theme(
            legend.position =  base::ifelse(legend == TRUE, "bottom", "none"),
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
            legend.title = ggplot2::element_blank())
  }  
  
  
  
  # 2.2 Code for Case Ground Truth available / / is.null(x_with_truth = F
  else {
    
    # Combine input time series to one data.frame
    series <- x_with_truth
    indicator = as.numeric(is.na(x_with_na))
  #  indicator2.
    df <- data.frame( series = series, indicator = indicator)
    
    
    gg <- ggplot2::ggplot(data = df, ggplot2::aes(x= 1:nrow(df), y=series)) +
      
      #Add points of series without NAs imputed + linetype
      ggplot2::geom_line(color = color_lines, linetype = linetype, size = size_lines) +
      
      ggplot2::geom_point(data = df, aes(x= 1:nrow(df), y=series ,colour = as.factor(indicator)),
                          shape = shape_points, size = size_points,
                          show.legend = TRUE) +
      
      ggplot2::scale_colour_manual(values=c("1"=color_imputations, "0"=color_points), 
                                   labels = c("1"="Imputed Values", "0"= "Known Values" )) + 
      
      ggplot2::ylab(ylab) + ggplot2::xlab(xlab) + ggplot2::ggtitle(main) +
      ggplot2::theme(
        legend.position =  base::ifelse(legend == TRUE, "bottom", "none"),
        axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
        legend.title = ggplot2::element_blank())
    
  }
    
    
        #mit real overplotten
    #(beides zu DF mit indicator zusammen)
    
    #dann ggpoint add imputaton
    
  return(gg)
}


#' Deprecated use \code{\link[imputeTS]{plotNA_imputations}} instead.
#' @description plotNA.imputations is replaced by \code{\link[imputeTS]{plotNA_imputations}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams plotNA_imputations
#' @keywords internal
#' @export
plotNA.imputations <- function(x.withNA, x.withImputations, x.withTruth = NULL,
                               legend = TRUE, main = "Visualization Imputed Values",
                               xlab = "Time", ylab = "Value",
                               colWithTruth = "green3", colLines = "black",
                               colWithImputations = "indianred2",
                               colWithNA = "steelblue2",
                               pch = 20, cex = 2, theme = ggplot2::theme_minimal(), ...) {
  .Deprecated(
    new = "plotNA_imputations",
    msg = "plotNA.imputations will be replaced by plotNA_imputations
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  plotNA_imputations(
    x.withNA, x.withImputations, x.withTruth,
    legend, main, xlab, ylab, colWithTruth, colLines, colWithImputations,
    colWithNA, pch, cex, theme, ...
  )
}
