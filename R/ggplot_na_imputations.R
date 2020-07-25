#' @title Visualize Imputed Values
#'
#' @description Visualize the imputed values in a time series.
#'
#' @param x_with_na Numeric Vector or Time Series (\code{\link{ts}}) object with NAs before imputation. This parameter and
#' x_with_imputations (and eventually x_with_truth if available) have to be set. The rest of the parameters are only needed if you want to adjust the default design.
#' 
#' @param x_with_imputations Numeric Vector or Time Series (\code{\link{ts}}) object with NAs replaced by imputed values. This parameter and
#' x_with_na (and eventually x_with_truth if available) have to be set. The rest of the parameters are only needed if you want to adjust the default design.
#' 
#' @param x_with_truth Numeric Vector or Time Series (\code{\link{ts}}) object with the real values (can be set to NULL if not known).
#'
#' @param x_axis_labels For adding specific x-axis labels. Takes a vector (with the same length as x) 
#' of either Date or POSIXct objects as an input. Default (NULL) is using the 
#' observation number as  x-axis tick labels.
#' 
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' 
#' @param xlab Label for x axis.
#' @param ylab Label for y axis.
#'
#' @param color_points Color of the symobls for the normal non-NA observations.
#' @param color_imputations Color of the symbols for the imputed values.
#' @param color_truth Color of the symbols for the ground truth of the NA values.
#' @param shape_points Shape of the symobls for the normal non-NA observations.
#' @param shape_imputations  Shape of the symbols for the imputed values.
#' @param shape_truth Shape of the symbols for the ground truth of the NA values.
#' @param size_points Size of the symobls for the normal non-NA observations.
#' @param size_imputations Size of the symbols for the imputed values.
#' @param size_truth Size of the symbols for the ground truth of the NA values.
#' 
#' @param color_lines Color for the lines connecting the observations/points.
#' @param size_lines Size for the lines connecting the observations/points.
#' @param linetype Linetype for the lines connecting the observations/points.
#' @param connect_na If TRUE the imputations/ground truth values are connected to the normal
#'  non-NA observations in the plot. Otherwise there are no connecting lines between symbols in NA areas.
#' 
#' @param legend If TRUE a legend is added at the bottom.
#' @param legend_size Size of the symbols used in the legend.
#' @param label_known Legend label for the normal non-NA observations.
#' @param label_imputations Legend label for the imputed values.
#' @param label_truth Legend label for the ground truth of the NA values.
#' 
#' @param theme Set a theme for ggplot2. Default is \code{\link[ggplot2]{theme_linedraw}}
#'
#' @details This plot can be used, to visualize imputed values for a time series.
#' Imputed values (filled NA gaps) are shown in a different color than the other values.
#' If real values (truth) for the NA gaps are known, they are added in a different color.
#'
#' @author Steffen Moritz
#'
#'
#' @seealso \code{\link[imputeTS]{ggplot_na_distribution}},\code{\link[imputeTS]{ggplot_na_intervals}},
#'  \code{\link[imputeTS]{ggplot_na_gapsize}}
#'
#' @examples
#' # Example 1: Visualize the values that were imputed by na.mean in the time series
#' impMean.Airgap <- na_mean(tsAirgap)
#' ggplot_na_imputations(tsAirgap, impMean.Airgap)
#'
#'
#' # Example 2: Visualize the imputed values by na_locf and the true values in the time series
#' impLOCF.Airgap <- na_locf(tsAirgap)
#' ggplot_na_imputations(tsAirgap, impLOCF.Airgap, tsAirgapComplete)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' tsAirgap %>%
#'   na_mean() %>%
#'   ggplot_na_imputations(x_with_na = tsAirgap)
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot geom_line geom_point aes
#' theme_minimal theme element_text element_blank xlab ylab ggtitle scale_color_manual guide_legend guides
#' @export
ggplot_na_imputations <- function(x_with_na, 
                                  x_with_imputations,
                                  x_with_truth = NULL,
                                  x_axis_labels = NULL,
                                  title = "Imputed Values",
                                  subtitle = "Visualization of missing value replacements",
                                  xlab = "Time", 
                                  ylab = "Value", 
                                  color_points = "steelblue",
                                  color_imputations = "indianred",
                                  color_truth = "seagreen3",
                                  color_lines = "lightslategray",
                                  shape_points = 16,
                                  shape_imputations = 18,
                                  shape_truth = 16 ,
                                  size_points = 1.5,
                                  size_imputations = 2.5,
                                  size_truth = 1.5,
                                  size_lines = 0.5,
                                  linetype = "solid",
                                  connect_na = T,
                                  legend = TRUE,
                                  legend_size = 5,
                                  label_known = "known values",
                                  label_imputations = "imputed values",
                                  label_truth = "ground truth",
                                  theme = ggplot2::theme_linedraw()) {

  
  ##
  ## 1. Input Check and Transformation
  ##
  
  # 1.1 Check if the input is multivariate
  
  if (!is.null(dim(x_with_na)[2]) && dim(x_with_na)[2] > 1) {
    stop("x_with_na is not univariate. The function only works with univariate input for x_with_na. For data types with 
         multiple variables/columns only input the column you want to plot as parameter x_with_na.")
  }
  
  if (!is.null(dim(x_with_imputations)[2]) && dim(x_with_imputations)[2] > 1) {
    stop("x_with_imputations is not univariate. The function only works with univariate input for x_with_imputations For data types with 
         multiple variables/columns only input the column you want to plot as parameter x_with_imputations")
  }
  
  if (!is.null(dim(x_with_truth)[2]) && dim(x_with_truth)[2] > 1) {
    stop("x_with_na is not univariate. The function only works with univariate input for x_with_truth For data types with 
         multiple variables/columns only input the column you want to plot as parameter x_with_truth")
  }
  
  
  # 1.2 Special handling data types
  
  if (any(class(x_with_na) == "tbl")) {
    x_with_na <- as.vector(as.data.frame(x_with_na)[, 1])
  }
  
  if (any(class(x_with_imputations) == "tbl")) {
    x_with_imputations <- as.vector(as.data.frame(x_with_imputations)[, 1])
  }
  
  if (any(class(x_with_truth) == "tbl") && !is.null(x_with_truth)) {
    x_with_truth <- as.vector(as.data.frame(x_with_truth)[, 1])
  }
  
  # 1.3 Checks and corrections for wrong data dimension
  
  # Altering multivariate objects with 1 column (which are essentially
  # univariate) to be dim = NULL
  if (!is.null(dim(x_with_na)[2])) {
    x_with_na <- x_with_na[, 1]
  }
  if (!is.null(dim(x_with_imputations)[2])) {
    x_with_imputations <- x_with_imputations[, 1]
  }
  if (!is.null(dim(x_with_truth)[2])) {
    x_with_truth <- x_with_truth[, 1]
  }
  
  
  # 1.4 Check if input is numeric
  
  if (!is.numeric(x_with_na)) {
    stop("Input x_with_na is not numeric")
  }
  if (!is.numeric(x_with_imputations)) {
    stop("Input x_with_imputations is not numeric")
  }
  if (!is.numeric(x_with_truth) && !is.null(x_with_truth)) {
    stop("Input x_with_truth is not numeric")
  }
  
  ##
  ## End Input Check and Transformation
  ##
  
 

  ##
  ## 2. Preparations
  ##
  
  # 2.1 Create required data

  # Input as vector
  
  x_with_na <- as.vector(x_with_na)
  x_with_imputations <- as.vector(x_with_imputations)
  x_with_truth <- as.vector(x_with_truth)
  
  
  # 2.2 Create dataframe for ggplot2
  
  # Define x-axis label data
  # if Date or POSIXct given for x_axis_labels time information can be plotted
  if (any(class(x_axis_labels) == "Date")) {
    time <- x_axis_labels
  }
  else if  (any(class(x_axis_labels) == "POSIXct"))
  {
    time <- x_axis_labels
  }
  else if (is.null(x_axis_labels)) {
    time <- seq_along(x_with_na)
  }
  else {
    stop("Input for x_axis_labels is not in a supported format, must a 
           vector of Date or a POSIXct objects with the same length as x_with_na and x_with_imputations")
  }
  
  if (!is.null(x_with_truth)) {
    df <- data.frame(time, x_with_imputations, x_with_na, x_with_truth)
  }
  else {
    df <- data.frame(time, x_with_imputations, x_with_na)
  }

  ##
  ## End Preparations
  ##
  
  ##
  ## 3. Create the ggplot2 plot
  ##
  
  # Create the plot
  gg <- ggplot2::ggplot(data = df)

  ## Add Lines
  # Don't connect the lines in the missing areas
  if (connect_na == F) {
    gg <- gg + ggplot2::geom_line(
      data = df, aes(x = time, y = x_with_na),
      na.rm = T, color = color_lines,
      linetype = linetype, size = size_lines
    )
  }
  # If truth available connect the true values in the missing areas
  else if (!is.null(x_with_truth)) {
    gg <- gg + ggplot2::geom_line(
      data = df, aes(x = time, y = x_with_truth),
      na.rm = T, color = color_lines,
      linetype = linetype, size = size_lines
    )
  }
  #If no truth available connect the imputed values in the missing areas
  else {
    gg <- gg + ggplot2::geom_line(
      data = df, aes(x = time, y = x_with_imputations),
      na.rm = T, color = color_lines,
      linetype = linetype, size = size_lines
    )
  }
  

  # Remove known values from imputations - to avoid overplotting
   df$x_with_imputations[ !is.na(x_with_na)] <- NA
   if (!is.null(x_with_truth)) {
     df$x_with_truth[ !is.na(x_with_na)] <- NA
   }

  ## Add points
  
  # Points for regular, known values
     gg <- gg + ggplot2::geom_point(data = df, aes(x = time, y = x_with_na, color = "1"), 
                      na.rm = T, shape = shape_points, size = size_points ) 
  
   
  # Points for Imputations
     gg <- gg + ggplot2::geom_point(data = df, aes(x = time, y = x_with_imputations, color = "2"), 
                                  na.rm = T, size = size_imputations , shape = shape_imputations) 
     
     # Points for truth
     if (!is.null(x_with_truth)) {
       gg <- gg + ggplot2::geom_point(data = df, aes(x = time, y = x_with_truth, color = "3"),
                                      na.rm = T, shape = shape_truth, size = size_truth)
     }
    

     if (!is.null(x_with_truth)) {
       
       gg <- gg+  scale_color_manual(
          name = element_blank(),
          breaks = c("1", "2","3"),
          labels = c( label_known, label_imputations, label_truth),
          values = c(  color_points, color_imputations, color_truth)
          ) 
     }
       else {
         gg <- gg+  scale_color_manual(
           name = element_blank(),
           breaks = c("1", "2"),
           labels = c( label_known, label_imputations),
           values = c(  color_points, color_imputations)
         ) 
       }

    gg <- gg + ggplot2::ylab(ylab) + ggplot2::xlab(xlab) +

    ggplot2::ggtitle(label = title, subtitle = subtitle) + theme
    
    if (!is.null(x_with_truth)) {
   gg <- gg+  ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(size=legend_size, shape = c(shape_points, shape_imputations, shape_truth) ))
      )
    }
    else {
      gg <- gg+  ggplot2::guides(color = ggplot2::guide_legend(
        override.aes = list(size=legend_size, shape = c(shape_points, shape_imputations) ))
      )    }

    gg <- gg + ggplot2::theme(
       legend.position =  base::ifelse(legend == TRUE, "bottom", "none"),
       legend.title = ggplot2::element_blank()
   )

  ##
  ##  End creating the ggplot2 plot
  ##

  return(gg)
}
