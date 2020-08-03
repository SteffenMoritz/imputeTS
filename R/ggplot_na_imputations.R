#' @title Visualize Imputed Values
#'
#' @description Visualize the imputed values in a time series.
#'
#' @param x_with_na Numeric Vector or Time Series (\code{\link{ts}}) object
#' with NAs before imputation. This parameter and x_with_imputation shave to
#' be set. The rest of the parameters are mostly needed for adjusting the plot
#' appearance.
#'
#' @param x_with_imputations Numeric Vector or Time Series (\code{\link{ts}})
#' object with NAs replaced by imputed values. This parameter and
#' x_with_imputation shave to be set.The rest of the parameters are mostly
#' needed for adjusting the plot appearance.
#'
#' @param x_with_truth Numeric Vector or Time Series (\code{\link{ts}}) object
#' with the real values (optional parameter). If the ground truth is known
#' (e.g. in experiments where the missing values were artificially added)
#' it can be displayed in the plot with this parameter.
#' Default is NULL (ground truth not known).
#'
#' @param x_axis_labels For adding specific x-axis labels. Takes a vector of
#' \code{\link[base]{Date}} or \code{\link[base]{POSIXct}} objects as an input
#' (needs the same length as x_with_na).
#' The Default (NULL) uses the observation numbers as x-axis tick labels.
#'
#' @param title Title of the Plot.
#'
#' @param subtitle Subtitle of the Plot.
#'
#' @param xlab Label for x-Axis.
#'
#' @param ylab Label for y-Axis.
#'
#' @param color_points Color for the Symbols/Points of the non-NA Observations.
#'
#' @param color_imputations Color for the Symbols/Points of the Imputed Values.
#'
#' @param color_truth Color for the Symbols/Points of the NA value Ground Truth
#' (only relevant when x_with_truth available).
#'
#' @param shape_points Shape for the Symbols/Points of the non-NA observations.
#' See https://ggplot2.tidyverse.org/articles/ggplot2-specs.html as reference.
#'
#' @param shape_imputations Shape for the Symbols/Points of the imputed values.
#' See https://ggplot2.tidyverse.org/articles/ggplot2-specs.html as reference.
#'
#' @param shape_truth Shape for the Symbols/Points of the NA value Ground Truth
#' (only relevant when x_with_truth available).
#'
#' @param size_points Size for the Symbols/Points of the non-NA Observations.
#'
#' @param size_imputations Size for the Symbols/Points of the Imputed Values.
#'
#' @param size_truth Size for the Symbols/Points of the NA value Ground Truth
#' (only relevant when x_with_truth available).
#'
#' @param color_lines Color for the Lines connecting the Observations/Points.
#'
#' @param size_lines Size for the Lines connecting the Observations/Points.
#'
#' @param linetype Linetype for the Lines connecting the Observations/Points.
#'
#' @param connect_na If TRUE the Imputations are connected
#' to the non-NA observations in the plot. Otherwise there are no
#' connecting lines between symbols in NA areas.
#'
#' @param legend If TRUE a Legend is added at the bottom.
#'
#' @param legend_size Size of the Symbols used in the Legend.
#'
#' @param label_known Legend label for the non-NA Observations.
#'
#' @param label_imputations Legend label for the Imputed Values.
#'
#' @param label_truth Legend label for the Ground Truth of the NA values.
#'
#' @param theme Set a Theme for ggplot2. Default is ggplot2::theme_linedraw().
#' (\code{\link[ggplot2]{theme_linedraw})}
#'
#' @details This plot can be used, to visualize imputed values for a time
#' series. Imputed values (filled NA gaps) are shown in a different color
#' than the other values. If real values (ground truth) for the NA gaps are known,
#' they can be optionally added in a different color.
#'
#' The only really needed parameters for this function are x_with_na
#' (the time series with NAs before imputation) and x_with_imputations
#' (the time series without NAs after imputation). All other parameters 
#' are msotly for altering the appearance of the plot.
#'
#' As long as the input is univariate and numeric the function also takes
#' data.frame, tibble, tsibble, zoo, xts as an input.
#'
#' The plot can be adjusted to your needs via the function parameters.
#' Additionally for more complex adjustments, the output can also be
#' adjusted via ggplot2 syntax. This is possible, since the output
#' of the function is a ggplot2 object. Also take a look at the Examples
#' to see how adjustments are made.
#'
#' @author Steffen Moritz, Sebastian Gatscha
#'
#'
#' @seealso \code{\link[imputeTS]{ggplot_na_distribution}},
#'  \code{\link[imputeTS]{ggplot_na_intervals}},
#'  \code{\link[imputeTS]{ggplot_na_gapsize}}
#'
#' @examples
#' # Example 1: Visualize imputation by na.mean
#' imp_mean <- na_mean(tsAirgap)
#' ggplot_na_imputations(tsAirgap, imp_mean)
#'
#'
#' # Example 2: Visualize imputation by na_locf and added ground truth 
#' imp_locf <- na_locf(tsAirgap)
#' ggplot_na_imputations(x_with_na = tsAirgap, 
#'                       x_with_imputations = imp_locf,
#'                       x_with_truth = tsAirgapComplete
#'                       )
#' 
#' 
#' # Example 3: Visualize imputation by na_kalman
#' imp_kalman <- na_kalman(tsAirgap)
#' ggplot_na_imputations(x_with_na = tsAirgap, x_with_imputations = imp_kalman)
#'
#'
#' # Example 4: Same as example 1, just written with pipe operator
#' tsAirgap %>%
#'   na_mean() %>%
#'   ggplot_na_imputations(x_with_na = tsAirgap)
#'   
#'
#' # Example 5: Visualize imputation by na_seadec - different color for imputed points
#' # Plot adjustments via ggplot_na_imputations function parameters
#' imp_seadec <- na_seadec(tsAirgap)
#' ggplot_na_imputations(x_with_na = tsAirgap, 
#'                       x_with_imputations = imp_seadec,
#'                       color_imputations = "gold")
#' 
#' 
#' # Example 6: Visualize imputation - different theme, point size imputations
#' # Plot adjustments via ggplot_na_imputations function parameters
#' imp_seadec <- na_seadec(tsAirgap)
#' ggplot_na_imputations(x_with_na = tsAirgap, 
#'                       x_with_imputations = imp_seadec,
#'                       theme = ggplot2::theme_classic(),
#'                       size_imputations = 5)
#'
#'                  
#' # Example 7: Visualize imputation - title, subtitle in center
#' # Plot adjustments via ggplot2 syntax
#' imp_seadec <- na_seadec(tsAirgap)
#' ggplot_na_imputations(x_with_na = tsAirgap,  x_with_imputations = imp_seadec) + 
#'     ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
#'     ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))   
#'
#'
#' # Example 8: Visualize imputation - title in center, no subtitle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' imp_mean <- na_mean(tsAirgap)
#' ggplot_na_imputations(x_with_na = tsAirgap,  
#'                       x_with_imputations = imp_mean,
#'                       subtitle = NULL) +
#'      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
#'   
#' @importFrom magrittr %>%
#'
#' @importFrom ggplot2 theme_linedraw ggplot geom_line aes geom_point
#' scale_color_manual element_blank xlab ylab ggtitle guides guide_legend
#' theme theme_classic
#'
#'
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
                                  shape_truth = 16,
                                  size_points = 1.5,
                                  size_imputations = 2.5,
                                  size_truth = 1.5,
                                  size_lines = 0.5,
                                  linetype = "solid",
                                  connect_na = TRUE,
                                  legend = TRUE,
                                  legend_size = 5,
                                  label_known = "known values",
                                  label_imputations = "imputed values",
                                  label_truth = "ground truth",
                                  theme = ggplot2::theme_linedraw()) {


  ##
  ## 1. Input Check and Transformation
  ##

  # 1.1 special handling data types
  # x_with_na
  if (any(class(x_with_na) == "tbl_ts")) {
    x_with_na <- as.vector(as.data.frame(x_with_na)[, 2])
  }
  else if (any(class(x_with_na) == "tbl")) {
    x_with_na <- as.vector(as.data.frame(x_with_na)[, 1])
  }
  # x_with_imputations
  if (any(class(x_with_imputations) == "tbl_ts")) {
    x_with_imputations <- as.vector(as.data.frame(x_with_imputations)[, 2])
  }
  else if (any(class(x_with_imputations) == "tbl")) {
    x_with_imputations <- as.vector(as.data.frame(x_with_imputations)[, 1])
  }
  # x_with_truth
  if (any(class(x_with_truth) == "tbl_ts")) {
    x_with_truth <- as.vector(as.data.frame(x_with_truth)[, 2])
  }
  else if (any(class(x_with_truth) == "tbl")) {
    x_with_truth <- as.vector(as.data.frame(x_with_truth)[, 1])
  }

  # 1.2 Check if the input is multivariate

  if (!is.null(dim(x_with_na)[2]) && dim(x_with_na)[2] > 1) {
    stop("x_with_na is not univariate.
    The function only works with univariate input for x_with_na.
    For data types with multiple variables/columns only input the
         column you want to plot as parameter x_with_na.")
  }

  if (!is.null(dim(x_with_imputations)[2]) && dim(x_with_imputations)[2] > 1) {
    stop("x_with_imputations is not univariate.
    The function only works with univariate input for x_with_imputations.
    For data types with multiple variables/columns only input the column
         you want to plot as parameter x_with_imputations")
  }

  if (!is.null(dim(x_with_truth)[2]) && dim(x_with_truth)[2] > 1) {
    stop("x_with_na is not univariate.
    The function only works with univariate input for x_with_truth.
    For data types with multiple variables/columns only input the
         column you want to plot as parameter x_with_truth")
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


  # 1.4 Input as vector

  x_with_na <- as.vector(x_with_na)
  x_with_imputations <- as.vector(x_with_imputations)
  x_with_truth <- as.vector(x_with_truth)


  # 1.5 Check if input is numeric

  if (!is.numeric(x_with_na)) {
    stop("Input x_with_na is not numeric")
  }
  if (!is.numeric(x_with_imputations)) {
    stop("Input x_with_imputations is not numeric")
  }
  if (!is.numeric(x_with_truth) && !is.null(x_with_truth)) {
    stop("Input x_with_truth is not numeric")
  }


  # 1.6 Same length of the series

  # x_with_na and x_with_imputations need same length
  if (length(x_with_na) != length(x_with_imputations)) {
    stop("Input x_with_na and x_with_imputations need to have the same length.
         x_with_na is the time series with NAs before imputation.
         x_with_imputations is the time series with filled NAs after applying imputation.")
  }

  # if x_with_truth available it needs also same length
  if (!is.null(x_with_truth) && (length(x_with_na) != length(x_with_truth))) {
    stop("Input x_with_na, x_with_imputations and x_with_truth need to have the same length.
         x_with_na is the time series with NAs before imputation.
         x_with_imputations is the time series with filled NAs after applying imputation.
         x_with_truth (optional) is the series with the ground truth for the imputed values")
  }



  # 1.7 Check preconditions about amount of NAs

  # Unwanted all NA inputs
  missindx_x_with_na <- is.na(x_with_na)
  if (all(missindx_x_with_na)) {
    stop("Input x_with_na consists only of NAs.
     Something with the input likely went wrong.
     Creating a ggplot_na_imputations plot does not make sense with an all NA input.
     This are the required inputs:
     x_with_na (time series before imputation) - still has NAs,
     x_with_imputations (time series after imputation) - NAs replaced by imputation")
  }

  missindx_x_with_imputations <- is.na(x_with_imputations)
  if (all(missindx_x_with_imputations)) {
    stop("Input x_with_imputations consists only of NAs.
     Something with the input likely went wrong.
     Creating a ggplot_na_imputations plot does not make sense with an all NA input.
     This are the required inputs:
     x_with_na (time series before imputation),
     x_with_imputations (time series after imputation)")
  }


  # Unwanted no NA inputs
  if (!anyNA(x_with_na)) {
    stop("Input x_with_na contains no NAs. At least one missing value is needed
     to create a meaningful ggplot_na_imputations plot)
     This are the required inputs:
     x_with_na (time series before imputation) - still has NAs,
     x_with_imputations (time series after imputation) - NAs replaced by imputation")
  }


  ##
  ## End Input Check and Transformation
  ##



  ##
  ## 2. Preparations
  ##

  # 2.1 Create dataframe for ggplot2

  # Define x-axis label data
  # if Date or POSIXct given for x_axis_labels time information can be plotted
  if (any(class(x_axis_labels) == "Date")) {
    time <- x_axis_labels
  }
  else if (any(class(x_axis_labels) == "POSIXct")) {
    time <- x_axis_labels
  }
  else if (is.null(x_axis_labels)) {
    time <- seq_along(x_with_na)
  }
  else {
    stop("Input for x_axis_labels is not in a supported format, must be a
           vector of Date or a POSIXct objects with the same length as
           x_with_na and x_with_imputations")
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
  if (connect_na == FALSE) {
    gg <- gg + ggplot2::geom_line(
      data = df, ggplot2::aes(x = time, y = x_with_na),
      na.rm = TRUE, color = color_lines,
      linetype = linetype, size = size_lines
    )
  }
  # If truth available connect the true values in the missing areas
  else if (!is.null(x_with_truth)) {
    gg <- gg + ggplot2::geom_line(
      data = df, ggplot2::aes(x = time, y = x_with_truth),
      na.rm = TRUE, color = color_lines,
      linetype = linetype, size = size_lines
    )
  }
  # If no truth available connect the imputed values in the missing areas
  else {
    gg <- gg + ggplot2::geom_line(
      data = df, ggplot2::aes(x = time, y = x_with_imputations),
      na.rm = TRUE, color = color_lines,
      linetype = linetype, size = size_lines
    )
  }


  # Remove known values from imputations - to avoid overplotting
  df$x_with_imputations[!is.na(x_with_na)] <- NA
  if (!is.null(x_with_truth)) {
    df$x_with_truth[!is.na(x_with_na)] <- NA
  }

  ## Add points

  # Points for regular, known values
  gg <- gg + ggplot2::geom_point(
    data = df, ggplot2::aes(x = time, y = x_with_na, color = "1"),
    na.rm = TRUE, shape = shape_points, size = size_points
  )


  # Points for Imputations
  gg <- gg + ggplot2::geom_point(
    data = df, ggplot2::aes(x = time, y = x_with_imputations, color = "2"),
    na.rm = TRUE, size = size_imputations, shape = shape_imputations
  )

  # Points for truth
  if (!is.null(x_with_truth)) {
    gg <- gg + ggplot2::geom_point(
      data = df, ggplot2::aes(x = time, y = x_with_truth, color = "3"),
      na.rm = TRUE, shape = shape_truth, size = size_truth
    )
  }


  if (!is.null(x_with_truth)) {
    gg <- gg + ggplot2::scale_color_manual(
      name = ggplot2::element_blank(),
      breaks = c("1", "2", "3"),
      labels = c(label_known, label_imputations, label_truth),
      values = c(color_points, color_imputations, color_truth)
    )
  }
  else {
    gg <- gg + ggplot2::scale_color_manual(
      name = ggplot2::element_blank(),
      breaks = c("1", "2"),
      labels = c(label_known, label_imputations),
      values = c(color_points, color_imputations)
    )
  }

  gg <- gg + ggplot2::ylab(ylab) + ggplot2::xlab(xlab) +

    ggplot2::ggtitle(label = title, subtitle = subtitle) + theme

  if (!is.null(x_with_truth)) {
    gg <- gg + ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(size = legend_size,
                          shape = c(shape_points, shape_imputations, shape_truth))
    ))
  }
  else {
    gg <- gg + ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(size = legend_size,
                          shape = c(shape_points, shape_imputations))
    ))
  }

  gg <- gg + ggplot2::theme(
    legend.position = base::ifelse(legend == TRUE, "bottom", "none"),
    legend.title = ggplot2::element_blank()
  )

  ##
  ##  End creating the ggplot2 plot
  ##

  return(gg)
}
