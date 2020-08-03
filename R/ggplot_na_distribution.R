#' @title Visualize the Distribution of Missing Values
#'
#' @description Visualize the distribution of missing values within a time series.
#'
#' @param x Numeric Vector (\code{\link[base]{vector}}) or Time Series
#' (\code{\link[stats]{ts}}) object containing NAs. This is the only mandatory
#' parameter - all other parameters are only needed for adjusting the plot appearance.
#'
#' @param x_axis_labels For adding specific x-axis labels. Takes a vector of
#'  \code{\link[base]{Date}} or \code{\link[base]{POSIXct}} objects
#' as an input (needs the same length as x) . The Default (NULL) uses the
#' observation numbers as  x-axis tick labels.
#'
#' @param color_points Color for the Symbols/Points.
#'
#' @param color_lines Color for the Lines.
#'
#' @param color_missing Color used for highlighting the time spans with NA values.
#'
#' @param color_missing_border Color used as border for time spans with NA values.
#'
#' @param alpha_missing Transparency value used for color_missing.
#'
#' @param title Title of the Plot (NULL for deactivating title).
#'
#' @param subtitle Subtitle of the Plot (NULL for deactivating subtitle).
#'
#' @param xlab Label for x-Axis.
#'
#' @param ylab Label for y-Axis.
#'
#' @param shape_points Symbol to use for the Observations/Points. See
#' https://ggplot2.tidyverse.org/articles/ggplot2-specs.html as reference.
#'
#' @param size_points Size of Symbols/Points.
#'
#' @param theme Set a Theme for ggplot2. Default is ggplot2::theme_linedraw().
#' (\code{\link[ggplot2]{theme_linedraw})}
#'
#' @details This function visualizes the distribution of missing values within
#'  a time series. If a value is NA, the background is colored differently.
#'  This gives a good overview of where most missing values occur.
#'
#' The only really needed parameter for this function is x (the univariate
#' time series that shall be visualized). All other parameters are solely
#' for altering the appearance of the plot.
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
#' For very long time series it might happen, that the plot gets too crowded
#' and overplotting issues occur. In this case the
#' \code{\link[imputeTS]{ggplot_na_intervals}} plotting function can provide
#' a more condensed overview.
#'
#'
#' @author Steffen Moritz, Sebastian Gatscha
#'
#' @seealso \code{\link[imputeTS]{ggplot_na_intervals}},
#' \code{\link[imputeTS]{ggplot_na_gapsize}},
#' \code{\link[imputeTS]{ggplot_na_imputations}}
#'
#' @examples
#' # Example 1: Visualize the missing values in x
#' x <- stats::ts(c(1:11, 4:9, NA, NA, NA, 11:15, 7:15, 15:6, NA, NA, 2:5, 3:7))
#' ggplot_na_distribution(x)
#'
#' # Example 2: Visualize the missing values in tsAirgap time series
#' ggplot_na_distribution(tsAirgap)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' x <- ts(c(1:11, 4:9, NA, NA, NA, 11:15, 7:15, 15:6, NA, NA, 2:5, 3:7))
#' x %>% ggplot_na_distribution()
#'
#' # Example 4: Visualize NAs in tsAirgap - different color for points
#' # Plot adjustments via ggplot_na_distribution function parameters
#' ggplot_na_distribution(tsAirgap, color_points = "grey")
#'
#' # Example 5: Visualize NAs in tsAirgap - different theme
#' # Plot adjustments via ggplot_na_distribution function parameters
#' ggplot_na_distribution(tsAirgap, theme = ggplot2::theme_classic())
#'
#' # Example 6: Visualize NAs in tsAirgap - title, subtitle in center
#' # Plot adjustments via ggplot2 syntax
#' ggplot_na_distribution(tsAirgap) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
#'   ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 7: Visualize NAs in tsAirgap - title in center, no subtitle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_distribution(tsAirgap, subtitle = NULL) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 8: Visualize NAs in tsAirgap - x-axis texts with angle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_distribution(tsAirgap, color_points = "grey") +
#'   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))
#'   
#' @importFrom ggplot2 theme_linedraw ggplot geom_point aes geom_line geom_bar ggtitle
#' xlab ylab theme element_text theme_classic
#'
#' @importFrom stats ts
#'
#' @importFrom magrittr %>%
#'
#' @export

ggplot_na_distribution <- function(x,
                                   x_axis_labels = NULL,
                                   color_points = "steelblue",
                                   color_lines = "steelblue2",
                                   color_missing = "indianred",
                                   color_missing_border = "indianred",
                                   alpha_missing = 0.5,
                                   title = "Distribution of Missing Values",
                                   subtitle = "Time Series with highlighted missing regions",
                                   xlab = "Time",
                                   ylab = "Value",
                                   shape_points = 20,
                                   size_points = 2.5,
                                   theme = ggplot2::theme_linedraw()) {
  data <- x


  ##
  ## 1. Input Check and Transformation
  ##

  # 1.1 special handling data types
  if (any(class(data) == "tbl_ts")) {
    data <- as.vector(as.data.frame(data)[, 2])
  }
  else if (any(class(data) == "tbl")) {
    data <- as.vector(as.data.frame(data)[, 1])
  }
  
  # 1.2 Check if the input is multivariate
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    stop("x is not univariate. The function only works with univariate
    input for x. For data types with multiple variables/columns only input
    the column you want to plot as parameter x.")
  }


  # 1.3 Checks and corrections for wrong data dimension

  # Altering multivariate objects with 1 column (which are essentially
  # univariate) to be dim = NULL
  if (!is.null(dim(data)[2])) {
    data <- data[, 1]
  }
  
  
  # 1.4 Input as vector
  data <- as.vector(data)
  
  
  # 1.5 Check if input is numeric
  if (!is.numeric(data)) {
    stop("Input x is not numeric")
  }
  
  
  # 1.6 Check preconditions about amount of NAs
  
  # exclude NA only inputs
  missindx <- is.na(data)
  if (all(missindx)) {
    stop("Input data consists only of NAs. At least one non-NA numeric value is needed
    for creating a meaningful ggplot_na_distribution plot)")
  }
  
  

  ##
  ## End Input Check and Transformation
  ##


  

  ##
  ## 2. Preparations
  ##

  # 2.1 Create required data

  # Get NA positions
  id_na <- which(is.na(data))

  
  # 2.2 Create dataframe for ggplot2

  # Define x-axis label data
  # if Date or POSIXct given for x_axis_labels time information can be plotted
  if (any(class(x_axis_labels) == "Date")) {
    time <- x_axis_labels
    width_na_bar <- as.numeric(time[2] - time[1]) * 0.9
  }
  else if (any(class(x_axis_labels) == "POSIXct")) {
    time <- x_axis_labels
    width_na_bar <- as.numeric(difftime(time[2], time[1], units = "secs")) * 0.9
  }
  else if (is.null(x_axis_labels)) {
    time <- seq_along(data)
    width_na_bar <- as.numeric(time[2] - time[1]) * 0.9
  }
  else {
    stop("Input for x_axis_labels is not in a supported format, must a
           vector of Date or a POSIXct objects with the same length as x")
  }

  # Create the remainder of the data.frame for ggplot2
  value <- data
  df <- data.frame(time, value)

  ##
  ## End Preparations
  ##


  ##
  ## 3. Create the ggplot2 plot
  ##

  # Create the plot
  gg <- ggplot2::ggplot() +

    # Adding the Line +  Parameters
    ggplot2::geom_line(
      data = df, na.rm = T,
      ggplot2::aes(x = time, y = value), col = color_lines
    ) +

    # Adding the Points + Parameters
    ggplot2::geom_point(
      data = df, na.rm = TRUE,
      ggplot2::aes(x = time, y = value), shape = shape_points,
      col = color_points, size = size_points
    ) +

    # Adding additional modifications like title, subtitle, theme,...
    ggplot2::ggtitle(label = title, subtitle = subtitle) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    theme

  # Add the red background bars for missing data areas

  if (length(id_na) > 0) {
    # Red Bars only if missing data in time series
    na_val <- max(df$value, na.rm = TRUE)
    gg <- gg +
      ggplot2::geom_bar(
        data = df[is.na(df$value), ], stat = "identity",
        ggplot2::aes(x = time, y = na_val),
        col = color_missing_border, fill = color_missing,
        alpha = alpha_missing, width = width_na_bar
      )
  }

  ##
  ##  End creating the ggplot2 plot
  ##


  return(gg)
}
