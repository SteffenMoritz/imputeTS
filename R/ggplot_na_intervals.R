#' @title Visualize Missing Values per Interval
#'
#' @description Visualization of missing values in barplot form.
#' Especially useful when looking at specific intervals and for
#' time series with a lot of observations.
#'
#' @param x Numeric Vector (\code{\link[base]{vector}}) or Time Series
#' (\code{\link[stats]{ts}}) object containing NAs. This is the only mandatory
#' parameter - all other parameters are only needed for adjusting the plot appearance.
#'
#' @param number_intervals Defines the number of bins to be created. Default
#' number of intervals (denoted by NULL) is calculated by \code{\link[grDevices]{nclass.Sturges}}
#' using Sturges' formula. If the interval_size parameter is set to a value
#' different to NULL this parameter is ignored.
#'
#' @param interval_size Defines how many observations should be in one bin/interval.
#' The required number of overall bins is afterwards calculated automatically.
#' If used this parameter overwrites the number_intervals parameter.
#' For a very long time series be sure to make the interval_size not extremely
#' small, otherwise because of  overplotting issues nothing can be seen until
#' you also increase the plot width.
#'
#' @param measure Whether the NA / non-NA ratio should be given as
#' percent or absolute numbers.
#'
#' \itemize{
#'    \item{"percent" - for percentages}
#'
#'    \item{"count" - for absolute numbers of NAs}
#'    }
#'
#' @param color_missing Color for the amount of missing values.
#'
#' @param color_existing Color for the amount of existing values.
#'
#' @param alpha_missing Alpha (transparency) value for the missing values.
#'
#' @param alpha_existing Alpha (transparency) value for the existing values.
#'
#' @param title Title of the Plot (NULL for deactivating title).
#'
#' @param subtitle Subtitle of the Plot (NULL for deactivating subtitle).
#'
#' @param xlab Label for x-Axis. Automatically set to the current interval size, if
#' no custom text is chosen.
#'
#' @param ylab Label for y-Axis. As default (NULL), the axis is automatically set
#'  to either 'Percent' or 'Count' dependent on the settings of parameter \code{measure}.
#'
#' @param color_border Color for the small borders between the intervals/bins.
#' Default is 'white'.
#'
#' @param theme Set a Theme for ggplot2. Default is ggplot2::theme_linedraw().
#' (\code{\link[ggplot2]{theme_linedraw})}
#'
#' @details This function visualizes the distribution of missing values within
#' a time series. In comparison to the \code{\link[imputeTS]{ggplot_na_distribution}}
#' function this is not done by plotting each observation of the time series
#' separately. Instead observations for time intervals are represented as
#' intervals/bins of multiple values. For these intervals information about
#' the amount of missing values are shown. This has the advantage, that also
#' for large time series a plot which is easy to overview can be created.
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
#' @author Steffen Moritz, Sebastian Gatscha
#'
#' @seealso \code{\link[imputeTS]{ggplot_na_distribution}},
#'  \code{\link[imputeTS]{ggplot_na_gapsize}},
#'  \code{\link[imputeTS]{ggplot_na_imputations}}
#'
#' @examples
#' # Example 1: Visualize the missing values in tsNH4 time series
#' ggplot_na_intervals(tsNH4)
#'
#' # Example 2: Visualize the missing values in tsHeating time series
#' ggplot_na_intervals(tsHeating)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' tsNH4 %>% ggplot_na_intervals()
#' 
#' # Example 4: Visualize NAs in tsNH4 - exactly 8 intervals
#' ggplot_na_intervals(tsNH4, number_intervals = 8)
#' 
#' # Example 5: Visualize NAs in tsNH4 - 300 observations per interval
#' ggplot_na_intervals(tsNH4, interval_size = 300)
#' 
#' # Example 6: Visualize NAs in tsAirgap - different color for NAs
#' # Plot adjustments via ggplot_na_intervals function parameters
#' ggplot_na_intervals(tsAirgap, color_missing = "pink")
#'
#' # Example 7: Visualize NAs in tsNH4 - different theme
#' # Plot adjustments via ggplot_na_intervals function parameters
#' ggplot_na_intervals(tsNH4, theme = ggplot2::theme_classic())
#'
#' # Example 8: Visualize NAs in tsAirgap - title, subtitle in center
#' # Plot adjustments via ggplot2 syntax
#' ggplot_na_intervals(tsAirgap) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
#'   ggplot2::theme(plot.subtitle = ggtext::element_markdown(hjust = 0.5))
#'
#' # Example 9: Visualize NAs in tsAirgap - title in center, no subtitle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_intervals(tsAirgap, subtitle = NULL) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 10: Visualize NAs in tsAirgap - x-axis texts with angle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_intervals(tsAirgap, color_missing = "grey") +
#'   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))
#'   
#' @importFrom magrittr %>%
#'
#' @importFrom grDevices nclass.Sturges
#'
#' @importFrom ggplot2 theme_linedraw alpha ggplot aes scale_fill_manual
#' theme element_blank scale_x_continuous scale_y_continuous
#' labs xlab ylab stat_bin after_stat theme_classic
#'
#' @importFrom ggtext element_markdown
#'
#' @export
ggplot_na_intervals <- function(x,
                                number_intervals = NULL,
                                interval_size = NULL,
                                measure = "percent",
                                color_missing = "indianred2",
                                color_existing = "steelblue",
                                alpha_missing = 0.8,
                                alpha_existing = 0.3,
                                title = "Missing Values per Interval",
                                subtitle = "Amount of NA and non-NA for successive intervals",
                                xlab = "Time Lapse (Interval Size: XX)",
                                ylab = NULL,
                                color_border = "white",
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

  # 2.1 Calculation default number of intervals
  if (is.null(number_intervals)) {
    number_intervals <- grDevices::nclass.Sturges(data)
  }


  # 2.2 Calculation break points

  if (!is.null(interval_size)) {
    breaks <- seq(from = 0, to = length(data) - 1, by = interval_size)
    breaks <- c(breaks, length(data))
  }
  else {
    breaks <- seq(from = 0, to = length(data) - 1, by = floor(length(data) / number_intervals))
    breaks <- c(breaks, length(data))
  }
  binwidth <- breaks[2]


  # 2.3 Process parameter settings

  # Add alpha values to colors
  color_missing <- ggplot2::alpha(color_missing, alpha_missing)
  color_existing <- ggplot2::alpha(color_existing, alpha_existing)

  # Set subtitle to default
  # (needed because .Rd usage section gives error when using defaults > 90 chars )
  if ( (!is.null(subtitle)) && (subtitle == "Amount of NA and non-NA for successive intervals")) {
    subtitle <- paste0("Amount of <b style='color:", color_missing, ";' >NA</b>
                and  <b style='color:", color_existing, "' >non-NA</b>
                for successive intervals")
  }

  # Set ylab according to choosen measure
  if (is.null(ylab)) {
    ifelse(measure == "percent", ylab <- "Percent", ylab <- "Count")
  }

  # Set xlab according to choosen parameters
  if (xlab == "Time Lapse (Interval Size: XX)") {
    xlab <- paste("Time Lapse (Interval Size:", binwidth, ")")
  }


  # 2.4 Create dataframe for ggplot2

  index <- seq_along(data)
  miss <- as.factor(is.na(data))
  df <- data.frame(index, miss)

  ##
  ## End Preparations
  ##


  ##
  ## 3. Create the ggplot2 plot
  ##

  # Create the ggplot2 plot
  gg <- ggplot2::ggplot(df, ggplot2::aes(index, fill = miss)) +
    ggplot2::scale_fill_manual(
      values = c(color_existing, color_missing),
      labels = c("NAs", "non-NAs")
    ) +
    theme +
    ggplot2::theme(
      legend.position = "none",
      legend.title = ggplot2::element_blank(),
      plot.subtitle = ggtext::element_markdown(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  count <- NULL
  if (measure == "percent") {
    gg <- gg + ggplot2::stat_bin(ggplot2::aes(y = ggplot2::after_stat(count / binwidth)),
      col = color_border, breaks = breaks, closed = "right"
    )
  }
  else {
    gg <- gg + ggplot2::stat_bin(ggplot2::aes(y = ggplot2::after_stat(count)),
      col = color_border, breaks = breaks, closed = "right"
    )
  }

  return(gg)
}
