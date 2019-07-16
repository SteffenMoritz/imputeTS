#' @title Visualize Distribution of Missing Values (Barplot)
#'
#' @description Visualization of missing values in barplot form. Especially useful for
#' time series with a lot of observations.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object containing NAs
#'
#' @param breaks Defines the number of bins to be created. Default number of breaks is calculated by \code{\link[grDevices]{nclass.Sturges}}
#' using Sturges' formula. If the breaksize parameter is set to a value different to NULL
#' this parameter is ignored.
#'
#' @param breaksize Defines how many observations should be in one bin. The required number of
#' overall bins is afterwards calculated automatically. This parameter if used overwrites the breaks parameter.
#'
#' @param percentage Whether the NA / non-NA ration should be given as percent or absolute numbers
#'
#' @param legend If TRUE a legend is added at the bottom
#'
#' @param axes If FALSE the axes are hidden
#' @param space The amount of space (as a fraction of the average bar width) left before each bar.
#' @param col A vector of colors for the bars or bar components.
#' @param main Main title
#' @param xlab Label for x axis
#' @param ylab Label for y axis
#' @param colborder Color for the bar chart borders. Default is 'black'.
#' @param xangle Angle of x-axis labels. Default is '0'.
#' @param theme Theme for ggplot2. Default is \code{\link[ggplot2]{theme_minimal}}
#' @param ... These parameters are passed to \code{\link[ggplot2]{geom_bar}}
#'
#' @details This function visualizes the distribution of missing values within a time series.
#' In comparison to the \code{\link[imputeTS]{plotNA_distribution}} function this is not done by plotting
#' each observation of the time series separately Instead observations for time intervals are represented as bars.
#' For these intervals information about the amount of missing values are shown. This has the advantage, that also
#' for large time series a plot which is easy to overview can be created.
#'
#' @author Steffen Moritz
#'
#' @seealso \code{\link[imputeTS]{plotNA_distribution}},
#'  \code{\link[imputeTS]{plotNA_gapsize}}, \code{\link[imputeTS]{plotNA_imputations}}
#'
#' @examples
#' # Example 1: Visualize the missing values in tsNH4 time series
#' plotNA_distributionBar(tsNH4)
#'
#' # Example 2: Visualize the missing values in tsHeating time series
#' plotNA_distributionBar(tsHeating, breaks = 20)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' tsNH4 %>% plotNA_distributionBar()
#' @importFrom grDevices nclass.Sturges
#' @importFrom ggplot2 ggplot geom_bar aes position_fill
#' scale_fill_manual scale_x_discrete scale_y_continuous
#' theme element_text element_blank xlab ylab ggtitle theme_minimal
#' @importFrom magrittr %>%
#' @export
plotNA_distributionBar <- function(x, breaks = grDevices::nclass.Sturges(x),
                                   breaksize = NULL, percentage = TRUE, legend = TRUE,
                                   axes = TRUE, space = 0,
                                   col = c("indianred2", "green2"),
                                   main = "Distribution of NAs",
                                   xlab = "Time Lapse", ylab = NULL,
                                   colborder = "black", xangle = 0,
                                   theme = ggplot2::theme_minimal(), ...) {
  data <- x

  ##
  ## Input check
  ##
  if (!is.null(dim(data)) && dim(data)[2] != 1) {
    stop("Input x is not univariate")
  }
  if (!is.numeric(data)) {
    stop("Input x is not numeric")
  }

  # Change zoo, xts, timeSeries objects to vector to avoid errors
  if (is.ts(data)) {
    data <- as.vector(data)
  }


  ##
  ## Plotting Code
  ##

  len_data <- length(data)

  # Calculate the breaksize from the demanded breaks
  if (is.null(breaksize)) breaksize <- ceiling(len_data / breaks)
  breakpoints <- unique(c(seq(1, len_data, breaksize), len_data))

  # Define the width of the last bin in order to make it smaller if it contains less values
  width_last <- (breakpoints[length(breakpoints)] - breakpoints[length(breakpoints) - 1]) /
    (breakpoints[2] - breakpoints[1])

  # calculate NA/non-NA ratio inside of every bin
  na_amount <- ok_mount <- numeric(0)
  for (i in 1:(length(breakpoints) - 1)) {
    cut <- data[(breakpoints[i] + 1):(breakpoints[i + 1])]

    nas <- length(which(is.na(cut)))
    na_amount <- c(na_amount, nas)

    oks <- length(cut) - nas
    ok_mount <- c(ok_mount, oks)
  }

  # calculate percentages if wanted
  if (percentage) {
    sums <- ok_mount + na_amount
    na_amount <- na_amount / sums
    ok_mount <- ok_mount / sums
    ylab1 <- "Percentage"
  } else {
    ylab1 <- "Number"
  }

  # check if ylab is pre set
  if (is.null(ylab)) ylab <- ylab1

  # create data to be plotted
  len_amo <- length(na_amount)
  df <- data.frame(
    "bin" = 1:len_amo,
    "na_amount" = c(na_amount, ok_mount),
    "value" = c(
      rep("NA", len_amo),
      rep("Not_NA", len_amo)
    )
  )


  # make plot
  # breakpoints = paste0(round((breakpoints[-1] / len_data) * 100,
  breakpoints <- paste0(round((breakpoints / len_data) * 100,
    digits = 0
  ), "%")


  gg <- ggplot2::ggplot(df) +
    ggplot2::geom_bar(aes(fill = df$value, y = na_amount, x = df$bin),
      position = ggplot2::position_stack(reverse = TRUE),
      stat = "identity", color = colborder,
      width = c(rep(1, len_amo - 1), width_last) / (space + 1),
      ...
    ) +
    ggplot2::scale_fill_manual(values = col, labels = c("NAs", "non-NAs")) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_discrete(
      breaks = seq(0, len_amo),
      labels = breakpoints,
      limits = as.character(seq(0, len_amo))
    ) +
    theme +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = xangle, hjust = 1.5),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::ggtitle(main)

  # hide axis
  if (!axes) {
    gg <- gg +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  }

  # add legend
  if (legend) {
    gg <- gg +
      ggplot2::theme(
        legend.title = element_blank(),
        legend.position = "bottom"
      )
  }

  return(gg)
}


#' Deprecated use \code{\link[imputeTS]{plotNA_distributionBar}} instead.
#' @description plotNA.distributionBar is replaced by \code{\link[imputeTS]{plotNA_distributionBar}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams plotNA_distributionBar
#' @keywords internal
#' @export
plotNA.distributionBar <- function(x, breaks = grDevices::nclass.Sturges(x),
                                   breaksize = NULL, percentage = TRUE, legend = TRUE,
                                   axes = TRUE, space = 0,
                                   col = c("indianred2", "green2"),
                                   main = "Distribution of NAs",
                                   xlab = "Time Lapse", ylab = NULL,
                                   colborder = "black", xangle = 0,
                                   theme = ggplot2::theme_minimal(), ...) {
  .Deprecated(
    new = "plotNA_distributionBar",
    msg = "plotNA.distributionBar will be replaced by plotNA_distributionBar
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  plotNA_distributionBar(
    x, breaks, breaksize, percentage, legend,
    axes, space, col, main, xlab, ylab, colborder, xangle,
    theme, ...
  )
}
