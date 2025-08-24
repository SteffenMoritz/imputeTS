#' @title Bubble Plot to Visualize Total NA Count of NA gap sizes
#'
#' @description Visualize the total NA count (gap size * occurrence) for
#' the existing gaps sizes (NAs in a row).
#' 
#' @param x Numeric Vector (\code{\link[base]{vector}}) or Time Series
#' (\code{\link[stats]{ts}}) object containing NAs. This is the only
#' mandatory parameter - all other parameters are only needed for adjusting
#' the plot appearance.
#'
#' @param colors_bubbles Choose a color gradient that encodes lower to
#' higher total NA counts.
#' Color codes can be given as vector. Using color palettes from  colorspace,
#' grDevices, RColorBrewer or other packages is useful here.
#' E.g. grDevices::heat.colors(10) would be a possible input.
#'
#' @param color_border Color for the border of the bubbles.
#'
#' @param alpha_bubbles Alpha (transparency) value used for filling the bubbles.
#'
#' @param labels_bubbles Should labels be added to the individual bubbles inside
#' the plot.
#' For many datasets there will be overplotting issues once labels are added.
#' In these cases using the min_gapsize, min_totals or min_occurrence options
#' might be useful to only display the most relevant gap sizes.
#'
#' You can choose between these labels to be added:
#' \itemize{
#'    \item{"none" - No label gets added to the bubbles}
#'    (default choice)
#'
#'    \item{"gap" - Adds a label displaying the gap size belonging to the
#'    respective bubble}
#'
#'    \item{"total" - Adds a label displaying the total NA count for the
#'    respective bubble}
#'
#'    \item{"gap-occurrence" - Adds a label displaying the respective
#'    gap size and number of its occurrence}
#'    }
#'
#'    The default setting is "none".
#'
#' @param size_bubbles Allows to scale the size of the bubbles.
#' Some experimenting with this parameter might be needed to get
#' a good visualization for your specific dataset.
#'
#' @param min_totals Only print bubbles for gap sizes that account
#' for at least min_totals NAs in the time series.
#'
#' @param min_occurrence Only print bubbles for gap sizes that occur at least
#' min_occurrence times in the time series.
#'
#' @param min_gapsize Only show gap sizes larger than min_gapsize. Together with
#' max_gapsize enables zooming into in certain regions of interest.
#'
#' @param max_gapsize Only show gapsizes smaller than max_gapsize. Together with
#' min_gapsize enables zooming into in certain regions of interest.
#'
#' @param title Title of the Plot.
#'
#' @param subtitle Subtitle of the Plot.
#'
#' @param xlab Label for x-Axis.
#'
#' @param ylab Label for y-Axis.
#'
#' @param legend If TRUE a legend is added on the right side
#'
#' @param legend_breaks Number of displayed breaks / labels in the legend.
#' Needs an integer giving the desired number of breaks as input. Breakpoints are
#' internally calculated by R's pretty() function, which can also lead to
#' values slightly smaller or larger than the desired number.
#'
#'
#' @param legend_title Defines the title of the legend.
#'
#' @param legend_position Defines position of the legend. Choose either
#' 'bottom', right', 'left' or 'top'.
#'
#' @param legend_point_sizes Defines the size of the symbols representing the total
#' NA bubbles in the legend.
#'
#' You can choose between "default", "actual" or a custom vector of sizes.
#'
#' \itemize{
#'    \item{"default" - Scales the points in the legend to symbolically
#'    resemble the size differences} (default choice)
#'
#'    \item{"actual" - Scales the points in the legend according
#'    to their actual size in the plot}
#'    }
#'
#' Since these two options are not be always sufficient, a custom vector of
#' sizes can be used as input. This would look like this: c(4,5,6,7). Be
#' aware, that the length of this vector must match the number of breakpoints
#' (can be adjusted with legend_breaks).
#'
#' @param theme Set a theme for ggplot2. Default is ggplot2::theme_linedraw().
#' (\code{\link[ggplot2]{theme_linedraw})}
#'
#' @author Steffen Moritz
#'
#' @return The output is a \code{\link[ggplot2]{ggplot2}} object that can be
#' further adjusted by using the ggplot syntax
#'
#' @details This function visualizes total NA counts by individual gap size
#' (consecutive NAs) in a time series. The bubble plot makes it easy to see
#' which gap sizes account for most of the NAs in the series. The size and
#' color of the bubbles represent the total number of NAs a given gap size
#' accounts for.
#'
#' Total NAs for a gap size are calculated as follows:
#' total NAs = occurrence * gap length
#'
#' For example, interpret a bubble for gap size 2 as follows:
#' a 2-NA gap (two NAs in a row) occurred 27 times in the time series and thus
#' accounts for 54 total NAs.
#'
#' On the x-axis, the different gap sizes are plotted in increasing order.
#' The y-axis shows the occurrence count of these gap sizes in the time series.
#'
#' The plot is useful for investigating possible root causes of the missing
#' data. It can indicate whether the missing data are random or whether there
#' are patterns of interest.
#'
#' Depending on the input time series, there might be too much information in
#' the plot, leading to overplotting. In these cases, use the parameters
#' \code{min_totals}, \code{min_occurrence}, and \code{min_gapsize} to display
#' only the information of interest.
#'
#' The only required parameter is \code{x} (the univariate time series with NAs
#' to visualize). All other parameters alter the appearance of the plot.
#'
#' As long as the input is univariate and numeric, the function also accepts
#' \code{data.frame}, \code{tibble}, \code{tsibble}, \code{zoo}, or \code{xts}
#' input.
#'
#' The plot can be adjusted via function parameters. For more complex
#' adjustments, you can modify the result using ggplot2 syntax, since the
#' function returns a ggplot2 object. See the Examples for typical adjustments.
#' 
#'
#' @seealso \code{\link[imputeTS]{ggplot_na_distribution}},
#'   \code{\link[imputeTS]{ggplot_na_distribution2}},
#'   \code{\link[imputeTS]{ggplot_na_gapsize}},
#'   \code{\link[imputeTS]{ggplot_na_imputations}}
#'
#' @examples
#' # Example 1: Visualize total NA counts in tsNH4
#' ggplot_na_gapsize2(tsNH4)
#'
#' # Example 2: Visualize total NA counts in tsNH4, different color gradient
#' ggplot_na_gapsize2(tsNH4, colors_bubbles = rev(grDevices::heat.colors(10)))
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' tsNH4 %>% ggplot_na_gapsize2()
#'
#' # Example 4: Visualize total NA counts in tsHeating
#' # Limited to gap sizes that account for a total of > 600 NAs
#' ggplot_na_gapsize2(tsHeating, min_totals = 600)
#'
#' # Example 5: Visualize total NA counts in tsNH4 - no legend
#' ggplot_na_gapsize2(tsNH4, legend = FALSE)
#'
#' # Example 6: Visualize total NA counts in tsAirgap - increased bubble size
#' ggplot_na_gapsize2(tsAirgap, size_bubbles = 35)
#'
#' # Example 7: Visualize total NA counts in tsNH4
#' # Plot adjustments via ggplot_na_gapsize2 function parameters
#' ggplot_na_gapsize2(tsNH4, theme = ggplot2::theme_classic())
#'
#' # Example 8: Visualize total NA counts in tsNH4 - title, subtitle in center
#' # Plot adjustments via ggplot2 syntax
#' ggplot_na_gapsize2(tsNH4) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
#'   ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 9: Visualize total NA counts in tsNH4 - title in center, no subtitle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_gapsize2(tsNH4, subtitle = NULL) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 10: Total NA counts in tsNH4 - legend on the bottom and color change
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_gapsize2(tsNH4, colors_bubbles = grDevices::heat.colors(10)) +
#'   ggplot2::theme(legend.position = "bottom")
#' @importFrom magrittr %>%
#'
#' @importFrom ggplot2 theme_linedraw ggplot aes geom_point scale_size_identity
#' geom_text scale_x_continuous scale_y_continuous scale_fill_gradientn
#' guide_legend ggtitle xlab ylab  theme element_text theme_classic
#'
#' @importFrom grDevices heat.colors
#'
#' @export
ggplot_na_gapsize2 <- function(x,
                               colors_bubbles = c("#FCFBFF", "#EFEEFA", "#DDDAEF",
                                                  "#C8C3E2", "#B1AAD4", "#9A8FC4",
                                                  "#8273B5", "#6B56A7", "#553695",
                                                  "#3D1778"),
                               color_border = "black",
                               alpha_bubbles = 0.4,
                               labels_bubbles = "none",
                               size_bubbles = 25,
                               min_totals = NULL,
                               min_occurrence = NULL,
                               min_gapsize = NULL,
                               max_gapsize = NULL,
                               title = "Gap Size Analysis",
                               subtitle = "Total NA counts for different gapsizes",
                               xlab = "Gapsize",
                               ylab = "Number occurrence",
                               legend = TRUE,
                               legend_breaks = 4,
                               legend_title = "Total NAs",
                               legend_position = "right",
                               legend_point_sizes = "default",
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
    for creating a meaningful ggplot_na_gapsize2 plot)")
  }

  # exclude inputs without NAs
  if (!anyNA(data)) {
    stop("Input data contains no NAs. At least one missing value is needed
         to create a meaningful ggplot_na_gapsize2 plot)")
  }



  ##
  ## End Input Check and Transformation
  ##



  ##
  ## 2. Preparations
  ##


  # 2.1 Create required data


  # Calculation consecutive NA information
  rle_na <- base::rle(is.na(data))
  vec <- rle_na$lengths[rle_na$values]
  gap_table <- table(vec)
  gap_names <- as.integer(names(gap_table))
  occurrences <- as.integer(gap_table)
  totals <- occurrences * gap_names




  # 2.2 Create dataframe for ggplot2

  df <- data.frame(gap = gap_names, occurrence = occurrences, total = totals)


  # 2.3 Adjust data to user selected parameters / filter

  # Filters to display only subsets of the data

  # Maximum Gapsize
  if (!is.null(max_gapsize)) {
    df <- subset(df, gap <= max_gapsize)
  }
  # Minimum gapsize
  if (!is.null(min_gapsize)) {
    df <- subset(df, gap >= min_gapsize)
  }
  # Minimum Total NAs
  if (!is.null(min_totals)) {
    df <- subset(df, total >= min_totals)
  }
  # Minimum Occurrence NAs
  if (!is.null(min_occurrence)) {
    df <- subset(df, occurrence >= min_occurrence)
  }
  
  # Error for too restrictive filters leaving no NA data to display
  if (length(df$gap) < 1) {
    stop("Too restrictive filter options set - nothing to display left.
          Your setting of either max_gapsize, min_gapsize, min_totals, min_occurrence or the
          combination of them left no NA data to display.)")
  }
  

  # 2.4 Calculate legend breaks and sizes


  # Create legend break points with pretty function.
  # Only use points within limits - otherwise there will be an error
  leg_breaks <- base::pretty(df$total, n = legend_breaks)
  leg_breaks <- leg_breaks[leg_breaks >= min(df$total) & leg_breaks <= max(df$total)]

  # Prevent empty breaks, when pretty() only chooses values outside limits
  if (length(leg_breaks) == 0) {
    leg_breaks <- totals[1]
  }

  # Define size of points in legend

  # Manual definition of legend point size
  if (is.numeric(legend_point_sizes)) {
    if (length(legend_point_sizes) == length(leg_breaks)) {
      leg_sizes <- legend_point_sizes
    }
    else {
      stop("When you input your own custom values for the size of the points in the legend,
             make sure your vector has the same size as are breaks in the legend.")
    }
  }
  # Scale points in the  legend with a symbolic, sensible size
  else if (legend_point_sizes == "default") {
    leg_sizes <- seq(from = 3, by = 2, length.out = length(leg_breaks))
  }
  # Scale points in the legend according to their actual size in the plot
  else if (legend_point_sizes == "actual") {
    leg_sizes <- leg_breaks / (max(df$total) / size_bubbles)
  }
  else {
    stop("Wrong values for parameter legend_pont_sizes chosen.
  To influence the size of points in the legend,
  either choose 'default', 'actual' or give a vector with your own desired sizes.
  This custom vector needs to have exactly as many elements as the legend has breaks")
  }


  ##
  ## End Preparations
  ##



  ##
  ## 3. Create the ggplot2 plot
  ##

  # Workaround for 'no visible binding' check() caused by ggplot2 vars
  gap <- df$gap
  occurrence <- df$occurrence
  total <- df$total
  

  # Create ggplot

  gg <- ggplot2::ggplot(data = df, ggplot2::aes(x = gap, y = occurrence)) +
    ggplot2::geom_point(
      alpha = alpha_bubbles, ggplot2::aes(
        fill = total, size =
          total / (max(total) / size_bubbles)
      ),
      color = color_border, pch = 21
    ) +
    ggplot2::scale_size_identity()

  # What to appear in the label, default no label
  if (labels_bubbles == "gap-occurrence") {
    gg <- gg + ggplot2::geom_text(ggplot2::aes(label = paste0(gap, "-gap\n", occurrence, "x")),
      size = 2, alpha = 1, color = "black"
    )
  }
  else if (labels_bubbles == "gap") {
    gg <- gg + ggplot2::geom_text(ggplot2::aes(label = paste0(gap, "-gap")),
      size = 2, alpha = 1, color = "black"
    )
  }
  else if (labels_bubbles == "total") {
    gg <- gg + ggplot2::geom_text(ggplot2::aes(label = paste0(total)),
      size = 2, alpha = 1, color = "black"
    )
  }
  else if (labels_bubbles == "occurrence") {
    gg <- gg + ggplot2::geom_text(ggplot2::aes(label = paste0(occurrence,"x")),
      size = 2, alpha = 1, color = "black"
    )
  }

  gg <- gg + ggplot2::scale_x_continuous(
    expand = c(0.1, 0.1),
    breaks = function(x) unique(floor(base::pretty(seq(0, (max(x) + 1) * 1.1))))
  ) +
    ggplot2::scale_y_continuous(
      expand = c(0.1, 0.1),
      breaks = function(x) unique(floor(base::pretty(seq(0, (max(x) + 1) * 1.1))))
    ) +
    ggplot2::scale_fill_gradientn(
      colors = colors_bubbles,
      breaks = leg_breaks,
      guide = ggplot2::guide_legend(
        title = legend_title,
        override.aes = list(size = leg_sizes)
      )
    ) +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    theme +
    ggplot2::theme(
      legend.position = legend_position,
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
    )

  # Removing legend
  if (!legend) {
    gg <- gg +
      ggplot2::theme(
        legend.position = "none",
      )
  }



  ##
  ##  End creating the ggplot2 plot
  ##


  return(gg)
}
