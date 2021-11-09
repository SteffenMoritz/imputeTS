#' @title Dotplot of Value Distribution directly before/after NAs
#'
#' @description Visualize the distribution of values directly
#' before/after NAs via a dotplot.
#' Useful to determine if missing values appear more often when a certain
#' threshold level is reached.
#'
#' @param x Numeric Vector (\code{\link[base]{vector}}) or Time Series
#' (\code{\link[stats]{ts}}) object containing NAs.
#' This is the only mandatory parameter - all other parameters are
#' only needed for adjusting the plot appearance.
#'
#' @param number_bins Number of bins of stacked observations to
#' be created. Default is length of time series divided
#' by ten - but with a minimum of 30 bins.
#'
#' @param color_before Color for the dots representing observations
#' directly before NA gaps.
#'
#' @param color_after Color for the dots representing
#' observations directly after NA gaps.
#'
#' @param color_regular Color for the dots representing all values
#' that are not next to NA observations.
#'
#' @param title Title of the plot (NULL for deactivating title).
#'
#' @param subtitle Subtitle of the plot (NULL for deactivating subtitle).
#'
#' @param xlab Label for x-Axis.
#'
#' @param ylab Label for y-Axis.
#'
#' @param legend If TRUE a legend is added at the bottom.
#'
#' @param legend_title Title for the legend.
#'
#' @param orientation Can be either 'vertical' or 'horizontal'. Defines
#' if the  plot is oriented vertically or horizontally.
#'
#' @param label_before Defines the legend label assigned to the
#' observations directly before NAs.
#'
#' @param label_after Defines the legend label assigned to the
#'  observations directly after NAs.
#'
#' @param label_regular Defines the legend label assigned to
#' the observations, that are not next to NA values.
#'
#' @param theme Set a Theme for ggplot2. Default is ggplot2::theme_linedraw().
#' (\code{\link[ggplot2]{theme_linedraw})}
#'
#' @details This function visualizes the distribution of missing
#' values directly before/after NAs via a dotplot. This is useful to
#' determine if missing values appear more often when near to a
#' certain value level.
#'
#' In a \link[ggplot2]{geom_dotplot} each dot represents one observation
#' in the time series. It can be directly seen how many values
#' are stacked into a bin (a value range).
#' 
#' The ggplot_na_level plot makes use of this and additionally 
#' colors observations before and after NAs differently.
#' 
#' The visualization of the before/after NA observations in a bin in
#' comparison to the regular observations can provide information
#' about the root cause of the missing values. It also can provide
#' indications, about the missing data mechanism (MCAR, MAR, MNAR).
#' 
#' By looking at this plot it can be seen whether the NAs appear
#' rather randomly after some values in the overall distribution or
#' if e.g. it can be said NAs more likely appear after high values.
#'
#' It could, for example be the case, that a sensor can't measure
#' values above 100 degree and always outputs NA values once the
#' temperature reaches 100 degree. With this plot, it can be realized,
#' that NAs in the next value always occur when the temperature is close
#' to 100 degree.
#' 
#' Thus, unusually high numbers of dots of before/after NA observations in a bin 
#' (in comparison the amount of dots of other observations in this bin)
#' should draw the users' attention.
#' 
#' 
#' The advantage of the dotplot of ggplot_na_level over the violin plots of ggplot_na_level2
#' is that each observation in the time series is really displayed as a dot in the dotplot.
#' For the user this can feel more intuitive. Especially, for very short time series 
#' the violins/boxplots and the summary statistics they provide are not so meaningful anymore. 
#' On the other hand, the ggplot_na_level is not a good choice for large time series. 
#' Drawing a visible dot for each observation comes to its limits, when the time series is larger 
#' than 500 observations. Also, while our assessment of distributions and anomalies usually 
#' works adequate on small amounts of data, we often struggle with large amounts of data. 
#' Here the violin/boxplot combination of ggplot_na_level2 is a great help.
#'
#'
#' The only really needed parameter for this function is x (the univariate
#' time series that shall be visualized). All other parameters are solely
#' for altering the appearance of the plot.
#'
#' As long as the input is univariate and numeric, the function also takes
#' data.frame, tibble, tsibble, zoo, xts as an input.
#'
#' The plot can be adjusted to your needs via the function parameters.
#' Additionally, for more complex adjustments, the output can also be
#' adjusted via ggplot2 syntax. This is possible, since the output
#' of the function is a ggplot2 object. Also take a look at the Examples
#' to see how adjustments are made.
#'
#' @author Steffen Moritz
#'
#' @seealso \code{\link[imputeTS]{ggplot_na_distribution2}},
#' \code{\link[imputeTS]{ggplot_na_gapsize}},
#' \code{\link[imputeTS]{ggplot_na_level2}},
#' \code{\link[imputeTS]{ggplot_na_distribution}},
#' \code{\link[imputeTS]{ggplot_na_imputations}}
#'
#' @examples
#' # Example 1: Visualize the before/after NA distributions 
#' x <- stats::ts(c(1:11, 4:9, NA, NA, NA, 11:15, 7:15, 15:6, NA, NA, 2:5, 3:7))
#' ggplot_na_level(x)
#'
#' # Example 2: Visualize the before/after in subset of tsNH4 time series, more bins
#' ggplot_na_level(tsNH4[1:500], number_bins = 100)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' x <- ts(c(1:11, 4:9, NA, NA, NA, 11:15, 7:15, 15:6, NA, NA, 2:5, 3:7))
#' x %>% ggplot_na_level()
#'
#' # Example 4: Visualize the before/after NA in tsAirgap - different color for violins
#' # Plot adjustments via ggplot_na_level function parameters
#' ggplot_na_level(tsAirgap, color_after = "green")
#'
#' # Example 5: Visualize before/after NA in tsAirgap - different theme and orientation
#' # Plot adjustments via ggplot_na_level function parameters
#' ggplot_na_level(tsAirgap, theme = ggplot2::theme_classic() , orientation = "horizontal")
#'
#' # Example 6: Visualize before/after NA in tsNH4 - title, subtitle in center
#' # Plot adjustments via ggplot2 syntax
#' ggplot_na_level(tsAirgap) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
#'   ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 7: Visualize before/after NA in tsAirgap - title in center, no subtitle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_level(tsAirgap, subtitle = NULL, orientation = "horizontal") +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 8: Visualize before/after NA in tsAirgap - y-axis texts with angle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_level(tsAirgap, color_regular = "grey") +
#'   ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 60, hjust = 1))
#'   
#' @importFrom stats ts
#'
#' @importFrom ggplot2 theme_linedraw ggplot geom_dotplot scale_fill_manual
#' scale_color_manual ggtitle xlab ylab scale_y_continuous
#' layer_data coord_flip theme element_text theme_classic
#'
#' @importFrom magrittr %>%
#'
#' @export

ggplot_na_level <- function(x,
                            number_bins = ifelse(length(x) / 10 < 30, 30, length(x) / 10),
                            color_before = "steelblue",
                            color_after = "yellowgreen",
                            color_regular = "azure2",
                            title = "Before/After Analysis",
                            subtitle = "Values before and after NAs",
                            xlab = NULL,
                            ylab = NULL,
                            legend = TRUE,
                            legend_title = "",
                            orientation = "vertical",
                            label_before = "before",
                            label_after = "after",
                            label_regular = "regular",
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
    stop("Input data consists only of NAs. Meaningful ggplot_na_level plots can only
       be generated with more non-NA data available.)")
  }
  
  # exclude inputs without NAs
  if (!anyNA(data)) {
    stop("Input data contains no NAs. At least one missing value is needed 
         to create a meaningful ggplot_na_level plot)")
  }



  ##
  ## End Input Check and Transformation
  ##




  ##
  ## 2. Preparations
  ##

  # 2.1 Calculate the before and after NA values

  # Get all indices of the data that come directly before and after an NA
  na_indx_after <- which(is.na(data[1:(length(data) - 1)])) + 1
  # starting from index 2 moves all indexes one in front, so no -1 needed for before
  na_indx_before <- which(is.na(data[2:length(data)]))

  # Get the actual values to the indices and put them in a data frame with a label
  if (length(na_indx_after) > 0) {
    after <- data.frame(id = "1", type = "3_after", input = na_remove(data[na_indx_after]))
  }
  else {
    after <- NULL
  }

  if (length(na_indx_before) > 0) {
    before <- data.frame(id = "1", type = "2_before", input = na_remove(data[na_indx_before]))
  }
  else {
    before <- NULL
  }
  all <- data.frame(id = "1", type = "1_regular", input = na_remove(data))

  

  # 2.2 Create dataframe for ggplot2

  # join the data together in one dataframe
  df <- rbind(before, all, after)

 

  ##
  ## End Preparations
  ##



  ##
  ## 3. Create the ggplot2 plot
  ##
  
  # Workaround for 'no visible binding' check() caused by ggplot2 vars
  type <- df$type
  input <- df$input
  

  # binwidth
  bin_width <- (max(df$input, na.rm = T) - min(df$input, na.rm = T)) / number_bins


  # Create the plot
  gg <- ggplot2::ggplot(data = df) +
    
    # Add dotplot / dots with precalculated bin width
    ggplot2::geom_dotplot(ggplot2::aes(x = input, fill = type, color = type),
      stackgroups = TRUE, binpositions = "all", method = "histodot", binwidth = bin_width
    )

  # Get fill color of the points right
  gg <- gg +
    ggplot2::scale_fill_manual(
      name = legend_title,
      values = c("1_regular" = color_regular, "2_before" = color_before, "3_after" = color_after),
      labels = c(label_regular, label_before, label_after),
    ) +

    # Border color is just always black
    ggplot2::scale_color_manual(
      name = legend_title,
      values = c("1_regular" = "black", "2_before" = "black", "3_after" = "black"),
      labels = c(label_regular, label_before, label_after),
    )


  # Add title, subtitle, scale, theme
  gg <- gg + ggplot2::ggtitle(label = title, subtitle = subtitle) +
    ggplot2::xlab(xlab) +
    # y-scale is meaniningless for dotplots
    ggplot2::scale_y_continuous(name = ylab, breaks = NULL) +
    theme

  # Add legend
  gg <- gg + ggplot2::theme(
    legend.position = base::ifelse(legend == TRUE, "bottom", "none"),
  )

  # Get maximum count per bin, to adjust aspect.ratio automatically
  # Otherwise there are out-of-plot issues due to dotplot behavior
  max_count_per_bin <- max(ggplot2::layer_data(gg, 1)$count)

  # Code for horizontal or vertical
  if (orientation == "horizontal") {
    gg <- gg + ggplot2::coord_flip()
    # Adjust aspect ratio to prevent cutting information
    gg <- gg + ggplot2::theme(aspect.ratio = (number_bins - 10) / max_count_per_bin)
  }
  else {
    # Adjust aspect ratio to prevent cutting information
    gg <- gg + ggplot2::theme(aspect.ratio = max_count_per_bin / (number_bins - 10))
  }


  ##
  ##  End creating the ggplot2 plot
  ##


  return(gg)
}
