#' @title Visualize Occurrences of NA gap sizes
#'
#' @description Visualize the Number of Occurrences for existing NA Gap Sizes
#' (NAs in a row) in a Time Series
#'
#' @param x Numeric Vector (\code{\link[base]{vector}}) or Time Series
#' (\code{\link[stats]{ts}}) object containing NAs. This is the only mandatory
#' parameter - all other parameters are only needed for adjusting the plot appearance.
#'
#' @param limit Specifies how many of the most common gap sizes are shown in
#' the plot.Default is 10. So only the 10 most often occurring gapsizes will
#' be shown. If more or all present gap sizes should be displayed, the limit needs
#' to be increased. Since this might add a lot of additional data, having
#' parameter  \code{orientation} set to 'horizontal' avoids overlaps in the axis
#' labels.
#'
#' @param include_total When set to TRUE the total NA count for a gapsize is
#' included in the plot (total = number occurrence x gap size).
#' E.g. if a gapsize of 3 occurs 10 times, this means this gap size makes
#' up for 30 NAs in total. This can be a good indicator of the
#' overall impact of a gapsize.
#'
#' @param ranked_by Should the results be sorted according to the number of
#' occurrence or total resulting NAs for a gapsize. Total resulting NAs
#' are calculated by (total = number occurrence x gap size).
#' \itemize{
#'    \item{"occurrence" - Sorting by 'number of occurrence' of a gap size}
#'
#'    \item{"total" - Sorting by 'total resulting NAs' of a gap size}
#'    }
#'
#'    The default setting is "occurrence".
#'
#' @param color_occurrence Defines the Color for the Bars of
#' 'number of occurrence'.
#'
#' @param color_total Defines the color for the bars of
#' 'total resulting NAs'.
#'
#' @param title Title of the Plot.
#'
#' @param subtitle Subtitle of the Plot.
#'
#' @param xlab Label for x-Axis.
#'
#' @param ylab Label for y-Axis.
#'
#' @param legend If TRUE a legend is added at the bottom.
#'
#' @param orientation Can be either 'vertical' or 'horizontal'. Defines
#' if the bars are plotted vertically or horizontally. For large amounts
#' of different gap sizes horizontal illustration is favorable (also see
#' parameter \code{limit}).
#'
#' @param label_occurrence Defines the label assigned to 'number of occurrence'
#' in the legend.
#' @param label_total Defines the label assigned to 'total resulting NAs'
#' in the legend.
#'
#' @param theme Set a Theme for ggplot2. Default is ggplot2::theme_linedraw().
#' (\code{\link[ggplot2]{theme_linedraw})}
#'
#' @author Steffen Moritz, Sebastian Gatscha
#'
#' @return The output is a \code{\link[ggplot2]{ggplot2}} object that can be
#' further adjusted by using the ggplot syntax
#'
#' @details This plotting function can be used to visualize the length of
#' the NA gaps (NAs in a row) in a time series. It shows a ranking of which
#' gap sizes occur most often. This ranking can be ordered by the number
#' occurrence of the gap sizes or by total resulting NAs for this gap size
#' (occurrence * gap length). A NA-gap of 3 occuring 10 times means 30 total
#' resulting NAs.
#'
#' A resulting plot can for example be described like this:
#' a 2 NA-gap (2 NAs in a row) occurred  27 times,
#' a 9 NA-gap (9 NAs in a row) occurred  11 times,
#' a 27 NA-gap (27 NAs in a row) occurred  1 times, ...
#'
#' The only really needed parameter for this function is x (the univariate
#' time series with NAs that shall be visualized). All other parameters
#' are solely for altering the appearance of the plot.
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
#' @seealso \code{\link[imputeTS]{ggplot_na_distribution}},
#'   \code{\link[imputeTS]{ggplot_na_intervals}},
#'   \code{\link[imputeTS]{ggplot_na_imputations}}
#'
#' @examples
#' # Example 1: Visualize the top gap sizes in tsNH4 (top 10 by default)
#' ggplot_na_gapsize(tsNH4)
#'
#' # Example 2: Visualize the top gap sizes in tsAirgap - horizontal bars
#' ggplot_na_gapsize(tsAirgap, orientation = "vertical")
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' tsNH4 %>% ggplot_na_gapsize()
#'
#' # Example 4: Visualize the top 20 gap sizes in tsNH4
#' ggplot_na_gapsize(tsNH4, limit = 20)
#'
#' # Example 5: Visualize top gap sizes in tsNH4 without showing total NAs
#' ggplot_na_gapsize(tsNH4, limit = 20, include_total = FALSE)
#'
#' # Example 6: Visualize top gap sizes in tsNH4 but ordered by total NAs
#' # (total = occurrence * gap length)
#' ggplot_na_gapsize(tsNH4, limit = 20, ranked_by = "total")
#'
#' # Example 7: Visualize top gap sizes in tsNH4 - different theme
#' # Plot adjustments via ggplot_na_gapsize function parameters
#' ggplot_na_gapsize(tsNH4, theme = ggplot2::theme_classic())
#'
#' # Example 8: Visualize top gap sizes in tsNH4 - title, subtitle in center
#' # Plot adjustments via ggplot2 syntax
#' ggplot_na_gapsize(tsNH4) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
#'   ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 9: Visualize top gap sizes in tsNH4 - title in center, no subtitle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_gapsize(tsNH4, subtitle = NULL) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 10: Top gap sizes in tsNH4 - legend on the right and color change
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_gapsize(tsNH4, color_total = "grey") +
#'   ggplot2::theme(legend.position = "right")
#' @importFrom magrittr %>%
#'
#' @importFrom ggplot2 theme_linedraw ggplot geom_bar aes scale_x_discrete
#' scale_fill_manual ggtitle xlab ylab theme element_text element_blank
#' coord_flip theme_classic
#'
#' @export
ggplot_na_gapsize <- function(x,
                              limit = 10,
                              include_total = TRUE,
                              ranked_by = "occurrence",
                              color_occurrence = "indianred",
                              color_total = "steelblue",
                              title = "Occurrence of gap sizes",
                              subtitle = "Gap sizes (NAs in a row) ordered by most common",
                              xlab = NULL,
                              ylab = "Number occurrence",
                              legend = TRUE,
                              orientation = "horizontal",
                              label_occurrence = "Number occurrence gapsize",
                              label_total = "Resulting NAs for gapsize",
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
    for creating a meaningful ggplot_na_gapsize plot)")
  }
  
  # exclude inputs without NAs
  if (!anyNA(data)) {
    stop("Input data contains no NAs. At least one missing value is needed 
         to create a meaningful ggplot_na_gapsize plot)")
  }
  
 

  ##
  ## End Input Check and Transformation
  ##

  

  ##
  ## 2. Preparations
  ##


  # 2.1 Create required data
  

  # Calculation consecutive NA information
  rle_na <- rle(is.na(data))
  vec <- rle_na$lengths[rle_na$values]
  occurrence_bar <- table(vec)
  gaps_vec <- as.integer(names(occurrence_bar))
  totals_bar <- occurrence_bar * gaps_vec
  labels1 <- paste0(gaps_vec, " NA-gap")



  # 2.2 Adjust to parameter selection by user

  # Sorting for ranked_by param
  if (ranked_by == "occurrence") {
    # sort according to occurrence of gapsizes
    fooind <- order(occurrence_bar)
    occurrence_bar <- occurrence_bar[fooind]
    totals_bar <- totals_bar[fooind]
    labels1 <- labels1[fooind]
  } else if (ranked_by == "total") {
    # sort accoding to total NAs
    fooind <- order(totals_bar)
    occurrence_bar <- occurrence_bar[fooind]
    totals_bar <- totals_bar[fooind]
    labels1 <- labels1[fooind]
  }
  else {
    stop("Wrong input for parameter ranked_by. Input must be either 'occurrence' or 'total'.
    Call ?ggplot_na_gapsize to view the documentation.")
  }

  # Adjust to show only a limited amount of bars for limit param
  if (length(occurrence_bar) > limit) {
    occurrence_bar <- occurrence_bar[(length(occurrence_bar) - limit + 1):length(occurrence_bar)]
    totals_bar <- totals_bar[(length(totals_bar) - limit + 1):length(totals_bar)]
    labels1 <- labels1[(length(labels1) - limit + 1):length(labels1)]
  }



  # 2.3 Create dataframe for ggplot2

  # data.frame for ggplot
  id <- seq_along(occurrence_bar)
  val <- c(occurrence_bar, totals_bar)
  label <- c(
    rep("occurrence_bar", length(occurrence_bar)),
    rep("totals_bar", length(totals_bar))
  )
  df <- data.frame(id, val, label)

  # Only number of occurrences bar
  if (include_total == FALSE) {
    df <- subset(df, label == "occurrence_bar")
  }

  ##
  ## End Preparations
  ##



  ##
  ## 3. Create the ggplot2 plot
  ##


  # Create ggplot
  gg <- ggplot2::ggplot(data = df) +
    ggplot2::geom_bar(aes(x = id, y = val, fill = label),
      color = "black",
      stat = "identity", position = "dodge"
    ) +
    ggplot2::scale_x_discrete(
      labels = labels1,
      limits = labels1
    ) +
    ggplot2::scale_fill_manual(
      values = c(color_occurrence, color_total),
      labels = c(label_occurrence, label_total)
    ) +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    theme +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      legend.title = ggplot2::element_blank()
    )

  # For flipping from vertical to horizontal bars
  if (orientation == "horizontal") {
    gg <- gg + ggplot2::coord_flip()
  }

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
