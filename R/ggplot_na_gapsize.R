#' @title Visualize Distribution of NA gap sizes
#'
#' @description Visualize Distribution of NA gap sizes (NAs in a row) in
#' a time series
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series
#' (\code{\link{ts}}) object containing NAs
#'
#' @param limit Specifies how many of the most common gap sizes are shown in
#' the plot.Default is 10. So only the 10 most often occurring gapsizes will
#' be shown. If more or all present gap sizes should be displayed, the limit needs
#' to be increased. Since this might add a lot of additional data, having
#' parameter  \code{orientation}set to 'vertical' avoids overlaps in the axis
#' labels.
#'
#' @param include_total Should the resulting total NA count for a gapsize be
#' included.If a gapsize of 3 occurs 10 times, this means this gap size makes
#' up for 30 individual NAs in total. This can be a good indicator of the
#' overall impact of a gapsize.
#'
#' @param ranked_by Should the results be sorted according to number of
#' occurrence' or 'total resulting NAs' of a gapsize. Total resulting NAs
#' are calculated by (number occurrence x gap size). A gap of 3 occuring
#' 10 times means 30 total resulting NAs.
#'
#' \itemize{
#'    \item{"occurrence" - Sorting by 'number of occurrence' of a gap size}
#'
#'    \item{"total" - Sorting by 'total resulting NAs' of a gap size}
#'    }
#'
#'    The default setting is "occurrence".
#'
#' @param title Title of the plot
#'
#' @param subtitle Subtitle of the plot
#'
#' @param orientation Can be either 'vertical' or 'horizontal'. Defines
#' if the bars are plotted vertically or horizontally. For large amounts
#' of different gap sizes vertical illustration is favourable (also see
#' parameter \code{limit}).
#'
#' @param xlab Label for x axis
#' @param ylab Label for y axis
#'
#' @param legend If TRUE a legend is added at the bottom.
#'
#' @param color_occurrence Defines the color for the bars of
#' 'number of occurrence'.
#'
#' @param color_total Defines the color for the bars of
#' 'total resulting NAs'.
#'
#' @param label_occurrence Defines the label assigned to 'number of occurrence'
#' in the legend.
#' @param label_total Defines the label assigned to 'total resulting NAs'
#' in the legend.
#'
#' @param theme Set a theme for ggplot2. Default is
#' \code{\link[ggplot2]{theme_linedraw}}
#'
#' @author Steffen Moritz
#'
#' @return The output is a \code{\link[ggplot2]{ggplot2}} object that can be
#' further adjusted by using the ggplot syntax
#'
#' @details This plotting function can be used to visualize the length of
#' the NA gaps (NAs in a row) in a time series. It shows a ranking of which
#'  gap sizes occur most often. This ranking can be ordered by the number
#'  occurrence of the gap sizes or by total resulting NAs for this gap size
#'  (occurrence * gap length). A NA-gap of 3 occuring 10 times means 30 total
#'  resulting NAs.
#'
#'  A resulting plot can for example be described like this:
#'  a 2 NA-gap (2 NAs in a row) occurred  27 times,
#'  a 9 NA-gap (9 NAs in a row) occurred  11 times,
#'  a 27 NA-gap (27 NAs in a row) occurred  1 times, ...
#'
#'  It is important to note, that while the parameters of this plotting
#'  function provide ease of use, the resulting plot is basically a ggplot
#'  object. This can be altered to your formatting wishes and requirements
#'  with the  respective ggplot commands/syntax (see also Examples).
#'
#' @seealso \code{\link[imputeTS]{ggplot_na_distribution}},
#'   \code{\link[imputeTS]{ggplot_na_intervals}},
#'   \code{\link[imputeTS]{ggplot_na_imputations}}
#'
#' @examples
#' # Example 1: Visualize the top gap sizes in tsNH4
#' ggplot_na_gapsize(tsNH4)
#'
#' # Example 2: Visualize the top gap sizes in tsAirgap
#' ggplot_na_gapsize(tsAirgap)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' tsNH4 %>% ggplot_na_gapsize()
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot geom_bar aes position_stack
#' scale_fill_manual scale_x_discrete coord_flip theme_minimal
#' theme element_text element_blank xlab ylab ggtitle scale_color_manual
#' @export
ggplot_na_gapsize <- function(x,
                              limit = 10,
                              include_total = T,
                              ranked_by = "occurrence",
                              title = "Occurrence of gap sizes",
                              subtitle = "Gap sizes (NAs in a row) ordered by most common",
                              orientation = "vertical",
                              xlab = NULL, ylab = "Number occurrence",
                              legend = TRUE,
                              color_occurrence = "indianred",
                              color_total = "steelblue",
                              label_occurrence = "Number occurrence gapsize",
                              label_total = "Resulting NAs for gapsize",
                              theme = ggplot2::theme_linedraw()) {
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
  if (!any(is.na(data))) {
    stop("No missing values in the input - nothing to plot")
  }

  # Change zoo, xts, timeSeries objects to vector to avoid errors
  if (is.ts(data)) {
    data <- as.vector(data)
  }


  ##
  ## Calculations
  ##

  ## Calculation consecutive NA information ##
  rle_na <- rle(is.na(data))
  vec <- rle_na$lengths[rle_na$values]
  occurrence_bar <- table(vec)
  gaps_vec <- as.integer(names(occurrence_bar))
  totals_bar <- occurrence_bar * gaps_vec
  labels1 <- paste0(gaps_vec, " NA-gap")


  ## Sorting ##
  if (ranked_by == "occurrence") {
    # sort accoding to occurrence of gapsizes
    fooind <- order(occurrence_bar)
    occurrence_bar <- occurrence_bar[fooind]
    totals_bar <- totals_bar[fooind]
    labels1 <- labels1[fooind]
  } else {
    # sort accoding to total NAs
    fooind <- order(totals_bar)
    occurrence_bar <- occurrence_bar[fooind]
    totals_bar <- totals_bar[fooind]
    labels1 <- labels1[fooind]
  }

  ## Adjust to show only a limited amount of bars (limit)
  if (length(occurrence_bar) > limit) {
    occurrence_bar <- occurrence_bar[(length(occurrence_bar) - limit + 1):length(occurrence_bar)]
    totals_bar <- totals_bar[(length(totals_bar) - limit + 1):length(totals_bar)]
    labels1 <- labels1[(length(labels1) - limit + 1):length(labels1)]
  }

  ##
  ## Plotting Code
  ##



  # data.frame for ggplot
  id <- seq_along(occurrence_bar)
  val <- c(occurrence_bar, totals_bar)
  label <- c(
    rep("occurrence_bar", length(occurrence_bar)),
    rep("totals_bar", length(totals_bar))
  )
  df <- data.frame(id, val, label)


  # Only number of occurrences bar
  if (include_total == F) {
    df <- subset(df, label == "occurrence_bar")
  }

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
  if (orientation == "vertical") {
    gg <- gg + ggplot2::coord_flip()
  }

  # Removing legend
  if (!legend) {
    gg <- gg +
      ggplot2::theme(
        legend.position = "none",
      )
  }


  return(gg)
}
