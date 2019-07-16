#' @title Visualize Distribution of NA gap sizes
#'
#' @description Visualize Distribution of NA gap sizes (NAs in a row) in a time series
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object containing NAs
#'
#' @param limit Specifies how many of the top gap sizes are shown in the plot.
#'
#' @param byTotalNA For byTotalNA = TRUE the top gap sizes according to their overall weight are shown. (occurrence * gap size)
#' For byTotalNA = FALSE the top gap sizes are shown by their number of occurrence. (occurrence)
#'
#' @param legend If TRUE a legend is added at the bottom
#'
#' @param col A vector of colors for the bars or bar components
#' @param xlab Label for x axis
#' @param ylab Label for y axis
#' @param main Main title
#'
#' @param horiz Logical. If FALSE, the bars are drawn vertically with the first bar to the left.
#' If TRUE, the bars are drawn horizontally with the first at the bottom.
#'
#' @param axes Logical. If TRUE, a vertical (or horizontal, if horiz is true) axis is drawn.
#'
#' @param beside Logical If FALSE, the columns are stacked bars, and if TRUE the columns are juxtaposed bars.
#'
#' @param theme Set a theme for ggplot2. Default is \code{\link[ggplot2]{theme_minimal}}
#'
#' @param ... Additional graphical parameters can be passed to \code{\link[ggplot2]{geom_bar}}
#'
#' @author Steffen Moritz
#'
#' @details This plotting function can be used to visualize the length of the NA gaps (NAs in a row)
#'  in a time series. It shows a ranking of which gap sizes occur most often. This ranking can be ordered by
#'  total NAs for this gap size (occurrence * gap length) or by occurrence of the gap size.
#'  The outcome will be somethink like in the time series 2NAs in a row occurred  27times,
#'  4NAs in a row occurred  11 times,  7NAs in a row occurred 5 times, 1NA in a row occurred 3 times, ... .
#'
#' @seealso \code{\link[imputeTS]{plotNA_distribution}},\code{\link[imputeTS]{plotNA_distributionBar}},
#'   \code{\link[imputeTS]{plotNA_imputations}}
#'
#' @examples
#' # Example 1: Visualize the top gap sizes in tsNH4
#' plotNA_gapsize(tsNH4)
#'
#' # Example 2: Visualize the top gap sizes in tsAirgap
#' plotNA_gapsize(tsAirgap)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' tsNH4 %>% plotNA_gapsize()
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot geom_bar aes position_stack
#' scale_fill_manual scale_x_discrete coord_flip theme_minimal
#' theme element_text element_blank xlab ylab ggtitle scale_color_manual
#' @export
plotNA_gapsize <- function(x, limit = 10, byTotalNA = FALSE,
                           legend = TRUE, col = c("indianred", "steelblue"),
                           xlab = "Ranking of the different gap sizes",
                           ylab = "Number",
                           main = "Occurrence of gap sizes (NAs in a row)",
                           horiz = FALSE, axes = TRUE,
                           beside = TRUE, theme = ggplot2::theme_minimal(), ...) {
  data <- x

  ##
  ## Input check #################
  ##

  if (!is.null(dim(data)) && dim(data)[2] != 1) stop("Input x is not univariate")
  if (!is.numeric(data)) stop("Input x is not numeric")
  if (!any(is.na(data))) stop("No missing values in the input - nothing to plot")

  # Change zoo, xts, timeSeries objects to vector to avoid errors
  if (is.ts(data)) data <- as.vector(data)

  ## Calculation consecutive NA information #################
  rle_na <- rle(is.na(data))
  vec <- rle_na$lengths[rle_na$values]
  bars1 <- table(vec)
  gaps_vec <- as.integer(names(bars1))
  bars2 <- bars1 * gaps_vec
  labels1 <- paste0(gaps_vec, " NAs")


  ## Sorting #################
  if (byTotalNA) {
    # sort accoding to overall NAs
    fooind <- order(bars1)
    bars1 <- bars1[fooind]
    bars2 <- bars2[fooind]
    labels1 <- labels1[fooind]
  } else {
    # sort accoding to overall NAs
    fooind <- order(bars2)
    bars1 <- bars1[fooind]
    bars2 <- bars2[fooind]
    labels1 <- labels1[fooind]
  }


  ##
  ## Plotting Code #################
  ##

  ## Adjust to show only a limited amount of bars (limit)
  if (length(bars1) > limit) {
    bars1 <- bars1[(length(bars1) - limit + 1):length(bars1)]
    bars2 <- bars2[(length(bars2) - limit + 1):length(bars2)]
    labels1 <- labels1[(length(labels1) - limit + 1):length(labels1)]
  }

  df <- data.frame(
    id = 1:length(bars1),
    val = c(bars1, bars2),
    label = c(
      rep("bars1", length(bars1)),
      rep("bars2", length(bars2))
    )
  )

  if (beside) {
    gg <- ggplot2::ggplot(data = df) +
      ggplot2::geom_bar(aes(x = df$id, y = df$val, fill = df$label),
        color = "black",
        stat = "identity", position = "dodge", ...
      )
    # stat = "identity", position = "dodge")
  } else {
    gg <- ggplot2::ggplot(data = df) +
      ggplot2::geom_bar(aes(x = df$id, y = df$val, fill = df$label),
        color = "black",
        stat = "identity", position = ggplot2::position_stack(reverse = TRUE), ...
      )
    # stat = "identity", position = position_stack(reverse = TRUE))
  }

  gg <- gg +
    ggplot2::scale_x_discrete(
      labels = labels1,
      limits = labels1
    ) +
    ggplot2::scale_fill_manual(
      values = col,
      labels = c(
        "Num occurrence gapsize",
        "Total NAs for gapsize"
      )
    ) +
    ggplot2::ggtitle(main) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    theme +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  if (horiz) gg <- gg + ggplot2::coord_flip()

  if (legend) {
    gg <- gg +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
        legend.title = ggplot2::element_blank()
      )
  }

  if (!axes) {
    gg <- gg +
      theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  }

  return(gg)
}


#' Deprecated use \code{\link[imputeTS]{plotNA_gapsize}} instead.
#' @description plotNA.gapsize is replaced by \code{\link[imputeTS]{plotNA_gapsize}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams plotNA_gapsize
#' @keywords internal
#' @export
plotNA.gapsize <- function(x, limit = 10, byTotalNA = FALSE,
                           legend = TRUE, col = c("indianred", "steelblue"),
                           xlab = "Ranking of the different gap sizes",
                           ylab = "Number",
                           main = "Occurrence of gap sizes (NAs in a row)",
                           horiz = FALSE, axes = TRUE,
                           beside = TRUE, theme = ggplot2::theme_minimal(), ...) {
  .Deprecated(
    new = "plotNA_gapsize",
    msg = "plotNA.gapsize will be replaced by plotNA_gapsize
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  plotNA_gapsize(
    x, limit, byTotalNA, legend, col, xlab, ylab, main,
    horiz, axes, beside, theme, ...
  )
}
