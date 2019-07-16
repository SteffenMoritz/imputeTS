#' @title Visualize Distribution of Missing Values
#'
#' @description Visualize the distribution of missing values within a time series.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object containing NAs
#'
#' @param colPoints Point color for observations
#' @param colBackgroundMV Background color for NA sequences
#' @param col Line color
#' @param main Main title
#' @param xlab Label for x axis
#' @param ylab Label for y axis
#' @param pch Plotting 'character', i.e., symbol to use
#' @param cexPoints Character (or symbol) expansion: a numerical vector
#' @param theme Set a theme for ggplot2. Default is \code{\link[ggplot2]{theme_minimal}}
#' @param ... These parameters are passed to \code{\link[ggplot2]{geom_line}}
#'
#' @details This function visualizes the distribution of missing values within a time series.
#' If a value is NA, the background is colored differently. This gives a good overview
#' of where most missing values occur.
#'
#'
#' @author Steffen Moritz
#'
#' @seealso \code{\link[imputeTS]{plotNA_distributionBar}},
#'  \code{\link[imputeTS]{plotNA_gapsize}}, \code{\link[imputeTS]{plotNA_imputations}}
#'
#' @examples
#' # Example 1: Visualize the missing values in x
#' # x <- ts(c(1:11, 4:9,NA,NA,NA,11:15,7:15,15:6,NA,NA,2:5,3:7))
#' # plotNA_distribution(x)
#'
#' # Example 2: Visualize the missing values in tsAirgap time series
#' plotNA_distribution(tsAirgap)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' x <- ts(c(1:11, 4:9, NA, NA, NA, 11:15, 7:15, 15:6, NA, NA, 2:5, 3:7))
#' x %>% plotNA_distribution()
#' @importFrom ggplot2 ggplot geom_point aes geom_line geom_bar ggtitle xlab ylab
#' theme theme_minimal element_text
#' @importFrom stats ts
#' @importFrom magrittr %>%
#' @export
plotNA_distribution <- function(x, colPoints = "steelblue", colBackgroundMV = "indianred2",
                                main = "Distribution of NAs", xlab = "Time", ylab = "Value",
                                pch = 20, cexPoints = 2.5, col = "black",
                                theme = ggplot2::theme_minimal(), ...) {
  data <- x

  ##
  ## Input check ##############
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

  id_na <- which(is.na(data))

  ##
  ## Plotting Code  ##############
  ##

  ## ggplot dataframe
  df <- data.frame(
    time = 1:length(data),
    value = data
  )

  ## Plot the line diagram of known values  ##############
  gg <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = df[!is.na(df$value), ],
      ggplot2::aes(x = df[!is.na(df$value), ]$time, y = df[!is.na(df$value), ]$value), shape = pch,
      col = colPoints, size = cexPoints
    ) +
    ggplot2::geom_line(
      data = df[!is.na(df$value), ],
      ggplot2::aes(x = df[!is.na(df$value), ]$time, y = df[!is.na(df$value), ]$value), col = col, ...
    ) +
    ggplot2::ggtitle(label = main) +
    xlab(xlab) + ylab(ylab) +
    theme +
    ggplot2::theme(plot.title = element_text(hjust = 0.5))

  if (length(id_na) > 0) {
    # Red Bars only if missing data in time series
    na_val <- max(df$value, na.rm = T)
    gg <- gg +
      ggplot2::geom_bar(
        data = df[is.na(df$value), ], stat = "identity",
        ggplot2::aes(x = time, y = na_val),
        col = colBackgroundMV, fill = colBackgroundMV
      )
  }

  return(gg)
}



#' Deprecated use \code{\link[imputeTS]{plotNA_distribution}} instead.
#' @description plotNA.distribution is replaced by \code{\link[imputeTS]{plotNA_distribution}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams plotNA_distribution
#' @keywords internal
#' @export
plotNA.distribution <- function(x, colPoints = "steelblue", colBackgroundMV = "indianred2",
                                main = "Distribution of NAs", xlab = "Time", ylab = "Value",
                                pch = 20, cexPoints = 2.5, col = "black",
                                theme = ggplot2::theme_minimal(), ...) {
  .Deprecated(
    new = "plotNA_distribution",
    msg = "plotNA.distribution will be replaced by plotNA_distribution.
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  plotNA_distribution(
    x, colPoints, colBackgroundMV,
    main, xlab, ylab, pch, cexPoints, col, theme, ...
  )
}
