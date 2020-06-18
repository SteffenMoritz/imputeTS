#' @title Visualize Distribution of Missing Values
#'
#' @description Visualize the distribution of missing values within a time series.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object containing NAs
#'
#' @param color_points Point color for observations
#' @param color_missing Background color used for showing NA sequences
#' @param alpha_missing Transparency value for background used for showing NA sequences
#' @param color_lines Line color
#' @param title Title of the plot (NULL for deactivating title)
#' @param subtitle Subtitle of the plot (NULL for deactivating subtitle)
#' @param xlab Label for x axis
#' @param ylab Label for y axis
#' @param shape_points Plotting 'character', i.e., symbol to use
#' @param size_points Character (or symbol) expansion: a numerical vector
#' @param theme Set a theme for ggplot2. Default is \code{\link[ggplot2]{theme_linedraw}}
#'
#' @details This function visualizes the distribution of missing values within a time series.
#' If a value is NA, the background is colored differently. This gives a good overview
#' of where most missing values occur.
#'
#'
#' @author Steffen Moritz
#'
#' @seealso \code{\link[imputeTS]{ggplot_na_intervals}},
#'  \code{\link[imputeTS]{ggplot_na_gapsize}}, \code{\link[imputeTS]{ggplot_na_imputations}}
#'
#' @examples
#' # Example 1: Visualize the missing values in x
#' # x <- ts(c(1:11, 4:9,NA,NA,NA,11:15,7:15,15:6,NA,NA,2:5,3:7))
#' # ggplot_na_distribution(x)
#'
#' # Example 2: Visualize the missing values in tsAirgap time series
#' ggplot_na_distribution(tsAirgap)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' x <- ts(c(1:11, 4:9, NA, NA, NA, 11:15, 7:15, 15:6, NA, NA, 2:5, 3:7))
#' x %>% ggplot_na_distribution()
#' @importFrom ggplot2 ggplot geom_point aes geom_line geom_bar ggtitle xlab ylab
#' theme theme_minimal element_text
#' @importFrom stats ts
#' @importFrom magrittr %>%
#' @export
ggplot_na_distribution <- function(x, 
                                color_points = "steelblue", 
                                color_lines = "steelblue2",
                                color_missing = "indianred", 
                                alpha_missing = 0.5,
                                title = "Distribution of Missing Values",
                                subtitle = "Time Series with highlighted missing regions",
                                xlab = "Time", 
                                ylab = "Value",
                                shape_points = 20, 
                                size_points = 2.5,
                                theme = ggplot2::theme_linedraw()) {
  data <- x

  
  ##### Alles in vector
  ##### Time extrahieren

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

  ## dataframe for ggplot
  time <- seq_along(data)
  value <- data
  df <- data.frame(time, value)

  ## Plot the line diagram of known values  ##############
  gg <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = df, na.rm = T,
      ggplot2::aes(x = time, y = value), col = color_lines
    ) +
    ggplot2::ggtitle(label = title, subtitle = subtitle) +
    xlab(xlab) +
    ylab(ylab) +
    ggplot2::geom_point(
      data = df, na.rm = T,
      ggplot2::aes(x = time, y = value), shape = shape_points,
      col = color_points, size = size_points
    ) +
    theme 


  if (length(id_na) > 0) {
    # Red Bars only if missing data in time series
    na_val <- max(df$value, na.rm = T)
    gg <- gg +
      ggplot2::geom_bar(
        data = df[is.na(df$value), ], stat = "identity",
        ggplot2::aes(x = time, y = na_val),
        col = color_missing, fill = color_missing, alpha = alpha_missing,
      )
  }

  return(gg)
}
