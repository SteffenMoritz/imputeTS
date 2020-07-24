#' @title Visualize Distribution of Missing Values
#'
#' @description Visualize the distribution of missing values within a time series.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
#' object containing NAs. This is the ONLY necessary parameter, all other are are 
#' only needed if you want to adjust the resulting plot.
#' @param x_axis_labels For adding specific x-axis labels. Takes a vector (with the same length as x) 
#' of either Date or POSIXct objects as an input. Default (NULL) is using the 
#' observation number as  x-axis tick labels.
#' @param color_points Point color for observations
#' @param color_lines Line color
#' @param color_missing Color used for highlighting the time spans with NA values
#' @param color_missing_border Color used as border for time spans with NA values
#' @param alpha_missing Transparency value used for highlighting the time spans with NA values
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
    
    # 1.1 Check if the input is multivariate
    if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
      stop("x is not univariate. The function only works with univariate input for x. For data types with 
         multiple variables/columns only input the column you want to plot as parameter x.")
    }

    # 1.2 special handling data types
    if (any(class(data) == "tbl")) {
      data <- as.vector(as.data.frame(data)[, 1])
    }
    
    # 1.3 Checks and corrections for wrong data dimension
    
    # Altering multivariate objects with 1 column (which are essentially
    # univariate) to be dim = NULL
    if (!is.null(dim(data)[2])) {
      data <- data[, 1]
    }

    ## 1.4 Check if input is numeric
    if (!is.numeric(data)) {
      stop("Input x is not numeric")
    }

    ##
    ## End Input Check and Transformation
    ##

    

    ##
    ## 2. Preparations
    ##

    # 2.1 Create required data

    # Input as vector
    data <- as.vector(data)

    # Get NA positions
    id_na <- which(is.na(data))


    
    # 2.2 Create dataframe for ggplot2

    # Define x-axis label data
    # if Date or POSIXct given for x_axis_labels time information can be plotted
    if (any(class(x_axis_labels) == "Date")) {
      time <- x_axis_labels
      width_na_bar <- as.numeric(time[2] - time[1])*0.9
    }
    else if  (any(class(x_axis_labels) == "POSIXct"))
    {
      time <- x_axis_labels
      width_na_bar <- as.numeric(difftime(time[2],time[1], units = "secs"))*0.9
    }
    else if (is.null(x_axis_labels)) {
      time <- seq_along(data)
      width_na_bar <- as.numeric(time[2] - time[1])*0.9
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
        data = df, na.rm = T,
        ggplot2::aes(x = time, y = value), shape = shape_points,
        col = color_points, size = size_points
      ) +
    
    # Adding additional modifications like title, subtitle, theme,...
      ggplot2::ggtitle(label = title, subtitle = subtitle) +
      xlab(xlab) +
      ylab(ylab) +
      theme

    # Add the red background bars for missing data areas

    if (length(id_na) > 0) {
      # Red Bars only if missing data in time series
      na_val <- max(df$value, na.rm = T)
      gg <- gg +
        ggplot2::geom_bar(
          data = df[is.na(df$value), ], stat = "identity",
          ggplot2::aes(x = time, y = na_val),
          col = color_missing_border, fill = color_missing, alpha = alpha_missing, width = width_na_bar
        )
    }

    ##
    ##  End creating the ggplot2 plot
    ##


    return(gg)
  
}
