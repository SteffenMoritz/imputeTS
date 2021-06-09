#' @title Violin Plot of Value Distribution directly before/after NAs
#'
#' @description Visualize the distribution of values directly before/after NAs via violin plots.
#' Useful to determine if missing values appear more often when a certain
#' threshold level is reached.
#'
#' @param x Numeric Vector (\code{\link[base]{vector}}) or Time Series
#' (\code{\link[stats]{ts}}) object containing NAs.
#' This is the only mandatory parameter - all other parameters are
#' only needed for adjusting the plot appearance.
#'
#' @param inside_information Defines what is inside the violin as an additional
#' distribution visualization. Accepts the following input:
#'
#' \itemize{
#'    \item{"boxplot" - Adds a boxplot inside the violins } (default choice)
#'    \item{"points" - Adds jittered points inside the violins}
#'    \item{"none" - Adds nothing inside the violins}
#'    }
#' Beware, though using jitter option "points" can lead to overlays for larger time series.
#'
#' @param color_before Color to fill the violin representing observations
#' directly before NA gaps.
#'
#' @param color_after Color to fill the violin representing
#' observations directly after NA gaps.
#'
#' @param color_source Color to fill the violin representing the
#' distribution of all non-NA values of a time series.
#'
#' @param color_inside Color used for the inside information
#' (color of boxplot border or color of points).
#'
#' @param alpha_violin Transparency value used for the violin.
#'
#' @param alpha_inside Transparency value used for the inside
#' information in the violin (boxplot, points).
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
#' @param orientation Can be either 'vertical' or 'horizontal'. Defines
#' if the violin plot is oriented vertically or horizontally.
#'
#' @param label_before Defines the label assigned to the
#' violin for values directly before NAs.
#'
#' @param label_after Defines the label assigned to the
#' violin for values directly after NAs.
#'
#' @param label_source Defines the label assigned to the violin
#' for the distribution of all values.
#' 
#' '@param add_n_label Whether to automatically additionally add a 
#' n-value (e.g. n = 100) to the labels as an indication
#' how many observations are represented by the violins.
#'
#' @param theme Set a Theme for ggplot2. Default is ggplot2::theme_linedraw().
#' (\code{\link[ggplot2]{theme_linedraw})}
#'
#' @details This function visualizes the distribution of missing
#' values directly before/after NAs via violin plots. This is useful to
#' determine if missing values appear more often when near to a
#' certain value level.
#' 
#' As described in \link[ggplot2]{geom_violin}: 'A violin plot is a compact
#' display of a continuous distribution. A violin plot is a mirrored density
#' plot displayed in the same way as a boxplot.'
#'
#' The visualization of the before/after NA distributions in comparison
#' to the overall distribution can provide information about the root
#' cause of the missing values. It also can provide indications, about 
#' the missing data mechanism (MCAR,MAR, MNAR).
#'
#' The default plot consists of three violins/boplots combinations -
#' one for all values directly before NAs, one for all values directly
#' after NAs and one for the overall distribution of all non-NA values.
#'
#' By looking at these plots it can be seen whether the NAs appear
#' rather randomly after some values in the overall distribution or
#' if e.g. it can be said NAs more likely appear after high values.
#'
#' It could, for example be the case, that a sensor can't measure
#' values above 100 degree and always outputs NA values once the
#' temperature reaches 100 degree. With these plots it could be realized,
#' that NAs in the next value always occur when the temperature is close
#' to 100 degree.
#'
#' Some more technical implementation details:
#'
#' The middle violin with the distribution of all non-NA observations also
#' includes the values directly before/after the NAs.
#'
#' Only the values directly before and after the NA gap are used for
#' the before/after violins.
#'
#' For the example series 6, 2, NA, NA, NA, 3, 6 this would mean:
#' \itemize{
#'    \item{The 2 value goes into the before distribution}
#'    \item{The 3 value goes into the after distribution}
#'    \item{Both 6 are not in before or after, since only values directly
#'  before or after the gaps are considered}
#'    \item{No extra values added to before/after as representatives for the middle NAs}
#'    }
#' So the source/overall distribution for this series would be \{6, 2, 3, 6\}
#' the before \{2\} and after \{6\}.
#'
#' Of course the overall plot only makes sense with a longer time series
#' with more missing values.
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
#' Additionally for more complex adjustments, the output can also be
#' adjusted via ggplot2 syntax. This is possible, since the output
#' of the function is a ggplot2 object. Also take a look at the Examples
#' to see how adjustments are made.
#'
#' @author Steffen Moritz
#'
#' @seealso \code{\link[imputeTS]{ggplot_na_intervals}},
#' \code{\link[imputeTS]{ggplot_na_gapsize}},
#' \code{\link[imputeTS]{ggplot_na_distribution}},
#' \code{\link[imputeTS]{ggplot_na_imputations}}
#'
#' @examples
#' # Example 1: Visualize the before/after NA distributions
#' x <- stats::ts(c(1:11, 4:9, NA, NA, NA, 11:15, 7:15, 15:6, NA, NA, 2:5, 3:7))
#' ggplot_na_level(x)
#'
#' # Example 2: Visualize the before/after NA distributions in tsNH4 time series
#' ggplot_na_level(tsNH4)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' x <- ts(c(1:11, 4:9, NA, NA, NA, 11:15, 7:15, 15:6, NA, NA, 2:5, 3:7))
#' x %>% ggplot_na_level()
#'
#' # Example 4: Visualize the before/after NA in tsAirgap - different color for violins
#' # Plot adjustments via ggplot_na_distribution function parameters
#' ggplot_na_level(tsAirgap, color_after = "green")
#'
#' # Example 5: Visualize before/after NA in tsAirgap - different theme
#' # Plot adjustments via ggplot_na_distribution function parameters
#' ggplot_na_level(tsAirgap, theme = ggplot2::theme_classic())
#'
#' # Example 6: Visualize before/after NA in tsNH4 - title, subtitle in center
#' # Plot adjustments via ggplot2 syntax
#' ggplot_na_level(tsAirgap) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
#'   ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 7: Visualize before/after NA in tsAirgap - title in center, no subtitle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_level(tsAirgap, subtitle = NULL) +
#'   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
#'
#' # Example 8: Visualize before/after NA in tsAirgap - y-axis texts with angle
#' # Plot adjustments via ggplot2 syntax and function parameters
#' ggplot_na_level(tsAirgap, color_source = "grey") +
#'   ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 60, hjust = 1))
#' @importFrom ggplot2 theme_linedraw ggplot geom_violin geom_boxplot geom_jitter ggtitle
#' xlab ylab scale_x_discrete scale_fill_manual theme element_blank element_text theme_classic
#'
#' @importFrom magrittr %>%
#'
#' @export

ggplot_na_level <- function(x,
                            inside_information = "boxplot",
                            color_before = "pink3",
                            color_after = "pink3",
                            color_source = "steelblue",
                            color_inside = "black",
                            alpha_violin = 0.5,
                            alpha_inside = 0.9,
                            title = "Before/After Analysis",
                            subtitle = "Level of values occurring directly before and after NAs",
                            xlab = "",
                            ylab = "Value",
                            legend = FALSE,
                            orientation = "vertical",
                            label_before = "before",
                            label_after = "after",
                            label_source = "source",
                            add_n_label = T,
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



  ##
  ## End Input Check and Transformation
  ##




  ##
  ## 2. Preparations
  ##

  # 2.1 Create required data

  # Get all indices of the data that comes directly before and after an NA

  na_indx_after <- which(is.na(data[1:(length(data) - 1)])) + 1
  # starting from index 2 moves all indexes one in front, so no -1 needed for before
  na_indx_before <- which(is.na(data[2:length(data)]))

  # Get the actual values to the indices and put them in a data frame with a label
  before <- data.frame(type = "before", input = na_remove(data[na_indx_before]))
  after <- data.frame(type = "after", input = na_remove(data[na_indx_after]))
  all <- data.frame(type = "source", input = na_remove(data))

  # Get n values for the plot labels
  n_before <- length(before$input)
  n_all <- length(all$input)
  n_after <- length(after$input)


  # 2.4 Create dataframe for ggplot2

  # join the data together in one dataframe
  df <- rbind(before, after, all)

  # Workaround for 'no visible binding' check() caused by ggplot2 vars
  type <- df$type
  input <- df$input


  ##
  ## End Preparations
  ##



  ##
  ## 3. Create the ggplot2 plot
  ##

  # Create the plot

  gg <- ggplot2::ggplot(data = df, ggplot2::aes(x = type, y = input, fill = type)) +

    # add violin shapes
    ggplot2::geom_violin(width = 1, alpha = alpha_violin)


  # Select inside of violin (either boxplot, points, nothing)
  if (inside_information == "boxplot") {
    gg <- gg + ggplot2::geom_boxplot(width = 0.1, color = color_inside, alpha = alpha_inside)
  }
  else if (inside_information == "points") {
    gg <- gg + ggplot2::geom_jitter(width = 0.1, color = color_inside, alpha = alpha_inside)
  }
  else if (inside_information == "none") {
    # add nothing
  }
  else {
    stop("Wrong input for parameter inside_information Input must be either 'boxplot',
  'points' or 'none'. Call ?ggplot_na_level to view the documentation.")
  }


  # Adding additional modifications like title, subtitle, theme,...
  gg <- gg + ggplot2::ggtitle(label = title, subtitle = subtitle) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    theme +
    ggplot2::scale_x_discrete(
      limits = c("before", "source", "after"),
      labels = c(paste0(label_before, ifelse(add_n_label, paste0(" \n n = ", n_before),"")),
                 paste0(label_source, ifelse(add_n_label, paste0(" \n n = ", n_all),"")),
                 paste0(label_after, ifelse(add_n_label, paste0(" \n n = ", n_after),"")))
    ) +
    ggplot2::scale_fill_manual(values = c(color_after, color_before, color_source))


  # For flipping from vertical to horizontal bars
  if (orientation == "horizontal") {
    gg <- gg + ggplot2::coord_flip()
  }

  # Select legend
  gg <- gg + ggplot2::theme(
    legend.position = base::ifelse(legend == TRUE, "right", "none"),
    legend.title = ggplot2::element_blank()
  )

  ##
  ##  End creating the ggplot2 plot
  ##


  return(gg)
}
