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
#' #Example 1: Visualize the missing values in tsNH4 time series
#' plotNA_distributionBar(tsNH4)
#' 
#' #Example 2: Visualize the missing values in tsHeating time series
#' plotNA_distributionBar(tsHeating, breaks = 20)
#' 
#' #Example 3: Same as example 1, just written with pipe operator
#' tsNH4 %>% plotNA_distributionBar
#' 
#' @importFrom grDevices nclass.Sturges
#' @importFrom ggplot2 ggplot geom_bar aes position_fill 
#' scale_fill_manual scale_x_discrete scale_y_continuous
#' theme element_text element_blank xlab ylab ggtitle theme_minimal
#' @importFrom magrittr %>%
#' @export
plotNA_distributionBar <- function(x, breaks = grDevices::nclass.Sturges(x), 
                                   breaksize = NULL, percentage = TRUE, legend = TRUE,
                                   axes =TRUE, space =0, 
                                   col=c('indianred2','green2'), 
                                   main = "Distribution of NAs", 
                                   xlab ="Time Lapse", ylab=NULL,
                                   colborder = "black", xangle = 0,
                                   theme = theme_minimal(), ... ) {
  
  data <- x
  
  ##
  ## Input check
  ## 
  if (!is.null(dim(data)) && dim(data)[2] != 1) {stop("Input x is not univariate")}
  if (!is.numeric(data)) {stop("Input x is not numeric")}
  
  # Change zoo, xts, timeSeries objects to vector to avoid errors
  if (is.ts(data)) {data <- as.vector(data)}
  
  
  ##
  ## Plotting Code
  ## 
  
  len_data <- length(data)
  
  #Calculate the breaksize from the demanded breaks
  if (is.null(breaksize)) breaksize <- ceiling(len_data / breaks)
  breakpoints <- unique(c(seq(1, len_data, breaksize), len_data))
  
  #Define the width of the last bin in order to make it smaller if it contains less values
  widthLast <- (breakpoints[length(breakpoints)] - breakpoints[length(breakpoints) - 1]) / 
    (breakpoints[2] - breakpoints[1])
  
  #calculate NA/non-NA ratio inside of every bin
  naAmount <- okAmount <- numeric(0)
  for (i in 1:(length(breakpoints) - 1)) {
    cut <- data[(breakpoints[i]+1):(breakpoints[i+1])]
    
    nas <- length(which(is.na(cut)))
    naAmount <- c(naAmount, nas)
    
    oks <- length(cut) - nas
    okAmount <- c(okAmount, oks)
  }
  
  #calculate percentages if wanted 
  if (percentage) {
    sums = okAmount + naAmount
    naAmount <- naAmount / sums
    okAmount <- okAmount / sums
    ylab1 <- "Percentage"
  } else {
    ylab1 <- "Number"
  }
  
  #check if ylab is pre set
  if (is.null(ylab)) ylab <- ylab1
  
  #create data to be plotted
  len_amo <- length(naAmount) 
  df <- data.frame(
    "bin" = 1:len_amo,
    "naAmount" = c(naAmount, okAmount),
    "value" = c(rep("NA", len_amo), 
                rep("Not_NA", len_amo))
  )
  

  #make plot
  # breakpoints = paste0(round((breakpoints[-1] / len_data) * 100,
  breakpoints = paste0(round((breakpoints / len_data) * 100,
                             digits = 0), "%")
  
  
  gg <- ggplot(df) +
    geom_bar(aes(fill = value, y = naAmount, x = bin),
             position = position_stack(reverse = TRUE),
             stat = "identity", color = colborder,
             width = c(rep(1,len_amo - 1), widthLast) / (space + 1)
             , ...
             ) +
    scale_fill_manual(values = col, labels = c("NAs", "non-NAs")) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(breaks = seq(0, len_amo),
                     labels = breakpoints,
                     limits = as.character(seq(0, len_amo))) + 
    theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = xangle, hjust = 1.5),
          plot.title = element_text(hjust = 0.5)) +
    xlab(xlab) + ylab(ylab) +
    ggtitle(main);
  
  #hide axis
  if (!axes) {
    gg <- gg + 
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
  }
  
  #add legend
  if (legend) {
    gg <- gg +
      theme(legend.title = element_blank(), 
            legend.position = "bottom")
  }
  
  suppressWarnings(print(gg))
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
                                axes =TRUE, space =0, 
                                col=c('indianred2','green2'), 
                                main = "Distribution of NAs", 
                                xlab ="Time Lapse", ylab=NULL,
                                colborder = "black", xangle = 0,
                                theme = theme_minimal(), ... ) {
  .Deprecated(
    new = "plotNA_distributionBar",
    msg = "plotNA.distributionBar will be replaced by plotNA_distributionBar
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  plotNA_distributionBar(x, breaks, breaksize, percentage, legend,
                         axes, space, col, main, xlab, ylab, colborder, xangle,
                         theme, ...)
}

