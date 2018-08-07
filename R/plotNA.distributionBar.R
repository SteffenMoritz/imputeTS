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
#' @param legend If TRUE a legend is shown at the bottom of the plot. A custom legend can be obtained by
#'  setting this parameter to FALSE and using  \code{\link[graphics]{legend}} function
#' 
#' @param axis If TRUE a x-axis with labels is added. A custom axis can be obtained by
#'  setting this parameter to FALSE and using  \code{\link[graphics]{axis}} function
#' @param space The amount of space (as a fraction of the average bar width) left before each bar.
#' @param col A vector of colors for the bars or bar components.
#' @param main Main title for the plot
#' @param xlab Label for x axis of the plot
#' @param ylab Label for y axis of plot
#' @param ... Additional graphical parameters that can be passed through to barplot 
#' 
#' @details This function visualizes the distribution of missing values within a time series.
#' In comparison to the \code{\link[imputeTS]{plotNA.distribution}} function this is not done by plotting
#' each observation of the time series separately Instead observations for time intervals are represented as bars.
#' For these intervals information about the amount of missing values are shown. This has the advantage, that also
#' for large time series a plot which is easy to overview can be created.
#'
#' @author Steffen Moritz
#' 
#' @seealso \code{\link[imputeTS]{plotNA.distribution}},
#'  \code{\link[imputeTS]{plotNA.gapsize}}, \code{\link[imputeTS]{plotNA.imputations}}
#' 
#' @examples
#' #Example 1: Visualize the missing values in tsNH4 time series
#' plotNA.distributionBar(tsNH4)
#' 
#' #Example 2: Visualize the missing values in tsHeating time series
#' plotNA.distributionBar(tsHeating, breaks = 20)
#' 
#' #Example 3: Same as example 1, just written with pipe operator
#' tsNH4 %>% plotNA.distributionBar
#' 
#' @importFrom  grDevices nclass.Sturges
#' @importFrom graphics legend barplot axis par plot
#' @importFrom magrittr %>%
#' @export plotNA.distributionBar

plotNA.distributionBar <- function(x, 
                                    breaks = grDevices::nclass.Sturges(x), breaksize = NULL, percentage = TRUE, legend = TRUE,
                                    axis =TRUE, space =0, col=c('indianred2','green2'), main = "Distribution of NAs", xlab ="Time Lapse", ylab=NULL ,  ... ) {
  
  data <- x
  
  ##
  ## Input check
  ## 
  if(!is.null(dim(data)) && dim(data)[2] != 1)
  {stop("Input x is not univariate")}
  
  if(!is.numeric(data))
  {stop("Input x is not numeric")}
  
  
  ##
  ## Plotting Code
  ## 
  
  #save par settings and reset after function
  par.default <- graphics::par(no.readonly=TRUE) 
  on.exit(graphics::par(par.default))
  
  
  
  #Calculate the breakssize from the demanded breaks
  if (is.null(breaksize)) {
    breaksize <- ceiling(length(data) / breaks)
  }
  
  breakpoints <- c(1)
  bp <- 1
  while ( bp < length(data))
  {
    bp <- bp+ breaksize
    if (bp >= length(data))
    { bp <- length(data) }
    breakpoints <- c(breakpoints,bp)  
  }
  
  #Define the width of the last bin in order to make it smaller if it contains less values
  widthLast <- (breakpoints[length(breakpoints)] - breakpoints[length(breakpoints)-1]) / (breakpoints[2] - breakpoints[1])
  
  #calculate NA/non-NA ratio inside of every bin
  naAmount <- numeric(0)
  okAmount <- numeric(0)
  for (i in 1:(length(breakpoints)-1)) {
    
    cut <- data[(breakpoints[i]+1):(breakpoints[i+1])]
    
    nas <- length(which(is.na(cut)))
    naAmount <- c(naAmount,nas )
    
    oks <- length(cut) - nas
    okAmount <- c(okAmount, oks )
    
  }
  
  #calculate percentages if wanted 
  if (percentage == TRUE) {
    
    temp1 <- naAmount/(okAmount+naAmount)
    temp2 <- okAmount/(okAmount+naAmount)
    naAmount[is.infinite(naAmount)] <- 1 
    okAmount[is.infinite(okAmount)] <- 1 
    naAmount <- temp1
    okAmount <- temp2
    ylab1 <- "Percentage"
    
  } else if(percentage == FALSE) {
    ylab1 <- "Number"
  }
  
  #check if ylab is pre set
  if (is.null(ylab)) {
    ylab <- ylab1
  }
  
  #create data to be plotted
  plotData <- matrix(c(naAmount,okAmount),byrow=TRUE,ncol=length(naAmount))
  
  if (legend == TRUE) { graphics::par(oma =c(0.5,0,0,0)) }
  
  #create the barplot
  graphics::barplot(plotData,width =c(rep(1,length(naAmount)-1),widthLast) , main =main, space =space,col=col,xlab =xlab,ylab=ylab, ...)
  
  breakpoints = paste0(  (round(   (breakpoints / length(data)*100   ) , digits = 0)  ), "%")
  #add axis
  if(axis ==TRUE) {
    graphics::axis(1, at=c(seq(0,length(naAmount))), labels = breakpoints, line = 0.5, tick = TRUE)
  }
  #add legend if wanted
 
  if (legend == TRUE) {
    graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    graphics::legend("bottom",  bty ='n',xjust =0.5, horiz = TRUE , cex=1, legend = c("NAs","non-NAs"), col = c("indianred2","green2"), pch = c(15))
  }
  
  
  
}




