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
#' @param legend If TRUE a legend is shown at the bottom of the plot. A custom legend can be obtained by
#'  setting this parameter to FALSE and using  \code{\link[graphics]{legend}} function
#' 
#' @param col A vector of colors for the bars or bar components.
#' @param xlab Label for x axis of the plot
#' @param ylab Label for y axis of plot
#' @param main Main title for the plot
#' 
#' @param cex.names Expansion factor for axis names (bar labels).
#' 
#' @param horiz A logical value. If FALSE, the bars are drawn vertically with the first bar to the left. If TRUE, the bars are drawn horizontally with the first at the bottom.
#' 
#' @param axes Logical. If TRUE, a vertical (or horizontal, if horiz is true) axis is drawn.
#' 
#' @param beside A logical value. If FALSE, the columns of height are portrayed as stacked bars, and if TRUE the columns are portrayed as juxtaposed bars.
#' 
#' @param las Numeric in {0,1,2,3}; the style of axis labels. 0:always parallel to the axis, 1:always horizontal, 2:always perpendicular to the axis, 3:always vertical.
#' 
#' @param ... Additional graphical parameters that can be passed through to barplot 
#' 
#' @author Steffen Moritz
#' 
#' @details This plotting function can be used to visualize the length of the NA gaps (NAs in a row)
#'  in a time series. It shows a ranking of which gap sizes occur most often. This ranking can be ordered by total NAs for this gap size (occurrence * gap length) or by occurrence of the gap size.
#'  The outcome will be somethink like in the time series 2NAs in a row occurred  27times, 4NAs in a row occurred  11 times, 
#'  7NAs in a row occurred 5 times, 1NA in a row occurred 3 times, ... .
#' 
#' @seealso \code{\link[imputeTS]{plotNA.distribution}},\code{\link[imputeTS]{plotNA.distributionBar}},
#'   \code{\link[imputeTS]{plotNA.imputations}}
#' 
#' @examples
#' #Example 1: Visualize the top gap sizes in tsNH4
#' plotNA.gapsize(tsNH4)
#' 
#' #Example 2: Visualize the top gap sizes in tsAirgap
#' plotNA.gapsize(tsAirgap)
#' 
#' #Example 3: Same as example 1, just written with pipe operator
#' tsNH4 %>% plotNA.gapsize
#' 
#' @importFrom magrittr %>%
#' @importFrom graphics lines par plot points barplot
#' @export plotNA.gapsize

plotNA.gapsize <- function(x, limit = 10, byTotalNA = FALSE , legend = TRUE, col = c('indianred','steelblue'), xlab="Ranking of the different gap sizes", ylab="Number",main ="Occurrence of gap sizes (NAs in a row)",cex.names = 0.7, horiz = FALSE ,  axes =TRUE,beside = TRUE,las = 1, ... ) {
  
  data <- x
  
  ##
  ## Input check
  ## 
  
  if(!is.null(dim(data)) && dim(data)[2] != 1)
  {stop("Input x is not univariate")}
  
  if(!is.numeric(data))
  {stop("Input x is not numeric")}
  
  if(!any(is.na(data)))
  {stop("No missing values in the input - nothing to plot")}
  
  ##
  ## Plotting Code
  ## 
  
  # Change zoo, xts, timeSeries objects to vector to avoid errors
  if (!is.ts(data)) 
  {data <- as.vector(data)}
  
  na_data <- is.na(data)
  id.na <- which(na_data)
  
  #save par settings and reset after function
  par.default <- graphics::par(no.readonly=TRUE) 
  on.exit(graphics::par(par.default))
  

  ## Calculation consecutive NA information (results is stored in vec)
  rle_na <- rle(na_data)
  vec <- rle_na$lengths[rle_na$values]
  bars1 <- table(vec)
  gaps_vec <- as.integer(names(bars1))
  bars2 <- bars1 * gaps_vec
  labels1 <- paste0(gaps_vec, " NAs")
  labels2 <- character(length = length(bars1))
  
  ## Sort either by NA 
  if ( byTotalNA == TRUE) {
    #sort accoding to overall NAs
    fooind <- order(bars1)  
    bars1 <- bars1[fooind]
    bars2 <- bars2[fooind]
    labels1 <- labels1[fooind]
  }
  else {
    #sort accoding to overall NAs
    fooind <- order(bars2)  
    bars1 <- bars1[fooind]
    bars2 <- bars2[fooind]
    labels1 <- labels1[fooind]
  }
  
  
  ##Adjust to show only a limited amount of bars (limit)
  if(length(bars1) > limit) {
    bars1 <- bars1[(length(bars1)-limit+1):length(bars1)]
    bars2 <- bars2[(length(bars2)-limit+1):length(bars2)]
    labels1 <- labels1[(length(labels1)-limit+1):length(labels1)]
    labels2 <- labels2[(length(labels2)-limit+1):length(labels2)]
  }

  inp <- matrix(c(bars1,bars2),byrow=TRUE,ncol=length(bars1))
  labels <-  as.vector(rbind(labels1,labels2)) 
  
  if (legend == TRUE) { graphics::par(oma =c(0.5,0,0,0)) }
  
  ##here comes the plot itself
  graphics::barplot(inp, names.arg = labels,main = main, las = las, horiz = horiz , axes = axes  ,beside = beside, col =col ,cex.names= cex.names,xlab =xlab,ylab=ylab, ...)
  
  if (legend == TRUE) {
    graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    graphics::legend("bottom",  bty ='n',xjust =0.5, horiz = TRUE , cex=1, legend = c(  "Num occurrence gapsize", "Total NAs for gapsize"), col = c("indianred", "steelblue"), pch = c(20))
  }
 
}
