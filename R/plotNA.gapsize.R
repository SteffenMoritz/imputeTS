#' @title Visualize Distribution of NA gapsizes
#' 
#' @description Visualize Distribution of NA gapsizes(NAs in a row) in a time series
#' 
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object containing NAs
#' 
#' @param limit Specifies how many of the top gapsizes are shown in the plot.
#' 
#' @param byTotalNA For byTotalNA = TRUE the top gapsizes according to their overall weight are shown. (occurence * gapsize)
#' For byTotalNA = FALSE the top gapsizes are shown by their number of occurence. (occurence)
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
#' @details This plotting function can be used to visualize the length of the NA gaps(NAs in a row)
#'  in a time series. It shows a ranking of which gapsizes occur most often. This ranking can be ordered by total NAs for this gapsize (occurence * gap length) or by occurence of the gapsize.
#'  The outcome will be somethink like in the time series 2NAs in a row occured 27times, 4NAs in a row occured 11 times, 
#'  7NAs in a row occured 5 times, 1NA in a row occured 3 times,... .
#' 
#' @seealso \code{\link[imputeTS]{plotNA.distribution}},\code{\link[imputeTS]{plotNA.distributionBar}},
#'   \code{\link[imputeTS]{plotNA.imputations}}
#' 
#' @examples
#' #Prerequisite: Load a time series with missing values
#' x <-tsNH4
#' 
#' #Example 1: Visualize the top gapsizes
#' plotNA.gapsize(x)
#' 
#' @importFrom graphics lines par plot points barplot
#' @export plotNA.gapsize

plotNA.gapsize <- function(x, limit = 10, byTotalNA = F , legend = T, col = c('indianred','steelblue'), xlab="Ranking of the different gapsizes", ylab="Number",main ="Occurance of  gapsizes (NAs in a row)",cex.names = 0.7, horiz = F ,  axes =T,beside = T,las = 1, ... ) {
  
  data <- x
  
  #Check for wrong input 
  data <- precheck(data)
  
  id.na <- which(is.na(data))
  
  #save par settings and reset after function
  par.default <- par(no.readonly=TRUE) 
  on.exit(par(par.default))
  

  ## Calculation consecutive NA information (results is stored in vec)
  vec <- rep(0, length(data))
  run <- 0
  for (i in 0:(length(data)-1)) {
    if(is.na(data[i+1])) {
      run <- run + 1
      if(i == (length(data)-1)) {
        vec[run] <- vec[run] + 1}
       }
    else {
      vec[run] <- vec[run] + 1
      run <- 0
    }
  }
  bars1 <- bars2 <- labels1 <- labels2 <- NULL
  for (i in 1:length(vec)) {
    if(vec[i] > 0) {
      bars1 <-c(bars1, vec[i])
      bars2 <- c(bars2, vec[i]*i )
      labels1 <- c(labels1,paste0(i," NAs"))
      labels2 <- c(labels2,paste(""))
      }
  }
  
  ## Sort either by NA 
  if ( byTotalNA == T) {
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
  
  if (legend == T) { par(oma =c(0.5,0,0,0)) }
  
  ##here comes the plot itself
  barplot(inp, names.arg = labels,main = main, las = las, horiz = horiz , axes = axes  ,beside = beside, col =col ,cex.names= cex.names,xlab =xlab,ylab=ylab, ...)
  
  if (legend == T) {
     par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
     plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
     legend("bottom",  bty ='n',xjust =0.5, horiz = T , cex=1, legend = c(  "Num occurence gapsize", "Total NAs for gapsize"), col = c("indianred", "steelblue"), pch = c(20))
  }
 
}
