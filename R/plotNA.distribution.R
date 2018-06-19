#' @title Visualize Distribution of Missing Values
#' 
#' @description Visualize the distribution of missing values within a time series.
#' 
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object containing NAs
#' 
#' @param colPoints Color of the points for each observation
#' @param colBackgroundMV Color for the background for the NA sequences
#' @param main Main label for the plot
#' @param xlab Label for x axis of the plot
#' @param ylab Label for y axis of plot
#' @param pch Plotting 'character', i.e., symbol to use.
#' @param cexPoints character (or symbol) expansion: a numerical vector.
#' @param col Color for the lines.
#' @param ... Additional graphical parameters that can be passed through to plot 
#' 
#' @details This function visualizes the distribution of missing values within a time series. Therefore, 
#' the time series is plotted and whenever a value is NA the background is colored differently.
#' This gives a nice overview, where in the time series most of the missing values occur.
#' 
#' 
#' @author Steffen Moritz
#' 
#' @seealso \code{\link[imputeTS]{plotNA.distributionBar}},
#'  \code{\link[imputeTS]{plotNA.gapsize}}, \code{\link[imputeTS]{plotNA.imputations}}
#' 
#' @examples
#' #Example 1: Visualize the missing values in x
#' x <- ts(c(1:11, 4:9,NA,NA,NA,11:15,7:15,15:6,NA,NA,2:5,3:7))
#' plotNA.distribution(x)
#' 
#' #Example 2: Visualize the missing values in tsAirgap time series
#' plotNA.distribution(tsAirgap)
#' 
#' #Example 3: Same as example 1, just written with pipe operator
#' x <- ts(c(1:11, 4:9,NA,NA,NA,11:15,7:15,15:6,NA,NA,2:5,3:7))
#' x %>% plotNA.distribution()
#' 
#' @importFrom graphics par plot points barplot
#' @export

plotNA.distribution <- function(x, colPoints = "steelblue", colBackgroundMV = "indianred2",
                                main="Distribution of NAs", xlab = "Time", ylab="Value", pch = 20, cexPoints = 0.8, col ="black", ... ) {
  
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

  id.na <- which(is.na(data))
  
  #Red Bars only if missing data in time series
  if (length(id.na > 0)) 
  {
    barplotData <- rep(NA,length(data))
    
    #make sure the end of the bar can not be seen
    barplotData[id.na] <- max(data, na.rm =TRUE )*100
    
    ##Plot the red in background for unknown values
    barplot(barplotData, col = colBackgroundMV,xaxt = "n", yaxt = "n",   xlab = "", ylab = "", border = colBackgroundMV)
    par(new=TRUE)
  }
  ## Plot the line diagram of known values
  
  plot(data, main =main, type = "l", xlab = xlab, ylab = ylab, col = col,... )
  points(data, pch= pch , cex = cexPoints, col = colPoints)
  
}




