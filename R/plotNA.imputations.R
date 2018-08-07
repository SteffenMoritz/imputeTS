#' @title Visualize Imputed Values
#' 
#' @description Visualize the imputed values in a time series. 
#' 
#' @param x.withNA Numeric Vector or Time Series (\code{\link{ts}}) object with NAs before imputation
#' @param x.withImputations Numeric Vector or Time Series (\code{\link{ts}}) object with NAs replaced by imputed values
#' @param x.withTruth Numeric Vector or Time Series (\code{\link{ts}}) object with the real values. (can be set to NULL if not known)
#' 
#' @param legend If TRUE a legend is shown at the bottom of the plot. A custom legend can be obtained by
#'  setting this parameter to FALSE and using  \code{\link[graphics]{legend}} function
#'
#' @param xlab Label for x axis of the plot
#' @param ylab Label for y axis of plot
#' 
#' @param main Main title for the plot
#' 
#' @param colWithImputations Defines the color for the imputed values.
#' @param colWithTruth Defines the color of the real values (truth) for the NA values.
#' @param colWithNA Defines the color of the non-NA observations.
#' @param colLines Defines the color of the lines connecting non-NA observations.

#' @param ylim the y limits of the plot
#' 
#' @param pch Either an integer specifying a symbol or a single character to be used as the default in plotting points.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default.
#' @param ... Additional graphical parameters that can be passed through to plot 
#' 
#' @details This plot can be used, to visualize the imputed values for a time series. Therefore, 
#' the imputed values (filled NA gaps) are shown in a different color than the other values.
#' If the real values (truth) behind the NA gaps are known these are also added in a different color.
#' 
#' @author Steffen Moritz
#' 
#' 
#' @seealso \code{\link[imputeTS]{plotNA.distribution}},\code{\link[imputeTS]{plotNA.distributionBar}},
#'  \code{\link[imputeTS]{plotNA.gapsize}}
#' 
#' @examples
#' #Example 1: Visualize the values that were imputed by na.mean in the time series
#' impMean.Airgap <- na.mean(tsAirgap)
#' plotNA.imputations(tsAirgap, impMean.Airgap)
#' 
#' 
#' #Example 2: Visualize the values that were imputed by na.locf and the true values in the time series
#' impLOCF.Airgap <- na.locf(tsAirgap)
#' plotNA.imputations(tsAirgap, impLOCF.Airgap, tsAirgapComplete)
#' 
#' #Example 3: Same as example 1, just written with pipe operator
#' tsAirgap %>% na.mean %>% plotNA.imputations(x.withNA = tsAirgap)
#' 
#' @importFrom magrittr %>%
#' @importFrom graphics legend lines plot points par
#' @export plotNA.imputations

plotNA.imputations <- function(x.withNA, x.withImputations, x.withTruth = NULL,
                                legend = TRUE, main ="Visualization Imputed Values", 
                                xlab="Time",ylab="Value",
                                colWithTruth ="green3", colLines ="black",
                                colWithImputations = "indianred2",
                                colWithNA ="steelblue2",
                                ylim = c(min(c(x.withImputations,x.withTruth),na.rm = TRUE),
                                         max(c(x.withImputations,x.withTruth),na.rm = TRUE)),
                                pch = 20, cex=0.8, ...) {
  
  data.withNA <- x.withNA
  data.withImputations <-  x.withImputations
  data.withTruth <- x.withTruth
  
  
  ##
  ## Input check
  ## 
  
  if(!is.null(dim(data.withNA)) && dim(data.withNA)[2] != 1)
  {stop("Input data.withNA is not univariate")}
  
  if(!is.numeric(data.withNA))
  {stop("Input data.withNA is not numeric")}
  
  if(!is.null(dim(data.withImputations)) && dim(data.withImputations)[2] != 1)
  {stop("Input data.withImputations is not univariate")}
  
  if(!is.numeric(data.withImputations))
  {stop("Input data.withImputations is not numeric")}
  
  
  ##
  ## Plotting Code
  ## 
  
  id.na <- which(is.na(data.withNA))
  
  #save par settings and reset after function
  par.default <- graphics::par(no.readonly=TRUE) 
  on.exit(graphics::par(par.default))
  
  if (legend == TRUE) { graphics::par(oma =c(0.5,0,0,0)) }
  
  
  #real time series (data.withTruth) not available
  if (is.null(data.withTruth)) {
    graphics::plot(data.withImputations,type = "l", ylim=ylim, col = colWithImputations, ylab = ylab, xlab = xlab,main =main,  ...)
    graphics::points(data.withImputations,col = colWithImputations,pch = pch, cex=cex )
    graphics::lines(data.withNA, col =colLines)
    graphics::points(data.withNA, col =colWithNA , pch = pch, cex=cex)
    
    if (legend == TRUE) {
      graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      graphics::legend("bottom",  bty ='n',xjust =0.5, horiz = TRUE , cex=1,legend = c("imputed values", "known values"), col = c("indianred2","steelblue"), pch = c(20))
    }
  }
  else {
    #check also if data.withTruth has right format
    if(!is.null(dim(data.withTruth)) && dim(data.withTruth)[2] != 1)
    {stop("Input x.withTruth is not univariate")}
    
    if(!is.numeric(data.withTruth))
    {stop("Input x.withTruth is not numeric")}
    
    graphics::plot(data.withTruth, type = "l", ylim=ylim, col = colWithTruth, ylab=ylab,xlab=xlab,main=main, ...)
    graphics::points(data.withTruth, col = colWithTruth,pch = pch, cex=cex )
    graphics::lines(data.withNA, col =colLines)
    graphics::points( data.withImputations, col = colWithImputations, pch = pch, cex=cex)
    graphics::points(data.withNA, col =colWithNA , pch = pch, cex=cex)
    
    if (legend == TRUE) {
      graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      graphics::legend("bottom",  bty ='n',xjust =0.5, horiz = TRUE , cex=1, legend = c("imputed values", "real values", "known values"), col = c("indianred2","green","steelblue"), pch = c(20))
    }
  }
}
