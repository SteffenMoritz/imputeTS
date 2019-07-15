#' @title Visualize Imputed Values
#' 
#' @description Visualize the imputed values in a time series. 
#' 
#' @param x.withNA Numeric Vector or Time Series (\code{\link{ts}}) object with NAs before imputation
#' @param x.withImputations Numeric Vector or Time Series (\code{\link{ts}}) object with NAs replaced by imputed values
#' @param x.withTruth Numeric Vector or Time Series (\code{\link{ts}}) object with the real values. (can be set to NULL if not known)
#' 
#' @param legend If TRUE a legend is added at the bottom
#'
#' @param xlab Label for x axis
#' @param ylab Label for y axis
#' 
#' @param main Main title
#' 
#' @param colWithImputations Color of imputed values
#' @param colWithTruth Color of real values (truth) for the NA values
#' @param colWithNA Color of non-NA observations
#' @param colLines Color of lines connecting non-NA observations
#' 
#' @param pch Either an integer specifying a symbol or a single character to be used as the default in plotting points.
#' @param cex A numerical value giving the size of points.
#' @param theme Set a theme for ggplot2. Default is \code{\link[ggplot2]{theme_minimal}}
#' 
#' @param ... Additional graphical parameters can be passed to \code{\link[ggplot2]{geom_line}} 
#' 
#' @details This plot can be used, to visualize imputed values for a time series. 
#' Imputed values (filled NA gaps) are shown in a different color than the other values.
#' If real values (truth) for the NA gaps are known, they are added in a different color.
#' 
#' @author Steffen Moritz
#' 
#' 
#' @seealso \code{\link[imputeTS]{plotNA_distribution}},\code{\link[imputeTS]{plotNA_distributionBar}},
#'  \code{\link[imputeTS]{plotNA_gapsize}}
#' 
#' @examples
#' #Example 1: Visualize the values that were imputed by na.mean in the time series
#' impMean.Airgap <- na.mean(tsAirgap)
#' plotNA_imputations(tsAirgap, impMean.Airgap)
#' 
#' 
#' #Example 2: Visualize the values that were imputed by na.locf and the true values in the time series
#' impLOCF.Airgap <- na.locf(tsAirgap)
#' plotNA_imputations(tsAirgap, impLOCF.Airgap, tsAirgapComplete)
#' 
#' #Example 3: Same as example 1, just written with pipe operator
#' tsAirgap %>% na.mean %>% plotNA_imputations(x.withNA = tsAirgap)
#' 
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot geom_line geom_point aes 
#' theme_minimal theme element_text element_blank xlab ylab ggtitle scale_color_manual
#' @export
plotNA_imputations <- function(x.withNA, x.withImputations, x.withTruth = NULL,
                                legend = TRUE, main ="Visualization Imputed Values", 
                                xlab="Time", ylab="Value",
                                colWithTruth ="green3", colLines ="black",
                                colWithImputations = "indianred2",
                                colWithNA ="steelblue2",
                                pch = 20, cex=2, theme = theme_minimal(), ...) {
  
  data.withNA <- x.withNA
  data.withImputations <-  x.withImputations
  data.withTruth <- x.withTruth
  
  
  ##
  ## Input check #################
  ## 
  
  if(!is.null(dim(data.withNA)) && dim(data.withNA)[2] != 1)
  {stop("Input data.withNA is not univariate")}
  
  if(!is.numeric(data.withNA))
  {stop("Input data.withNA is not numeric")}
  
  if(!is.null(dim(data.withImputations)) && dim(data.withImputations)[2] != 1)
  {stop("Input data.withImputations is not univariate")}
  
  if(!is.numeric(data.withImputations))
  {stop("Input data.withImputations is not numeric")}
  
  # Change zoo, xts, timeSeries objects to vector to avoid errors
  if (is.ts(data.withNA)) data.withNA <- as.vector(data.withNA)
  if (is.ts(data.withImputations)) data.withImputations <- as.vector(data.withImputations)
  if (is.ts(data.withTruth)) data.withTruth <- as.vector(data.withTruth)
  
  ##
  ## Plotting Code #################
  ## 
  
  index_Imputatios <- 1:length(data.withImputations)
  index_with_NA <- 1:length(data.withNA)
  
  #real time series (data.withTruth) not available
  if (is.null(data.withTruth)) {
    gg <- ggplot() + 
      geom_line(aes(x = index_Imputatios, y = data.withImputations, 
                    # col = colWithImputations) +
                    col = colWithImputations
                    , ...)) +
      geom_point(aes(x = index_Imputatios, y = data.withImputations),
                 pch = pch, size = cex,
                 col = colWithImputations) +
      geom_line(aes(x = index_with_NA, y = data.withNA, col = colLines)
                , ...) +
      geom_point(aes(x = index_with_NA, y = data.withNA),
                 pch = pch, size = cex,
                 col = colWithNA) +
      ylab(ylab) + xlab(xlab) +
      scale_color_manual(values = c(colWithNA, colWithImputations), 
                         labels = c("Imputed values", 
                                    "Known values")) +
      ggtitle(main) +
      theme +
      theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
    
    
    if (legend) {
      gg <- gg + 
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 30, hjust = 1),
              legend.title = element_blank())
    }
  }
  else {
    #check also if data.withTruth has right format
    if(!is.null(dim(data.withTruth)) && dim(data.withTruth)[2] != 1)
    {stop("Input x.withTruth is not univariate")}
    if(!is.numeric(data.withTruth)) 
    {stop("Input x.withTruth is not numeric")}

    index_with_Truth <- 1:length(data.withTruth)
    
    gg <- ggplot() + 
      geom_point(aes(x = index_Imputatios, y = data.withImputations,
                     col = colWithImputations),
                 pch = pch, size = cex) +
      geom_line(aes(x = index_with_Truth, y = data.withTruth),
                col = colWithTruth, ...) +
      geom_line(aes(x = index_with_NA, y = data.withNA), 
                col = "black", ...) +
      geom_point(aes(x = index_with_Truth, y = data.withTruth,
                     col = colWithTruth),
                 pch = pch, size = cex) +
      geom_point(aes(x = index_with_NA, y = data.withNA, 
                     col = colWithNA),
                 pch = pch, size = cex) +
      ylab(ylab) + xlab(xlab) +
      scale_color_manual(values = c(colWithTruth, colWithImputations, colWithNA),
                         labels = c("Real values",
                                    "Imputed values",
                                    "Known values")) +
      ggtitle(main) +
      theme +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5));
    
    
    if (legend) {
      gg <- gg +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 30, hjust = 1),
              legend.title = element_blank())
    }
  }
  suppressWarnings(print(gg))
}


#' Deprecated use \code{\link[imputeTS]{plotNA_imputations}} instead.
#' @description plotNA.imputations is replaced by \code{\link[imputeTS]{plotNA_imputations}}.
#' The functionality stays the same. The new name better fits modern R code
#' style guidelines (which prefer _ over . in function names).
#' @inheritParams plotNA_imputations
#' @keywords internal
#' @export
plotNA.imputations <- function(x.withNA, x.withImputations, x.withTruth = NULL,
                               legend = TRUE, main ="Visualization Imputed Values", 
                               xlab="Time", ylab="Value",
                               colWithTruth ="green3", colLines ="black",
                               colWithImputations = "indianred2",
                               colWithNA ="steelblue2",
                               pch = 20, cex=2, theme = theme_minimal(), ...) {
  .Deprecated(
    new = "plotNA_imputations",
    msg = "plotNA.imputations will be replaced by plotNA_imputations
    Functionality stays the same.
    The new function name better fits modern R code style guidelines.
    Please adjust your code accordingly."
  )
  plotNA_imputations(x.withNA, x.withImputations, x.withTruth,
                     legend, main, xlab, ylab, colWithTruth, colLines, colWithImputations,
                     colWithNA, pch, cex, theme, ...)
}

