
#' @title Print Statistics about Missing Values
#' 
#' @description Print summary stats about the distribution of missing values in a univariate time series. 
#'  
#' @param x Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object containing NAs
#'  
#' @param bins Split number for bin stats. Number of bins the time series gets divided into. 
#' For each bin information about amount/percentage of missing values is printed. 
#' Default value is 4 - what means stats about the 1st,2nd,3rd,4th quarter of the time
#' series are shown.
#' 
#' @param printOnly Choose if the function Prints or Returns. For printOnly = TRUE the function has no return value and just prints out
#' missing value stats. If printOnly is changed to FALSE, nothing is printed and the function returns a list.
#' Print gives a little bit more information, since the returned list does not include "Stats for Bins" 
#' and "overview NA series"
#' 
#' @return A \code{\link{list}} containing the stats. Beware: Function gives only a return value
#' if printOnly = FALSE.
#' 
#' @details Prints the following information about the missing values in the time series:
#' \itemize{
#'    \item{"Length of time series" - Number of observations in the time series (including NAs)}
#'    \item{"Number of Missing Values" - Number of missing values in the time series}
#'    \item{"Percentage of Missing Values" - Percentage of missing values in the time series}
#'    \item{"Stats for Bins" - Number/percentage of missing values for the split into bins }
#'    \item{"Longest NA gap" - Longest series of consecutive missing values (NAs in a row) in the time series }
#'    \item{"Most frequent gap size" - Most frequent occuring series of missing values in the time series}
#'    \item{"Gap size accounting for most NAs" - The series of consecutive missing values that accounts for most missing values overall in the time series}
#'    \item{"Overview NA series" - Overview about how often each series of consecutive missing values occurs. Series occuring 0 times are skipped}
#'    }
#'    It is furthermore, important to note, that you are able to choose wheather the function returns a list
#'    or prints the information only. (see description of parameter "printOnly")
#'    
#' @author Steffen Moritz
#' @seealso \code{\link[imputeTS]{plotNA.distribution}},\code{\link[imputeTS]{plotNA.distributionBar}},
#'  \code{\link[imputeTS]{plotNA.gapsize}}
#' 
#' @examples
#' #Prerequisite: Load a time series with missing values
#' x <- tsNH4
#' 
#' #Example 1: Print stats about the missing data
#' statsNA(x)
#' 
#' @export


statsNA <- function(x, bins = 4, printOnly = T) {
  
  data <- x
  
  #Check for wrong input 
  data <- precheck(data)
  
  
  missindx <- is.na(data)  
  
  ## Count NAs
  numberNAs <- length(data[missindx])
  
  ## Calculate Percentage
  pctNAs <- numberNAs / length(data)*100
  
  
  
  ## NA in Bins // BinsF is result data frame
  
  #Create DF to store infomation for each bin
    binsF <- data.frame(start = numeric(bins) , end=numeric(bins), num = numeric(bins),
                        numNA=numeric(bins), numNonNA=numeric(bins), pctNA=numeric(bins))
  
    
    lengthBin <- ceiling(length(data) / bins)
    
    temp <- 0
    for (i in 1:bins) {
      binsF$start[i] <- temp +1
      temp <- temp + lengthBin
      binsF$end[i] <- temp
    }
    binsF$end[bins] <- length(data)
    
    
    for (i in 1:bins) {
      binsF$num[i] <- binsF$end[i] - binsF$start[i]+1
      tempData <- data[binsF$start[i]:binsF$end[i]]
      binsF$numNA[i] <- length(tempData[is.na(tempData)])
      binsF$numNonNA[i] <- binsF$num[i] - binsF$numNA[i]
      binsF$pctNA[i] <- binsF$numNA[i] / binsF$num[i] * 100
    }
    
      

  ## Consecutive NAs // vec is result vector

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
    
    #Most Common Consecutive NA
    max <- 0
    indx <- 0
    for (i in 1:length(vec)) { 
        if (vec[i] >= max) {
          max <- vec[i]
          indx <- i
        }
    }
    commonNA <- indx
    commonNAnum <- max
    
    #Longest Consecutive NA
    longestNA <- NA
    for (i in length(vec):1) { 
        if(vec[i]>0) {
          longestNA <- i
          break
          }
    }
    
    #Biggest Weight Consecutive NA
    max <- 0
    indx <- 0
    for (i in 1:length(vec)) { 
      if (vec[i]*i >= max) {
        max <- vec[i]*i
        indx <- i
      }
    }
    wcommonNA <- indx
    wcommonNAnum <- max
    
    
  
  ####Print everything
  if (printOnly == T) {
      ## Print Number NA and Pct NA
      print("Length of time series:")
      print(length(data))
      print("-------------------------")
      print("Number of Missing Values:")
      print(numberNAs)
      print("-------------------------")
      
      print("Percentage of Missing Values:")
      print(paste0(format(pctNAs, digits = 3),"%"))
      print("-------------------------")
      
      #Exit if no NAs
      if(numberNAs < 1) {
        print("No NAs in the time Series.")
        return("No NAs")
      }
      
      ##Print bin stats
      print("Stats for Bins")
      for (i in 1:length(binsF$num)) {
        print(paste0("  Bin " , i , " (" , binsF$num[i] , " values from ", binsF$start[i] , " to " ,
                     binsF$end[i] ,") :      ",binsF$numNA[i] ,
                     " NAs (" , format(binsF$pctNA[i], digits = 3) , "%)" ))
      }
      print("-------------------------")
      
      ##Print Consecutive NAs
      print("Longest NA gap (series of consecutive NAs)")
      print(paste(longestNA, "in a row"))
      print("-------------------------")
      print("Most frequent gap size (series of consecutive NA series)")
      print(paste0( commonNA, " NA in a row (occuring ",commonNAnum," times)"))
      print("-------------------------")
      print("Gap size accounting for most NAs")
      print(paste0( wcommonNA, " NA in a row (occuring ", wcommonNAnum/wcommonNA, " times, making up for overall ",wcommonNAnum," NAs)"))
      print("-------------------------")
      
      print("Overview NA series")
      
      for (i in 1:length(vec)) {
        if(vec[i] > 0) {print(paste0("  ",i," NA in a row: ", vec[i], " times"))}
      }
  }
  
  if (printOnly == F) {
      output <- list(
             lengthTimeSeries = length(data),
             numberNAs = numberNAs,
             percentageNAs = paste0(format(pctNAs, digits = 3),"%"),
             naGapLongest = longestNA,
             naGapMostFrequent = commonNA,
             naGapMostOverallNAs = wcommonNA
         )
      
      return(output)
  }
}