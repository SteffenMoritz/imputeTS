
#' @title Print Statistics about Missing Values
#'
#' @description Print summary stats about the distribution of
#' missing values in a univariate time series.
#'
#' @param x Numeric Vector (\code{\link{vector}}) or
#' Time Series (\code{\link{ts}}) object containing NAs
#'
#' @param bins Split number for bin stats. Number of bins the time series gets
#' divided into. For each bin information about amount/percentage of missing
#' values is printed. Default value is 4 - what means stats about the
#' 1st,2nd,3rd,4th quarter of the time series are shown.
#'
#' @param print_only Choose if the function Prints or Returns.
#' For print_only = TRUE the function has no return value and just prints out
#' missing value stats. If print_only is changed to FALSE, nothing is printed
#' and the function returns a list.Print gives a little bit more information,
#' since the returned list does not include "Stats for Bins"
#' and "overview NA series"
#'
#' @return A \code{\link{list}} containing the stats. Beware: Function gives
#' only a return value if print_only = FALSE.
#'
#' @details Prints the following information about the missing values in the time series:
#' \itemize{
#'    \item{"Length of time series" - Number of observations in the time series (including NAs)}
#'    \item{"Number of Missing Values" - Number of missing values in the time series}
#'    \item{"Percentage of Missing Values" - Percentage of missing values in the time series}
#'    \item{"Number of Gaps" - Number of NA gaps (consisting of one or more consecutive NAs) in the time series}
#'    \item{"Average Gap Size" - Average size of consecutive NAs for the NA gaps in the time series}
#'    \item{"Stats for Bins" - Number/percentage of missing values for the split into bins }
#'    \item{"Longest NA gap" - Longest series of consecutive missing values (NAs in a row) in the time series }
#'    \item{"Most frequent gap size" - Most frequent occurring series of missing values in the time series}
#'    \item{"Gap size accounting for most NAs" - The series of consecutive missing values that accounts for most missing values overall in the time series}
#'    \item{"Overview NA series" - Overview about how often each series of consecutive missing values occurs. Series occurring 0 times are skipped}
#'    }
#'    It is furthermore, important to note, that you are able to choose whether
#'    the function returns a list or prints the information only.
#'    (see description of parameter "print_only")
#'
#' @author Steffen Moritz
#' @seealso \code{\link[imputeTS]{ggplot_na_distribution}},
#' \code{\link[imputeTS]{ggplot_na_intervals}}, 
#' \code{\link[imputeTS]{ggplot_na_gapsize}}
#'
#' @examples
#' # Example 1: Print stats about the missing data in tsNH4
#' statsNA(tsNH4)
#'
#' # Example 2: Return list with stats about the missing data in tsAirgap
#' statsNA(tsAirgap, print_only = FALSE)
#'
#' # Example 3: Same as example 1, just written with pipe operator
#' tsNH4 %>% statsNA()
#' @importFrom magrittr %>%
#' @export


statsNA <- function(x, bins = 4, print_only = TRUE) {
  data <- x

  ##
  ## Input check
  ##

  if (!is.null(dim(data)) && dim(data)[2] != 1) {
    stop("Input x is not univariate")
  }

  if (!is.numeric(data)) {
    stop("Input x is not numeric")
  }


  ##
  ## Analysis Code
  ##

  missindx <- is.na(data)

  ## Count NAs
  number_NAs <- length(data[missindx])

  ## Calculate Percentage
  pct_NAs <- number_NAs / length(data) * 100



  ## NA in Bins // bins_df is result data frame

  # Create DF to store infomation for each bin
  bins_df <- data.frame(
    start = numeric(bins), end = numeric(bins), num = numeric(bins),
    num_NA = numeric(bins), num_nonNA = numeric(bins), pct_NA = numeric(bins)
  )


  length_bin <- ceiling(length(data) / bins)

  temp <- 0
  for (i in 1:bins) {
    bins_df$start[i] <- temp + 1
    temp <- temp + length_bin
    bins_df$end[i] <- temp
  }
  bins_df$end[bins] <- length(data)


  for (i in 1:bins) {
    bins_df$num[i] <- bins_df$end[i] - bins_df$start[i] + 1
    temp_data <- data[bins_df$start[i]:bins_df$end[i]]
    bins_df$num_NA[i] <- length(temp_data[is.na(temp_data)])
    bins_df$num_nonNA[i] <- bins_df$num[i] - bins_df$num_NA[i]
    bins_df$pct_NA[i] <- bins_df$num_NA[i] / bins_df$num[i] * 100
  }



  ## Consecutive NAs // vec is result vector

  vec <- rep(0, length(data))
  run <- 0
  for (i in 0:(length(data) - 1)) {
    if (is.na(data[i + 1])) {
      run <- run + 1
      if (i == (length(data) - 1)) {
        vec[run] <- vec[run] + 1
      }
    }
    else {
      vec[run] <- vec[run] + 1
      run <- 0
    }
  }

  # Most Common Consecutive NA
  max <- 0
  indx <- 0
  for (i in 1:length(vec)) {
    if (vec[i] >= max) {
      max <- vec[i]
      indx <- i
    }
  }
  common_NA <- indx
  common_NAnum <- max

  # Longest Consecutive NA
  longest_NA <- NA
  for (i in length(vec):1) {
    if (vec[i] > 0) {
      longest_NA <- i
      break
    }
  }

  # Biggest Weight Consecutive NA
  max <- 0
  indx <- 0
  for (i in 1:length(vec)) {
    if (vec[i] * i >= max) {
      max <- vec[i] * i
      indx <- i
    }
  }
  wcommon_NA <- indx
  wcommon_NAnum <- max


  # Average NA gapsize

  number_gaps <- sum(vec)
  if (number_NAs > 0) {
    average_gapsize <- number_NAs / number_gaps
  }
  else {
    average_gapsize <- 0
  }



  #### Print everything
  if (print_only == TRUE) {

    ## Print Number NA and Pct NA
    print("Length of time series:")
    print(length(data))
    print("-------------------------")

    print("Number of Missing Values:")
    print(number_NAs)
    print("-------------------------")

    print("Percentage of Missing Values:")
    print(paste0(format(pct_NAs, digits = 3), "%"))
    print("-------------------------")

    print("Number of Gaps:")
    print(number_gaps)
    print("-------------------------")

    print("Average Gap Size:")
    print(average_gapsize)
    print("-------------------------")

    # Exit if no NAs
    if (number_NAs < 1) {
      print("No NAs in the time series.")
      print("-------------------------")
      return("There are no NAs in the time series")
    }

    ## Print bin stats
    print("Stats for Bins")
    for (i in 1:length(bins_df$num)) {
      print(paste0(
        "  Bin ", i, " (", bins_df$num[i], " values from ", bins_df$start[i], " to ",
        bins_df$end[i], ") :      ", bins_df$num_NA[i],
        " NAs (", format(bins_df$pct_NA[i], digits = 3), "%)"
      ))
    }
    print("-------------------------")

    ## Print Consecutive NAs
    print("Longest NA gap (series of consecutive NAs)")
    print(paste(longest_NA, "in a row"))
    print("-------------------------")
    print("Most frequent gap size (series of consecutive NA series)")
    print(paste0(common_NA, " NA in a row (occuring ", common_NAnum, " times)"))
    print("-------------------------")
    print("Gap size accounting for most NAs")
    print(paste0(wcommon_NA, " NA in a row (occuring ", wcommon_NAnum / wcommon_NA, 
                 " times, making up for overall ", wcommon_NAnum, " NAs)"))
    print("-------------------------")

    print("Overview NA series")

    for (i in 1:length(vec)) {
      if (vec[i] > 0) {
        print(paste0("  ", i, " NA in a row: ", vec[i], " times"))
      }
    }
  }

  if (print_only == FALSE) {
    output <- list(
      length_series = length(data),
      number_NAs = number_NAs,
      number_na_gaps = number_gaps,
      average_size_na_gaps = average_gapsize,
      percentage_NAs = paste0(format(pct_NAs, digits = 3), "%"),
      longest_na_gap = longest_NA,
      most_frequent_na_gap = common_NA,
      most_weighty_na_gap = wcommon_NA,
      df_distribution_na_gaps = vec
    )

    return(output)
  }
}
