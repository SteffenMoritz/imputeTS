% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na_replace.R
\name{na_replace}
\alias{na_replace}
\title{Replace Missing Values by a Defined Value}
\usage{
na_replace(x, fill = 0, maxgap = Inf)
}
\arguments{
\item{x}{Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
object in which missing values shall be replaced}

\item{fill}{Value used to replace the missing values}

\item{maxgap}{Maximum number of successive NAs to still perform imputation on.
Default setting is to replace all NAs without restrictions. With this
option set, consecutive NAs runs, that are longer than 'maxgap' will
be left NA. This option mostly makes sense if you want to
treat long runs of NA afterwards separately.}
}
\value{
Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
object (dependent on given input at parameter x)
}
\description{
Replaces all missing values with a given value.
}
\examples{
# Prerequisite: Create Time series with missing values
x <- ts(c(2, 3, NA, 5, 6, NA, 7, 8))

# Example 1: Replace all NAs with 3.5
na_replace(x, fill = 3.5)

# Example 2: Replace all NAs with 0
na_replace(x, fill = 0)

# Example 3: Same as example 1, just written with pipe operator
x \%>\% na_replace(fill = 3.5)
}
\seealso{
\code{\link[imputeTS]{na_interpolation}},
\code{\link[imputeTS]{na_kalman}}, \code{\link[imputeTS]{na_locf}},
\code{\link[imputeTS]{na_ma}}, \code{\link[imputeTS]{na_mean}},
\code{\link[imputeTS]{na_random}},
\code{\link[imputeTS]{na_seadec}}, \code{\link[imputeTS]{na_seasplit}}
}
\author{
Steffen Moritz
}
