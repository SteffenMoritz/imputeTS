% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na_interpolation.R
\name{na_interpolation}
\alias{na_interpolation}
\title{Missing Value Imputation by Interpolation}
\usage{
na_interpolation(x, option = "linear", maxgap = Inf, ...)
}
\arguments{
\item{x}{Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
object in which missing values shall be replaced}

\item{option}{Algorithm to be used. Accepts the following input:
\itemize{
\item{"linear" - for linear interpolation using \link{approx} } (default choice)
\item{"spline" - for spline interpolation using \link{spline}}
\item{"stine" - for Stineman interpolation using \link[stinepack]{stinterp}}
}}

\item{maxgap}{Maximum number of successive NAs to still perform imputation on.
Default setting is to replace all NAs without restrictions. With this
option set, consecutive NAs runs, that are longer than 'maxgap' will
be left NA. This option mostly makes sense if you want to
treat long runs of NA afterwards separately.}

\item{...}{Additional parameters to be passed through to \link{approx} or
\link{spline} interpolation functions}
}
\value{
Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}})
object (dependent on given input at parameter x)
}
\description{
Uses either linear, spline or stineman interpolation
to replace missing values.
}
\details{
Missing values get replaced by values of \link{approx}, \link{spline}
or \link[stinepack]{stinterp} interpolation.

The na_interpolation function also supports the use of additional parameters from the respective
underlying interpolation functions. While usually not really needed, it is useful to know that
this advanced use is in principle possible. These additional parameters are not specified explicitly
in the na_interpolation function documentation. Take a look into the documentation of the \link[stinepack]{stinterp}, \link{approx} and \link{spline} functions to get an overview about these additional parameters.

An example for such a parameter is the 'method' argument of spline, which can be used to
further specify the type of spline to be used. Possible values are "fmm", "natural",
"periodic", "monoH.FC" and "hyman" (as can be seen in the \link{spline}
documentation). The respective function call using this additional parameter would
look like this:
\code{na_interpolation(x, option ="spline", method ="natural")}

Like in this example other additional detail parameters (gained from \link{approx},
\link{spline}, \link[stinepack]{stinterp} documentation) can be used by just including
them in the na_interpolation function call. As already mentioned, these advanced possibilities
for settings parameters are only helpful for specific use cases. For regular use
the standard parameters provided directly in the na_interpolation documentation should be
more than enough.
}
\examples{
# Prerequisite: Create Time series with missing values
x <- ts(c(2, 3, 4, 5, 6, NA, 7, 8))

# Example 1: Perform linear interpolation
na_interpolation(x)

# Example 2: Perform spline interpolation
na_interpolation(x, option = "spline")

# Example 3: Perform stine interpolation
na_interpolation(x, option = "stine")

# Example 4: Perform linear interpolation, with additional parameter pass through from spline()
# Take a look at the 'Details' section of the na_interpolation documentation 
# for more information about advanced parameter pass through options
na_interpolation(x, option ="spline", method ="natural")

# Example 5: Same as example 1, just written with pipe operator
x \%>\% na_interpolation()

# Example 6: Same as example 2, just written with pipe operator
x \%>\% na_interpolation(option = "spline")
}
\references{
Johannesson, Tomas, et al. (2015). "Package stinepack".
}
\seealso{
\code{\link[imputeTS]{na_kalman}}, \code{\link[imputeTS]{na_locf}},
\code{\link[imputeTS]{na_ma}}, \code{\link[imputeTS]{na_mean}},
\code{\link[imputeTS]{na_random}}, \code{\link[imputeTS]{na_replace}},
\code{\link[imputeTS]{na_seadec}}, \code{\link[imputeTS]{na_seasplit}}
}
\author{
Steffen Moritz, Ron Hause
}
