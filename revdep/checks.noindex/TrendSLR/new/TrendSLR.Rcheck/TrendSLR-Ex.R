pkgname <- "TrendSLR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('TrendSLR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Balt")
### * Balt

flush(stderr()); flush(stdout())

### Name: Balt
### Title: Ocean water level data for Baltimore, USA
### Aliases: Balt
### Keywords: datasets

### ** Examples

data(Balt) # typical data file structure
ts1 <- ts(Balt[2], start = Balt[1, 1]) # convert to time series object
plot(ts1, type = "l", xlab = "Year", ylab = "Annual Average Mean Sea Level (mm)",
main = 'BALTIMORE, USA')
str(Balt) # check structure of data file



cleanEx()
nameEx("check.decomp")
### * check.decomp

flush(stderr()); flush(stdout())

### Name: check.decomp
### Title: Diagnostic tools to inspect SSA decomposition for mean sea level
###   records.
### Aliases: check.decomp

### ** Examples

# -------------------------------------------------------------------------
# View application of different diagnostic tools for Baltimore mean sea level record.
# -------------------------------------------------------------------------

data(Balt) # Baltimore mean sea level record
ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object

g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 1) # SSA filled gap

check.decomp(g, option = 3) # check screen plot, default settings
check.decomp(g, option = 3, comps = 10) # check screen plot
check.decomp(g, option = 4) # check screen plot
check.decomp(g, option = 5) # check screen plot, default settings
check.decomp(g, option = 5, trend = c(1,2), DOF = 20) # check screen plot
check.decomp(g, option = 5, trend = c(1,2,3), DOF = 30) # check screen plot




cleanEx()
nameEx("custom.trend")
### * custom.trend

flush(stderr()); flush(stdout())

### Name: custom.trend
### Title: Isolate trend component from mean sea level records via
###   customised input parameters and analysis
### Aliases: custom.trend

### ** Examples


data(Balt) # Baltimore mean sea level record
ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object
g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 1) # SSA gap fill

data(t)
str(t) # check structure of object




cleanEx()
nameEx("gap.fillview")
### * gap.fillview

flush(stderr()); flush(stdout())

### Name: gap.fillview
### Title: Inspect gap-filling options for mean sea level records.
### Aliases: gap.fillview

### ** Examples

# -------------------------------------------------------------------------
# View different options for filling the Baltimore annual mean sea level record.
# -------------------------------------------------------------------------

data(Balt) # Baltimore mean sea level record
ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object

g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 1) # SSA
g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 2) # Linear interpolation
g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 3) # Cubic spline interpolation
g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 4) # Stineman's interpolation
g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 5) # Weighted moving average

str(g) # Check structure of outputted object




cleanEx()
nameEx("msl.fileplot")
### * msl.fileplot

flush(stderr()); flush(stdout())

### Name: msl.fileplot
### Title: Plotting to file options in JPEG format.
### Aliases: msl.fileplot

### ** Examples


# Plot to file from "custom.trend" object

data(t) # "custom.trend" object
str(t) # check object

# -------------------------------------------------------------------------
# The following call to msl.fileplot can be found in the temporary
# directory under the file name "Plot1.jpeg".
# -------------------------------------------------------------------------

wd <- tempdir() # find temp directory
msl.fileplot(t, wdir = wd) # default screen plot output




cleanEx()
nameEx("msl.screenplot")
### * msl.screenplot

flush(stderr()); flush(stdout())

### Name: msl.screenplot
### Title: Plotting to filescreen options.
### Aliases: msl.screenplot

### ** Examples


# Plot to screen from "msl.trend" object

data(s) # "msl.trend" object
str(s) # check object

msl.screenplot(s) # default screen plot output, 3 panels, 95% confidence intervals
msl.screenplot(s, type=2) # plot time series, 95% confidence intervals
msl.screenplot(s, type=3) # plot velocity, 95% confidence intervals
msl.screenplot(s, type=4, ci=2) # plot acceleration, 99% confidence intervals
msl.screenplot(s, type=5, ci=2) # 2 panels, 99% confidence intervals




cleanEx()
nameEx("msl.trend")
### * msl.trend

flush(stderr()); flush(stdout())

### Name: msl.trend
### Title: Isolate trend component from mean sea level records.
### Aliases: msl.trend

### ** Examples


data(Balt) # Baltimore mean sea level record
ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object

data(s)
str(s) # check structure of object
msl.screenplot(s) # check screen output




cleanEx()
nameEx("s")
### * s

flush(stderr()); flush(stdout())

### Name: s
### Title: sample 'msl.trend' object
### Aliases: s
### Keywords: datasets

### ** Examples


data(Balt) # Baltimore mean sea level record
ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object

data(s)
str(s) # check structure of object
msl.screenplot(s) # check screen output



cleanEx()
nameEx("summary")
### * summary

flush(stderr()); flush(stdout())

### Name: summary
### Title: Summary outputs of decomposed time series.
### Aliases: summary

### ** Examples


data(s) # msl.trend object
data(t) # custom.trend object
summary(s) # summary for object of class 'msl.trend'
summary(t) # summary for object of class 'custom.trend'




cleanEx()
nameEx("t")
### * t

flush(stderr()); flush(stdout())

### Name: t
### Title: sample 'custom.trend' object
### Aliases: t
### Keywords: datasets

### ** Examples


data(Balt) # Baltimore mean sea level record
ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object
g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 1) # SSA gap fill

data(t)
str(t) # check structure of object



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
