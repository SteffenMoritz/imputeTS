pkgname <- "imputeTestbench"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('imputeTestbench')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("impute_errors")
### * impute_errors

flush(stderr()); flush(stdout())

### Name: impute_errors
### Title: Function working as testbench for comparison of imputing models
### Aliases: impute_errors

### ** Examples

## Not run: 
##D # default options
##D aa <- impute_errors(dataIn = nottem)
##D aa
##D plot_errors(aa)
##D 
##D # change the simulation for missing obs
##D aa <- impute_errors(dataIn = nottem, smps = 'mar')
##D aa
##D plot_errors(aa)
##D 
##D # use one interpolation method, increase repetitions
##D aa <- impute_errors(dataIn = nottem, methods = 'na.interp', repetition = 100)
##D aa
##D plot_errors(aa)
##D 
##D # change the error metric
##D aa <- impute_errors(dataIn = nottem, errorParameter = 'mae')
##D aa
##D plot_errors(aa)
##D 
##D # passing additional arguments to imputation methods
##D impute_errors(dataIn = nottem, addl_arg = list(na_mean = list(option = 'mode')))
## End(Not run)



cleanEx()
nameEx("mae")
### * mae

flush(stderr()); flush(stdout())

### Name: mae
### Title: Mean Absolute Error Calculation
### Aliases: mae

### ** Examples

## Generate 100 random numbers within some limits
x <- sample(1:7, 100, replace = TRUE)
y <- sample(1:4, 100, replace = TRUE)
z <- mae(x, y)
z



cleanEx()
nameEx("mape")
### * mape

flush(stderr()); flush(stdout())

### Name: mape
### Title: Mean Absolute Percent Error Calculation
### Aliases: mape

### ** Examples

## Generate 100 random numbers within some limits
x <- sample(1:7, 100, replace = TRUE)
y <- sample(1:4, 100, replace = TRUE)
z <- mape(x, y)
z



cleanEx()
nameEx("plot_errors")
### * plot_errors

flush(stderr()); flush(stdout())

### Name: plot_errors
### Title: Function to plot the Error Comparison
### Aliases: plot_errors plot_errors.errprof

### ** Examples

aa <- impute_errors(dataIn = nottem)

# default plot
plot_errors(aa)
## Not run: 
##D # bar plot of averages at each repetition
##D plot_errors(aa, plotType = 'bar')
##D 
##D # line plot of averages at each repetition
##D plot_errors(aa, plotType = 'line')
##D 
##D # change the plot aesthetics
##D 
##D library(ggplot2)
##D p <- plot_errors(aa)
##D p + scale_fill_brewer(palette = 'Paired', guide_legend(title = 'Default'))
##D p + theme(legend.position = 'top')
##D p + theme_minimal()
##D p + ggtitle('Distribution of error for imputed values')
##D p + scale_y_continuous('RMSE')
## End(Not run)



cleanEx()
nameEx("plot_impute")
### * plot_impute

flush(stderr()); flush(stdout())

### Name: plot_impute
### Title: Plot imputations
### Aliases: plot_impute

### ** Examples

# default
plot_impute(dataIn = nottem)

# change missing percent total
plot_impute(dataIn = nottem, missPercent = 10)

# show missing values
plot_impute(dataIn = nottem, showmiss = TRUE)

# use mar sampling
plot_impute(dataIn = nottem, smps = 'mar')

# change the plot aesthetics
## Not run: 
##D library(ggplot2)
##D p <- plot_impute(dataIn = nottem, smps = 'mar')
##D p + scale_colour_manual(values = c('black', 'grey'))
##D p + theme_minimal()
##D p + ggtitle('Imputation examples with different methods')
##D p + scale_y_continuous('Temp at Nottingham Castle (F)')
## End(Not run)



cleanEx()
nameEx("rmse")
### * rmse

flush(stderr()); flush(stdout())

### Name: rmse
### Title: Root Mean Square Error Calculation
### Aliases: rmse

### ** Examples

## Generate 100 random numbers within some limits
x <- sample(1:7, 100, replace = TRUE)
y <- sample(1:4, 100, replace = TRUE)
z <- rmse(x, y)
z



cleanEx()
nameEx("sample_dat")
### * sample_dat

flush(stderr()); flush(stdout())

### Name: sample_dat
### Title: Sample time series data
### Aliases: sample_dat

### ** Examples

a <- rnorm(1000)

# default sampling
sample_dat(a)

# use mar sampling
sample_dat(a, smps = 'mar')

# show a plot of one repetition
sample_dat(a, plot = TRUE)

# show a plot of one repetition, mar sampling
sample_dat(a, smps = 'mar', plot = TRUE)

# change plot aesthetics
library(ggplot2)
p <- sample_dat(a, plot = TRUE)
p + scale_colour_manual(values = c('black', 'grey'))
p + theme_minimal()
p + ggtitle('Example of simulating missing data')



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
