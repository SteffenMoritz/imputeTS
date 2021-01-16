pkgname <- "baytrends"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('baytrends')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("analysisOrganizeData")
### * analysisOrganizeData

flush(stderr()); flush(stdout())

### Name: analysisOrganizeData
### Title: Analysis Organization & Data Preparation
### Aliases: analysisOrganizeData

### ** Examples

# run analysis relying on default specifications, examine analySpec for
# default options
dfr <- analysisOrganizeData(dataCensored)
df        <- dfr[["df"]]
analySpec <- dfr[["analySpec"]]

# analyze bottom dissolved oxygen at 2 stations using only data from 1/1/1995-12/31/2015
analySpec <-list()
analySpec$parameterFilt <- c('do')
analySpec$layerFilt     <- c('B')
analySpec$stationFilt   <- c('CB3.3C', 'CB5.4')
analySpec$dateFilt      <- as.POSIXct(c("1995-01-01", "2015-12-31"))
dfr <- analysisOrganizeData(dataCensored, analySpec)
df        <- dfr[["df"]]
analySpec <- dfr[["analySpec"]]




cleanEx()
nameEx("baseDay")
### * baseDay

flush(stderr()); flush(stdout())

### Name: baseDay
### Title: Base Day
### Aliases: baseDay
### Keywords: internal manip

### ** Examples


# The default numeric result
baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"))
# The result as a factor
baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"), numeric=FALSE)



cleanEx()
nameEx("baseDay2decimal")
### * baseDay2decimal

flush(stderr()); flush(stdout())

### Name: baseDay2decimal
### Title: Base Day
### Aliases: baseDay2decimal
### Keywords: internal manip

### ** Examples

# The baseDay ordered by calendar year
bd.tmp <- baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"), 
  numeric=FALSE)
baseDay2decimal(bd.tmp)
# ordered by water year, result should agree
bd.tmp <- baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"), 
  numeric=FALSE, year="water")
baseDay2decimal(bd.tmp)



cleanEx()
nameEx("closeOut")
### * closeOut

flush(stderr()); flush(stdout())

### Name: closeOut
### Title: Document Processing Time and Other Session Time
### Aliases: closeOut

### ** Examples

closeOut()



cleanEx()
nameEx("dectime")
### * dectime

flush(stderr()); flush(stdout())

### Name: dectime
### Title: Decimal Time
### Aliases: dectime
### Keywords: internal manip

### ** Examples


dectime("11/11/1918", date.format="%m/%d/%Y")
dectime(1988:1990)



cleanEx()
nameEx("dectime2Date")
### * dectime2Date

flush(stderr()); flush(stdout())

### Name: dectime2Date
### Title: Date Conversion
### Aliases: dectime2Date
### Keywords: internal manip

### ** Examples


dectime("02/07/2013", date.format="%m/%d/%Y")
# Convert back the printed result:
dectime2Date(2013.103)



cleanEx()
nameEx("detrended.flow")
### * detrended.flow

flush(stderr()); flush(stdout())

### Name: detrended.flow
### Title: Create Seasonally Detrended Flow Data Set
### Aliases: detrended.flow

### ** Examples

## Not run: 
##D # Define Function Inputs
##D usgsGageID    <- c("01491000", "01578310")
##D siteName      <- c("Choptank River near Greensboro, MD",
##D                    "Susquehanna River at Conowingo, MD")
##D yearStart     <- 1983
##D yearEnd       <- 2016
##D dvAvgWinSel   <- c(1, 5, 10, 15, 20, 30, 40, 50, 60, 90, 120, 150, 180, 210)
##D dvAvgWgtSel   <- "uniform"
##D dvAvgSidesSel <- 1
##D lowess.f      <- 0.2
##D                  
##D # Run Function
##D flow.detrended <- detrended.flow(usgsGageID, siteName, yearStart, yearEnd
##D                                 , dvAvgWinSel, dvAvgWgtSel, dvAvgSidesSel
##D                                , lowess.f)
## End(Not run)



cleanEx()
nameEx("detrended.salinity")
### * detrended.salinity

flush(stderr()); flush(stdout())

### Name: detrended.salinity
### Title: Create Seasonally Detrended Salinty Data Set
### Aliases: detrended.salinity

### ** Examples

## Not run: 
##D # Show Example Dataset (sal)
##D str(sal)
##D 
##D # Define Function Inputs
##D df.sal        <- sal
##D dvAvgWinSel   <- 30
##D lowess.f      <- 0.2
##D minObs        <- 40
##D minObs.sd    <- 10
##D                  
##D # Run Function
##D salinity.detrended <- detrended.salinity(df.sal, dvAvgWinSel, 
##D                                  lowess.f, minObs, minObs.sd) 
## End(Not run)              



cleanEx()
nameEx("dot-F")
### * dot-F

flush(stderr()); flush(stdout())

### Name: .F
### Title: Print out figure title (customization of pandoc.emphasis and
###   pandoc.strong )
### Aliases: .F
### Keywords: internal

### ** Examples

text<-"Hello World!"
.F(text)
.F(text, 4)
.F(text, 4,'e')
.F(text, 4,'s')



cleanEx()
nameEx("dot-H")
### * dot-H

flush(stderr()); flush(stdout())

### Name: .H
### Title: Print out header (shortened pandoc.header)
### Aliases: .H
### Keywords: internal

### ** Examples

.H("1st level header",1)



cleanEx()
nameEx("dot-H1")
### * dot-H1

flush(stderr()); flush(stdout())

### Name: .H1
### Title: Print out 1st level header (shortened pandoc.header)
### Aliases: .H1
### Keywords: internal

### ** Examples

.H1("1st level header")
.H3("3rd level header")



cleanEx()
nameEx("dot-H2")
### * dot-H2

flush(stderr()); flush(stdout())

### Name: .H2
### Title: Print out 2nd level header (shortened pandoc.header)
### Aliases: .H2
### Keywords: internal

### ** Examples

.H2("2nd level header")
.H3("3rd level header")



cleanEx()
nameEx("dot-H3")
### * dot-H3

flush(stderr()); flush(stdout())

### Name: .H3
### Title: Print out 3rd level header (shortened pandoc.header)
### Aliases: .H3
### Keywords: internal

### ** Examples

.H2("2nd level header")
.H3("3rd level header")



cleanEx()
nameEx("dot-H4")
### * dot-H4

flush(stderr()); flush(stdout())

### Name: .H4
### Title: Print out 4th level header (shortened pandoc.header)
### Aliases: .H4
### Keywords: internal

### ** Examples

.H2("2nd level header")
.H4("4th level header")



cleanEx()
nameEx("dot-H5")
### * dot-H5

flush(stderr()); flush(stdout())

### Name: .H5
### Title: Print out 5th level header (shortened pandoc.header)
### Aliases: .H5
### Keywords: internal

### ** Examples

.H2("2nd level header")
.H5("5th level header")



cleanEx()
nameEx("dot-P")
### * dot-P

flush(stderr()); flush(stdout())

### Name: .P
### Title: Paragraph (customization of pandoc.p)
### Aliases: .P
### Keywords: internal

### ** Examples

.P()



cleanEx()
nameEx("dot-T")
### * dot-T

flush(stderr()); flush(stdout())

### Name: .T
### Title: Print out table title (customization of pandoc.emphasis and
###   pandoc.strong )
### Aliases: .T
### Keywords: internal

### ** Examples

text<-"Hello World!"
.T(text)
.T(text, 4)
.T(text, 4,'e')
.T(text, 4,'s')



cleanEx()
nameEx("dot-V")
### * dot-V

flush(stderr()); flush(stdout())

### Name: .V
### Title: Print out text (blended pandoc.emphasis, .verbatim, and .strong)
### Aliases: .V
### Keywords: internal

### ** Examples

.V("Hello World!",'v')
.V("Hello World!",'e')
.V("Hello World!",'s')
.V("Hello World!")



cleanEx()
nameEx("dot-checkRange")
### * dot-checkRange

flush(stderr()); flush(stdout())

### Name: .checkRange
### Title: Check Data Range - function that checks for allowable values
### Aliases: .checkRange
### Keywords: internal

### ** Examples

# create an example data frame
df <- data.frame(
       x1 = c("X1","Y2","A1","B2","C1", "X1","","A1","","C1"),
       x2 = seq(5, 14 ) + runif(10) ,
       x3 = as.POSIXct(c("1/10/2008", "1/21/2008", "3/1/2008", "3/26/1993",
                         "11/1/2012", "6/10/2000", "8/2/1990", "7/8/2005",
                         "1/6/2008", "9/11/2008"),
                         format="%m/%d/%Y"), stringsAsFactors =FALSE)
# add a few missing values
df[1,1]=NA
df[3,2]=NA
df[5,3]=NA
df

# establish allowable values for screening
x1Scrn <- as.character(c("A1", "B2", "C1", "Y2"))   # character
x2Scrn <- c(7,13)                                   # min/max value
x3Scrn <- as.POSIXct(c("1999-01-01", "2008-09-10")) # min/max date (POSIXct format)

# return df with new column indicating pass [TRUE] / fail [FALSE]
.checkRange(df, var="x1", varScrn=x1Scrn, numNA=FALSE, deleteOption='mark')
.checkRange(df, var="x2", varScrn=x2Scrn, numNA=FALSE, deleteOption='mark')
.checkRange(df, var="x3", varScrn=x3Scrn, numNA=FALSE, deleteOption='mark')

# return df with only rows that pass check
.checkRange(df, var="x1", varScrn=x1Scrn, numNA=FALSE, deleteOption='pass')
.checkRange(df, var="x2", varScrn=x2Scrn, numNA=FALSE, deleteOption='pass')
.checkRange(df, var="x3", varScrn=x3Scrn, numNA=FALSE, deleteOption='pass')



cleanEx()
nameEx("dot-chkParameter")
### * dot-chkParameter

flush(stderr()); flush(stdout())

### Name: .chkParameter
### Title: Reduce dataframe and parameter list based on user selected
###   parameterFilt
### Aliases: .chkParameter
### Keywords: internal

### ** Examples

#df <- chkParameter(df,parameterFilt=c("tn", "tp"))



cleanEx()
nameEx("dot-findFile")
### * dot-findFile

flush(stderr()); flush(stdout())

### Name: .findFile
### Title: Find Recent File Information
### Aliases: .findFile
### Keywords: internal

### ** Examples

# name of most recently modified file
## Not run: 
##D .findFile()         # current directory
##D .findFile("..")     # one directory up
##D #
##D # list of files and common attributes one directory up
##D .findFile(folder="..", file="*.*", n=2, fileNameOnly=FALSE)      #two most recent files
##D .findFile(folder="..", file="*.*", n="all", fileNameOnly=FALSE)  #all files
## End(Not run)



cleanEx()
nameEx("dot-reAttDF")
### * dot-reAttDF

flush(stderr()); flush(stdout())

### Name: .reAttDF
### Title: Re-attribute df based on previous df
### Aliases: .reAttDF
### Keywords: internal

### ** Examples

# create data frame
df0 <- data.frame (sta=c("A","A"), lay=c("B","C"), x1 =c(NA,2), x2 =c( 4,14))

#add simple attribute
attr(df0, "Attribute1") <- "Test attribute1"

#run aggregate -- loose attributes
df1 <- aggregate(x2 ~ sta, data=df0, mean, na.action=na.pass, na.rm=TRUE)
df2 <- .reAttDF(df1, df0)



cleanEx()
nameEx("eventProcessing")
### * eventProcessing

flush(stderr()); flush(stdout())

### Name: eventNum
### Title: Event Processing
### Aliases: eventNum eventLen eventSeq
### Keywords: internal manip

### ** Examples


## Notice the difference caused by setting reset to TRUE
eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE))
eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE)

## Notice the difference caused by setting reset to TRUE
eventSeq(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE)))
eventSeq(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE))

## Notice the difference caused by setting reset to TRUE
eventLen(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE))
## This is an example of the summary option
eventLen(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE), summary=TRUE)



cleanEx()
nameEx("fillMissing")
### * fillMissing

flush(stderr()); flush(stdout())

### Name: fillMissing
### Title: Fill Missing Values
### Aliases: fillMissing
### Keywords: internal manip

### ** Examples

## Not run: 
##D #library(smwrData)
##D data(Q05078470)
##D # Create missing values in flow, the first sequence is a peak and the second is a recession
##D Q05078470$FlowMiss <- Q05078470$FLOW
##D Q05078470$FlowMiss[c(109:111, 198:201)] <- NA
##D # Interpolate the missing values
##D Q05078470$FlowFill <- fillMissing(Q05078470$FlowMiss)
##D # How did we do (line is actual, points are filled values)?
##D par(mfrow=c(2,1), mar=c(5.1, 4.1, 1.1, 1.1))
##D with(Q05078470[100:120, ], plot(DATES, FLOW, type="l"))
##D with(Q05078470[109:111, ], points(DATES, FlowFill))
##D with(Q05078470[190:210, ], plot(DATES, FLOW, type="l"))
##D with(Q05078470[198:201, ], points(DATES, FlowFill))
## End(Not run)



cleanEx()
nameEx("filterWgts")
### * filterWgts

flush(stderr()); flush(stdout())

### Name: filterWgts
### Title: Create filter weights
### Aliases: filterWgts
### Keywords: internal

### ** Examples

wgts<- filterWgts(0,"uniform")
wgts<- filterWgts(7,"uniform")
wgts<- filterWgts(7,"centered")
wgts<- filterWgts(7,"weighted")
x <- 1:100
filter(x, filterWgts(7,"weighted"), sides=1)



cleanEx()
nameEx("gamDiff")
### * gamDiff

flush(stderr()); flush(stdout())

### Name: gamDiff
### Title: Compute an estimate of difference based on GAM results
### Aliases: gamDiff

### ** Examples

# run analysisOrganizeData function to create the list analySpec
dfr <- analysisOrganizeData (dataCensored, report=NA)
df        <- dfr[["df"]]
analySpec <- dfr[["analySpec"]]

# set GAM models to just one model
analySpec$gamModels <- list(
  list(option=2, name= "Non-linear trend with Seasonality (plus Interactions)",
       model= "~ cyear + s(cyear) + s(doy,bs='cc')+ ti(cyear,doy,bs=c('tp','cc'))", deriv=FALSE))

# run GAM for a single water quality variable, station and layer
gamResult <- gamTest(df, 'tn', 'CB5.4', 'S', analySpec=analySpec)

# use gamDiff to replicate estimates of change calculated in the above
gamDiff(gamRslt=gamResult[["gamOutput2"]]$gamRslt,
        iSpec=gamResult$iSpec, analySpec=analySpec,
        base.yr.set = NA, test.yr.set = NA,
        doy.set = NA, alpha = 0.05)

# use gamDiff to calculate changes from 2005/06 to 2013/14
gamDiff(gamRslt=gamResult[["gamOutput2"]]$gamRslt,
        iSpec=gamResult$iSpec, analySpec=analySpec,
        base.yr.set = c(2004:2005), test.yr.set = c(2013:2014),
        doy.set = NA, alpha = 0.05)




cleanEx()
nameEx("gamPlotDispSeason")
### * gamPlotDispSeason

flush(stderr()); flush(stdout())

### Name: gamPlotDispSeason
### Title: Plot censored gam fits vs. time
### Aliases: gamPlotDispSeason

### ** Examples

## Not run: 
##D # Specify parameter and station to analyze
##D dep        <- 'do'
##D stat       <- 'CB5.4'
##D layer      <- 'B'
##D 
##D # Prepare data and set up specifications for analysis
##D dfr <- analysisOrganizeData (dataCensored)
##D df        <- dfr[[1]]
##D analySpec <- dfr[[2]]
##D 
##D # Apply gamTest 
##D gamResult <- gamTest(df, dep, stat, layer, analySpec=analySpec)
##D gamPlotDisp(gamResult = gamResult, analySpec = analySpec,
##D             fullModel = 2, seasAvgModel = 2, seasonalModel = 2,
##D             diffType = "regular", obserPlot = TRUE, interventionPlot = TRUE,
##D             seasAvgPlot = TRUE, seasAvgConfIntPlot = FALSE,
##D             seasAvgSigPlot = FALSE, fullModelPlot = TRUE, seasModelPlot = TRUE,
##D             BaseCurrentMeanPlot = FALSE, adjustedPlot = FALSE)
##D 
##D # Apply gamTestSeason
##D gamResult2 <- gamTestSeason(df, dep, stat, layer, analySpec=analySpec,
##D                             gamSeasonPlot = c("7/15-8/15", "purple", "range"))
##D gamPlotDispSeason(gamResult = gamResult2, analySpec = analySpec,
##D                   fullModel = 2, seasAvgModel = 2, seasonalModel = 2,
##D                   diffType = "regular", obserPlot = TRUE, interventionPlot = TRUE,
##D                   seasAvgPlot = TRUE, seasAvgConfIntPlot = FALSE,
##D                   seasAvgSigPlot = FALSE, fullModelPlot = FALSE, seasModelPlot = FALSE,
##D                   BaseCurrentMeanPlot = TRUE, adjustedPlot = FALSE, gamSeasonFocus = TRUE)
## End(Not run)



cleanEx()
nameEx("gamTest")
### * gamTest

flush(stderr()); flush(stdout())

### Name: gamTest
### Title: Perform GAM analysis
### Aliases: gamTest

### ** Examples

## Not run: 
##D # Specify parameter and station to analyze
##D dep        <- 'do'
##D stat       <- 'CB5.4'
##D layer      <- 'B'
##D 
##D # Prepare data and set up specifications for analysis
##D dfr <- analysisOrganizeData (dataCensored)
##D df        <- dfr[[1]]
##D analySpec <- dfr[[2]]
##D 
##D # Apply gamTest 
##D gamResult <- gamTest(df, dep, stat, layer, analySpec=analySpec)
##D gamPlotDisp(gamResult = gamResult, analySpec = analySpec,
##D             fullModel = 2, seasAvgModel = 2, seasonalModel = 2,
##D             diffType = "regular", obserPlot = TRUE, interventionPlot = TRUE,
##D             seasAvgPlot = TRUE, seasAvgConfIntPlot = FALSE,
##D             seasAvgSigPlot = FALSE, fullModelPlot = TRUE, seasModelPlot = TRUE,
##D             BaseCurrentMeanPlot = FALSE, adjustedPlot = FALSE)
##D 
##D # Apply gamTestSeason
##D gamResult2 <- gamTestSeason(df, dep, stat, layer, analySpec=analySpec,
##D                             gamSeasonPlot = c("7/15-8/15", "purple", "range"))
##D gamPlotDispSeason(gamResult = gamResult2, analySpec = analySpec,
##D                   fullModel = 2, seasAvgModel = 2, seasonalModel = 2,
##D                   diffType = "regular", obserPlot = TRUE, interventionPlot = TRUE,
##D                   seasAvgPlot = TRUE, seasAvgConfIntPlot = FALSE,
##D                   seasAvgSigPlot = FALSE, fullModelPlot = FALSE, seasModelPlot = FALSE,
##D                   BaseCurrentMeanPlot = TRUE, adjustedPlot = FALSE, gamSeasonFocus = TRUE)
## End(Not run)



cleanEx()
nameEx("gamTestSeason")
### * gamTestSeason

flush(stderr()); flush(stdout())

### Name: gamTestSeason
### Title: Perform GAM analysis for Specified Season
### Aliases: gamTestSeason

### ** Examples

## Not run: 
##D # Specify parameter and station to analyze
##D dep        <- 'do'
##D stat       <- 'CB5.4'
##D layer      <- 'B'
##D 
##D # Prepare data and set up specifications for analysis
##D dfr <- analysisOrganizeData (dataCensored)
##D df        <- dfr[[1]]
##D analySpec <- dfr[[2]]
##D 
##D # Apply gamTest 
##D gamResult <- gamTest(df, dep, stat, layer, analySpec=analySpec)
##D gamPlotDisp(gamResult = gamResult, analySpec = analySpec,
##D             fullModel = 2, seasAvgModel = 2, seasonalModel = 2,
##D             diffType = "regular", obserPlot = TRUE, interventionPlot = TRUE,
##D             seasAvgPlot = TRUE, seasAvgConfIntPlot = FALSE,
##D             seasAvgSigPlot = FALSE, fullModelPlot = TRUE, seasModelPlot = TRUE,
##D             BaseCurrentMeanPlot = FALSE, adjustedPlot = FALSE)
##D 
##D # Apply gamTestSeason
##D gamResult2 <- gamTestSeason(df, dep, stat, layer, analySpec=analySpec,
##D                             gamSeasonPlot = c("7/15-8/15", "purple", "range"))
##D gamPlotDispSeason(gamResult = gamResult2, analySpec = analySpec,
##D                   fullModel = 2, seasAvgModel = 2, seasonalModel = 2,
##D                   diffType = "regular", obserPlot = TRUE, interventionPlot = TRUE,
##D                   seasAvgPlot = TRUE, seasAvgConfIntPlot = FALSE,
##D                   seasAvgSigPlot = FALSE, fullModelPlot = FALSE, seasModelPlot = FALSE,
##D                   BaseCurrentMeanPlot = TRUE, adjustedPlot = FALSE, gamSeasonFocus = TRUE)
## End(Not run)     



cleanEx()
nameEx("getUSGSflow")
### * getUSGSflow

flush(stderr()); flush(stdout())

### Name: getUSGSflow
### Title: Retrieve USGS daily flow data in a wide format
### Aliases: getUSGSflow
### Keywords: internal

### ** Examples

# set retrieval parameters
yearStart   <- 2014
yearEnd     <- 2014
siteNumber <- c('01578310')

# regular retrieval (default usage)
df <- getUSGSflow(siteNumber, yearStart, yearEnd)




cleanEx()
nameEx("impute")
### * impute

flush(stderr()); flush(stdout())

### Name: impute
### Title: Impute Censored Values
### Aliases: impute

### ** Examples

## Not run: 
##D x  <- dataCensored[1:20,"tdp"]
##D x.lower <- impute(x,'lower')
##D x.mid   <- impute(x,'mid')
##D x.upper <- impute(x,'upper')
##D x.norm  <- impute(x,'norm')
##D x.lnorm <- impute(x,'lnorm')
## End(Not run)



cleanEx()
nameEx("imputeDF")
### * imputeDF

flush(stderr()); flush(stdout())

### Name: imputeDF
### Title: Impute Censored Values in dataframes
### Aliases: imputeDF

### ** Examples

## Not run: 
##D df  <- dataCensored[1:20, ]
##D df.lower <- imputeDF(df,'lower')
##D df.mid   <- imputeDF(df,'mid')
##D df.upper <- imputeDF(df,'upper')
##D df.norm  <- imputeDF(df,'norm')
##D df.lnorm <- imputeDF(df,'lnorm')
## End(Not run)



cleanEx()
nameEx("layerAggregation")
### * layerAggregation

flush(stderr()); flush(stdout())

### Name: layerAggregation
### Title: Aggregate data layers
### Aliases: layerAggregation

### ** Examples

## Not run: 
##D dfr    <- analysisOrganizeData(dataCensored)
##D 
##D # retrieve all corrected chlorophyll-a concentrations for Station CB5.4,
##D # missing values are removed and transformation applied. Note, a 
##D # warning is displayed indicating that data set has layers but user did
##D # not specify layer in retrieval. layerAggregation then aggregates per 
##D # specifications
##D dfr2   <- selectData(dfr[["df"]], 'chla', 'CB5.4', analySpec=dfr[["analySpec"]])
##D df2    <- dfr2[[1]]   # data frame of selected data
##D iSpec2 <- dfr2[[2]]   # meta data about selected data
##D df2a   <- layerAggregation(df2, avgTechnique="mean", layerAggOption=4)
## End(Not run)




cleanEx()
nameEx("loadModels")
### * loadModels

flush(stderr()); flush(stdout())

### Name: loadModels
### Title: Load Built-in GAM formulas
### Aliases: loadModels

### ** Examples

# run analysisOrganizeData function to create the list analySpec
dfr <- analysisOrganizeData (dataCensored, report=NA)
df        <- dfr[["df"]]
analySpec <- dfr[["analySpec"]]

# current models in analySpec
analySpec$gamModels

# set models in analySpec to gam0, gam1, and gam2 only
analySpec$gamModels <- loadModels(c('gam0','gam1','gam2'))




cleanEx()
nameEx("makeSurvDF")
### * makeSurvDF

flush(stderr()); flush(stdout())

### Name: makeSurvDF
### Title: Convert dataframe to include survival (Surv) objects
### Aliases: makeSurvDF

### ** Examples

df <- dataCensored[1:20,]
colnames(df)
df1 <- unSurvDF(df)
colnames(df1)
# Default values
df2 <- makeSurvDF(df1)
colnames(df2)
# User values
df3 <- unSurvDF(df, "_LOW", "_HIGH")
colnames(df3)
df4 <- makeSurvDF(df3, "_LOW", "_HIGH")
colnames(df4)




cleanEx()
nameEx("na2miss")
### * na2miss

flush(stderr()); flush(stdout())

### Name: na2miss
### Title: Recode Data
### Aliases: na2miss miss2na
### Keywords: internal manip

### ** Examples


## Construct simple substitutions
na2miss(c(1, 2, 3, NA, 5, 6))



cleanEx()
nameEx("nobs")
### * nobs

flush(stderr()); flush(stdout())

### Name: nobs
### Title: Compute the Number of Non-Missing Observations
### Aliases: nobs

### ** Examples

x <- c(1,2,3,5,NA,6,7,1,NA )
length(x)
nobs(x)

df <- data.frame(x=rnorm(100), y=rnorm(100))
df[1,1] <- NA
df[1,2] <- NA
df[2,1] <- NA

nobs(df)

fit <- lm(y ~ x, data=df)
nobs(fit)




cleanEx()
nameEx("saveDF")
### * saveDF

flush(stderr()); flush(stdout())

### Name: saveDF
### Title: Save R object to disk
### Aliases: saveDF

### ** Examples

## Not run: 
##D df <- data.frame(x=c(1:100))
##D saveDF(df, 'test_note')
## End(Not run)




cleanEx()
nameEx("seasAdjflow")
### * seasAdjflow

flush(stderr()); flush(stdout())

### Name: seasAdjflow
### Title: Create Daily Seasonally-adjusted Log Flow Residuals
### Aliases: seasAdjflow
### Keywords: internal

### ** Examples

#Set Retrieval Parameters
yearStart   <- 1983
yearEnd     <- 2015
siteNumbers <- c("01578310")

# Regular Retrieval (default usage)
df <- getUSGSflow(siteNumbers, yearStart, yearEnd, fill=TRUE)
# Apply default smoothing
df <- seasAdjflow(df,"01578310")




cleanEx()
nameEx("selectData")
### * selectData

flush(stderr()); flush(stdout())

### Name: selectData
### Title: Select data for analysis from a larger data frame
### Aliases: selectData

### ** Examples

## Not run: 
##D dfr    <- analysisOrganizeData(dataCensored)
##D 
##D # retrieve Secchi depth for Station CB5.4, no transformations are applied
##D dfr1   <- selectData(dfr[["df"]], 'secchi', 'CB5.4', 'S', transform=FALSE,
##D                     remMiss=FALSE, analySpec=dfr[["analySpec"]])
##D df1    <- dfr1[[1]]   # data frame of selected data
##D iSpec1 <- dfr1[[2]]   # meta data about selected data
##D 
##D # retrieve surface corrected chlorophyll-a concentrations for Station CB5.4,
##D # missing values are removed and transformation applied
##D dfr2   <- selectData(dfr[["df"]], 'chla', 'CB5.4', 'S', analySpec=dfr[["analySpec"]])
##D df2    <- dfr2[[1]]   # data frame of selected data
##D iSpec2 <- dfr2[[2]]   # meta data about selected data
## End(Not run)



cleanEx()
nameEx("unSurv")
### * unSurv

flush(stderr()); flush(stdout())

### Name: unSurv
### Title: Converts Surv object into a 3-column matrix
### Aliases: unSurv

### ** Examples

df1 <- dataCensored[dataCensored$station=="CB3.3C","chla"][1:30]
colnames(df1)
# Default values
df2 <- unSurv(df1)
colnames(df2)
# User values
df3 <- unSurv(df1, "LOW", "HIGH")
colnames(df3)




cleanEx()
nameEx("unSurvDF")
### * unSurvDF

flush(stderr()); flush(stdout())

### Name: unSurvDF
### Title: Converts Surv objects in a dataframe to "lo" and "hi" values
### Aliases: unSurvDF

### ** Examples

df <- dataCensored[dataCensored$station=="CB3.3C", ][1:20,]
colnames(df)
# Default values
df2 <- unSurvDF(df)
colnames(df2)
# User values
df3 <- unSurvDF(df, "_LOW", "_HIGH")
colnames(df3)




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
