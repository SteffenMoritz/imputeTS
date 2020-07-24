pkgname <- "RiverLoad"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('RiverLoad')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CQregression")
### * CQregression

flush(stderr()); flush(stdout())

### Name: CQregression
### Title: Relationship between concentration and flow
### Aliases: CQregression
### Keywords: regression

### ** Examples

data("flow.data1","conc.data1")
union<-db.union(flow.data1, conc.data1)
reg.relationship<-CQregression(union, 2)



cleanEx()
nameEx("annual.mean")
### * annual.mean

flush(stderr()); flush(stdout())

### Name: annual.mean
### Title: Annual mean of flow records
### Aliases: annual.mean
### Keywords: univar

### ** Examples

data("flow.data1")
annual.mean(flow.data1)
annual.mean(flow.data1, "sd")



cleanEx()
nameEx("beale.period")
### * beale.period

flush(stderr()); flush(stdout())

### Name: beale.period
### Title: Load estimation with Beale ratio estimator based on monthly or
###   annual relationship
### Aliases: beale.period
### Keywords: univ

### ** Examples

data("flow.data1","conc.data1")
union<-db.union(flow.data1, conc.data1)
beale.periodM<-beale.period(union, 2, "month")
beale.periodY<-beale.period(union, 2, "year")



cleanEx()
nameEx("beale.ratio")
### * beale.ratio

flush(stderr()); flush(stdout())

### Name: beale.ratio
### Title: Load estimation with Beale ratio method
### Aliases: beale.ratio
### Keywords: univ

### ** Examples

data("flow.data1", "conc.data1")
union<-db.union(flow.data1, conc.data1)
beale<-beale.ratio(union, 2)
beale.month<-beale.ratio(union, 2, "month")
beale.year<-beale.ratio(union, 2, "year")



cleanEx()
nameEx("conc.data1")
### * conc.data1

flush(stderr()); flush(stdout())

### Name: conc.data1
### Title: Concentration dataset of Kaskaskia River
### Aliases: conc.data1
### Keywords: datasets

### ** Examples

data("conc.data1")
str(conc.data1)
summary(conc.data1)



cleanEx()
nameEx("conc.data2")
### * conc.data2

flush(stderr()); flush(stdout())

### Name: conc.data2
### Title: Concentration dataset of Sandusky River
### Aliases: conc.data2
### Keywords: datasets

### ** Examples

data("conc.data2")
str(conc.data2)
summary(conc.data2)



cleanEx()
nameEx("conc.data3")
### * conc.data3

flush(stderr()); flush(stdout())

### Name: conc.data3
### Title: Dataset with faked concentration records of Adda River
### Aliases: conc.data3
### Keywords: datasets

### ** Examples

data("conc.data3")
str(conc.data3)



cleanEx()
nameEx("daily.mean")
### * daily.mean

flush(stderr()); flush(stdout())

### Name: daily.mean
### Title: Daily mean of flow records
### Aliases: daily.mean
### Keywords: univar

### ** Examples

data("flow.data1")
daily.mean(flow.data1)
daily.mean(flow.data1, "sd")


cleanEx()
nameEx("db.intersect")
### * db.intersect

flush(stderr()); flush(stdout())

### Name: db.intersect
### Title: Intersection of flow and concentration data in a unique data
###   frame
### Aliases: db.intersect
### Keywords: manip datagen

### ** Examples

data("flow.data2","conc.data2")
intersect<-db.intersect(flow.data2, conc.data2)
summary<-db.intersect



cleanEx()
nameEx("db.union")
### * db.union

flush(stderr()); flush(stdout())

### Name: db.union
### Title: Union of flow and concentration data in a unique data frame
### Aliases: db.union
### Keywords: manip datagen

### ** Examples

data("flow.data2", "conc.data2")
intersect<-db.intersect(flow.data2, conc.data2)
summary<-db.intersect



cleanEx()
nameEx("ferguson")
### * ferguson

flush(stderr()); flush(stdout())

### Name: ferguson
### Title: Load estimation with Ferguson rating curve
### Aliases: ferguson
### Keywords: regression

### ** Examples

data("flow.data2", "conc.data2")
union<-db.union(flow.data2, conc.data2)
CQregression(union,1)
ferg<-ferguson(union, 1)
ferg.month<-ferguson(union, 1, "month")
ferg.year<-ferguson(union, 1, "year")



cleanEx()
nameEx("ferguson.period")
### * ferguson.period

flush(stderr()); flush(stdout())

### Name: ferguson.period
### Title: Load estimation with Ferguson method based on monthly or annual
###   relationship
### Aliases: ferguson.period
### Keywords: regression

### ** Examples

data("flow.data2","conc.data2")
union<-db.union(flow.data2, conc.data2)
fer.periodM<-ferguson.period(union, 1, "month")
fer.periodY<-ferguson.period(union, 1, "year")



cleanEx()
nameEx("flow.data1")
### * flow.data1

flush(stderr()); flush(stdout())

### Name: flow.data1
### Title: Dataset of flow record of Kaskskia River
### Aliases: flow.data1
### Keywords: datasets

### ** Examples

data("flow.data1")
summary(flow.data1)



cleanEx()
nameEx("flow.data2")
### * flow.data2

flush(stderr()); flush(stdout())

### Name: flow.data2
### Title: Dataset of flow record of Sandusky River
### Aliases: flow.data2
### Keywords: datasets

### ** Examples

data(flow.data2)
summary(flow.data2)



cleanEx()
nameEx("flow.data3")
### * flow.data3

flush(stderr()); flush(stdout())

### Name: flow.data3
### Title: Flow record dataset of Adda River
### Aliases: flow.data3
### Keywords: datasets

### ** Examples

data("flow.data3")
str(flow.data3)



cleanEx()
nameEx("method1")
### * method1

flush(stderr()); flush(stdout())

### Name: method1
### Title: Load estimation with time-weighted flow and concentration method
### Aliases: method1
### Keywords: univ

### ** Examples

data("flow.data1","conc.data1")
union<-db.union(flow.data1, conc.data1)
met1<-method1(union, 2)
met1.month<-method1(union, 2, "month")
met1.year<-method1(union, 2, "year")



cleanEx()
nameEx("method2")
### * method2

flush(stderr()); flush(stdout())

### Name: method2
### Title: Load estimation with discharge-weighted concentration method
### Aliases: method2
### Keywords: univ

### ** Examples

data("flow.data1", "conc.data1")
union<-db.union(flow.data1, conc.data1)
met2<-method2(union, 2)
met2.month<-method2(union, 2, "month")
met2.year<-method2(union, 2, "year")



cleanEx()
nameEx("method3")
### * method3

flush(stderr()); flush(stdout())

### Name: method3
### Title: Load estimation with mean discharge-weighted concentration
###   method
### Aliases: method3
### Keywords: univ

### ** Examples

data("flow.data1", "conc.data1")
union<-db.union(flow.data1, conc.data1)
met3<-method3(union, 2)
met3.month<-method3(union, 2, "month")
met3.year<-method3(union, 2, "year")



cleanEx()
nameEx("method4")
### * method4

flush(stderr()); flush(stdout())

### Name: method4
### Title: Load estimation with time-weighted concentration method
### Aliases: method4
### Keywords: univ

### ** Examples

data("flow.data1", "conc.data1")
union<-db.union(flow.data1, conc.data1)
met4<-method4(union, 2)
met4.month<-method4(union, 2, "month")
met4.year<-method4(union, 2, "year")



cleanEx()
nameEx("method5")
### * method5

flush(stderr()); flush(stdout())

### Name: method5
### Title: Load estimation with time and discharge weighted method
### Aliases: method5
### Keywords: univ

### ** Examples

data("flow.data1", "conc.data1")
union<-db.union(flow.data1, conc.data1)
met5<-method5(union, 2)
met5.month<-method5(union, 2, "month")
met5.year<-method5(union, 2, "year")



cleanEx()
nameEx("method6")
### * method6

flush(stderr()); flush(stdout())

### Name: method6
### Title: Load estimation based on linear interpolation of concentration
### Aliases: method6
### Keywords: arit

### ** Examples

data("flow.data1", "conc.data1")
union<-db.union(flow.data1, conc.data1)
met6<-method6(union, 2)
met6.month<-method6(union, 2, "month")
met6.year<-method6(union, 2, "year")



cleanEx()
nameEx("monthly.mean")
### * monthly.mean

flush(stderr()); flush(stdout())

### Name: monthly.mean
### Title: Monthly mean of flow records not differentiated by year
### Aliases: monthly.mean
### Keywords: univar

### ** Examples

data("flow.data2")
mon<-monthly.mean(flow.data2)
mon.sd<-monthly.mean(flow.data2, "sd")



cleanEx()
nameEx("monthly.year.mean")
### * monthly.year.mean

flush(stderr()); flush(stdout())

### Name: monthly.year.mean
### Title: Monthly mean of flow records differentiated by year
### Aliases: monthly.year.mean
### Keywords: univar

### ** Examples

data("flow.data1")
mon<-monthly.mean(flow.data1)
mon.sd<-monthly.mean(flow.data1, "sd")



cleanEx()
nameEx("rating")
### * rating

flush(stderr()); flush(stdout())

### Name: rating
### Title: Load estimation with log log rating curve
### Aliases: rating
### Keywords: regression

### ** Examples

data("flow.data1","conc.data1")
union<-db.union(flow.data1, conc.data1)
CQregression(union,1)
reg<-rating(union, 2)
reg.month<-rating(union, 2, "month")
reg.year<-rating(union, 2, "year")



cleanEx()
nameEx("rating.period")
### * rating.period

flush(stderr()); flush(stdout())

### Name: rating.period
### Title: Load estimation with log log rating curve based on monthly or
###   annual relationship
### Aliases: rating.period
### Keywords: regression

### ** Examples

data("flow.data2","conc.data2")
union<-db.union(flow.data2, conc.data1)
reg.periodM<-rating.period(union, 1, "month")
reg.periodY<-rating.period(union, 1, "year")



cleanEx()
nameEx("reg.inspection")
### * reg.inspection

flush(stderr()); flush(stdout())

### Name: reg.inspection
### Title: Parameters of regression analysis between flow and concentration
### Aliases: reg.inspection
### Keywords: regression

### ** Examples

data("flow.data3", "conc.data3")
union<-db.union(flow.data3, conc.data3)
reg.parametrs<-reg.inspection(union, 1)



cleanEx()
nameEx("residual.plot")
### * residual.plot

flush(stderr()); flush(stdout())

### Name: residual.plot
### Title: Residual plots of one selected component
### Aliases: residual.plot
### Keywords: regression dplot

### ** Examples

data("flow.data1","conc.data1")
union<-db.union(flow.data1, conc.data1)
residual.plot(union, 1)



cleanEx()
nameEx("rsquared.period")
### * rsquared.period

flush(stderr()); flush(stdout())

### Name: rsquared.period
### Title: Coefficient of determination for period based regression
###   analyses
### Aliases: rsquared.period
### Keywords: regression

### ** Examples

data("flow.data3","conc.data3")
union<-db.union(flow.data3, conc.data3)
rsquared.period(union,1, "month")
rsquared.period(union,1, "year")



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
