pkgname <- "naniar"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('naniar')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_any_miss")
### * add_any_miss

flush(stderr()); flush(stdout())

### Name: add_any_miss
### Title: Add a column describing presence of any missing values
### Aliases: add_any_miss

### ** Examples


airquality %>% add_any_miss()
airquality %>% add_any_miss(Ozone)
airquality %>% add_any_miss(Ozone, Solar.R)




cleanEx()
nameEx("add_label_missings")
### * add_label_missings

flush(stderr()); flush(stdout())

### Name: add_label_missings
### Title: Add a column describing if there are any missings in the dataset
### Aliases: add_label_missings

### ** Examples


airquality %>% add_label_missings()
airquality %>% add_label_missings(Ozone)
airquality %>% add_label_missings(Ozone, Solar.R)
airquality %>% add_label_missings(Ozone, Solar.R, missing = "yes", complete = "no")




cleanEx()
nameEx("add_label_shadow")
### * add_label_shadow

flush(stderr()); flush(stdout())

### Name: add_label_shadow
### Title: Add a column describing whether there is a shadow
### Aliases: add_label_shadow

### ** Examples


airquality %>%
  add_shadow(Ozone, Solar.R) %>%
  add_label_shadow()




cleanEx()
nameEx("add_miss_cluster")
### * add_miss_cluster

flush(stderr()); flush(stdout())

### Name: add_miss_cluster
### Title: Add a column that tells us which "missingness cluster" a row
###   belongs to
### Aliases: add_miss_cluster

### ** Examples


add_miss_cluster(airquality)
add_miss_cluster(airquality, cluster_method = "ward.D")
add_miss_cluster(airquality, cluster_method = "ward.D", n_clusters = 3)
add_miss_cluster(airquality, n_clusters = 3)



cleanEx()
nameEx("add_n_miss")
### * add_n_miss

flush(stderr()); flush(stdout())

### Name: add_n_miss
### Title: Add column containing number of missing data values
### Aliases: add_n_miss

### ** Examples


airquality %>% add_n_miss()
airquality %>% add_n_miss(Ozone, Solar.R)
airquality %>% add_n_miss(dplyr::contains("o"))





cleanEx()
nameEx("add_prop_miss")
### * add_prop_miss

flush(stderr()); flush(stdout())

### Name: add_prop_miss
### Title: Add column containing proportion of missing data values
### Aliases: add_prop_miss

### ** Examples


airquality %>% add_prop_miss()

airquality %>% add_prop_miss(Solar.R)

airquality %>% add_prop_miss(Solar.R, Ozone)

airquality %>% add_prop_miss(Solar.R, Ozone, label = "testing")

# this can be applied to model the proportion of missing data
# as in Tierney et al (doi: 10.1136/bmjopen-2014-007450)
library(rpart)
library(rpart.plot)

airquality %>%
add_prop_miss() %>%
rpart(prop_miss_all ~ ., data = ., model = TRUE) %>%
prp(type = 4,
    extra = 101,
    prefix = "prop_miss = ")



cleanEx()
nameEx("add_shadow")
### * add_shadow

flush(stderr()); flush(stdout())

### Name: add_shadow
### Title: Add a shadow column to dataframe
### Aliases: add_shadow

### ** Examples


airquality %>% add_shadow(Ozone)
airquality %>% add_shadow(Ozone, Solar.R)




cleanEx()
nameEx("add_shadow_shift")
### * add_shadow_shift

flush(stderr()); flush(stdout())

### Name: add_shadow_shift
### Title: Add a shadow shifted column to a dataset
### Aliases: add_shadow_shift

### ** Examples


pedestrian %>% add_shadow_shift(hourly_counts)

airquality %>% add_shadow_shift(Ozone, Solar.R)




cleanEx()
nameEx("add_span_counter")
### * add_span_counter

flush(stderr()); flush(stdout())

### Name: add_span_counter
### Title: Add a counter variable for a span of dataframe
### Aliases: add_span_counter

### ** Examples

## Not run: 
##D # add_span_counter(pedestrian, span_size = 100)
## End(Not run)



cleanEx()
nameEx("all-is-miss-complete")
### * all-is-miss-complete

flush(stderr()); flush(stdout())

### Name: all-is-miss-complete
### Title: Identify if all values are missing or complete
### Aliases: all-is-miss-complete all_na all_miss all_complete

### ** Examples


misses <- c(NA, NA, NA)
complete <- c(1, 2, 3)
mixture <- c(NA, 1, NA)

all_na(misses)
all_na(complete)
all_na(mixture)
all_complete(misses)
all_complete(complete)
all_complete(mixture)




cleanEx()
nameEx("any-na")
### * any-na

flush(stderr()); flush(stdout())

### Name: any-na
### Title: Identify if there are any missing or complete values
### Aliases: any-na any_na any_miss any_complete

### ** Examples


anyNA(airquality)
any_na(airquality)
any_miss(airquality)
any_complete(airquality)





cleanEx()
nameEx("as_shadow.data.frame")
### * as_shadow.data.frame

flush(stderr()); flush(stdout())

### Name: as_shadow.data.frame
### Title: Create shadow data
### Aliases: as_shadow.data.frame

### ** Examples


as_shadow(airquality)




cleanEx()
nameEx("as_shadow_upset")
### * as_shadow_upset

flush(stderr()); flush(stdout())

### Name: as_shadow_upset
### Title: Convert data into shadow format for doing an upset plot
### Aliases: as_shadow_upset

### ** Examples


## Not run: 
##D 
##D library(UpSetR)
##D airquality %>%
##D   as_shadow_upset() %>%
##D   upset()
## End(Not run)




cleanEx()
nameEx("bind_shadow")
### * bind_shadow

flush(stderr()); flush(stdout())

### Name: bind_shadow
### Title: Bind a shadow dataframe to original data
### Aliases: bind_shadow

### ** Examples


bind_shadow(airquality)

# bind only the variables that contain missing values
bind_shadow(airquality, only_miss = TRUE)

aq_shadow <- bind_shadow(airquality)

# explore missing data visually
library(ggplot2)

# using the bounded shadow to visualise Ozone according to whether Solar
# Radiation is missing or not.

ggplot(data = aq_shadow,
       aes(x = Ozone)) +
       geom_histogram() +
       facet_wrap(~Solar.R_NA,
       ncol = 1)




cleanEx()
nameEx("cast_shadow")
### * cast_shadow

flush(stderr()); flush(stdout())

### Name: cast_shadow
### Title: Add a shadow column to a dataset
### Aliases: cast_shadow

### ** Examples


airquality %>% cast_shadow(Ozone)
airquality %>% cast_shadow(Ozone, Solar.R)
library(ggplot2)
library(magrittr)
airquality  %>%
  cast_shadow(Ozone,Solar.R) %>%
  ggplot(aes(x = Ozone,
             colour = Solar.R_NA)) +
        geom_density()




cleanEx()
nameEx("cast_shadow_shift")
### * cast_shadow_shift

flush(stderr()); flush(stdout())

### Name: cast_shadow_shift
### Title: Add a shadow and a shadow_shift column to a dataset
### Aliases: cast_shadow_shift

### ** Examples


airquality %>% cast_shadow_shift(Ozone)
airquality %>% cast_shadow_shift(Ozone,Temp)

airquality %>% cast_shadow_shift(dplyr::contains("o"))




cleanEx()
nameEx("cast_shadow_shift_label")
### * cast_shadow_shift_label

flush(stderr()); flush(stdout())

### Name: cast_shadow_shift_label
### Title: Add a shadow column and a shadow shifted column to a dataset
### Aliases: cast_shadow_shift_label

### ** Examples


airquality %>% cast_shadow_shift_label(Ozone)
airquality %>% cast_shadow_shift_label(Ozone, Solar.R)

# replicate the plot generated by geom_miss_point()

library(ggplot2)

airquality %>%
  cast_shadow_shift_label(Ozone,Solar.R) %>%
  ggplot(aes(x = Ozone_shift,
             y = Solar.R_shift,
             colour = any_missing)) +
        geom_point()




cleanEx()
nameEx("common_na_numbers")
### * common_na_numbers

flush(stderr()); flush(stdout())

### Name: common_na_numbers
### Title: Common number values for NA
### Aliases: common_na_numbers
### Keywords: datasets

### ** Examples


dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                          1,   "A",   -100,
                          3,   "N/A", -99,
                          NA,  NA,    -98,
                          -99, "E",   -101,
                          -98, "F",   -1)

miss_scan_count(dat_ms, -99)
miss_scan_count(dat_ms, c(-99,-98))
miss_scan_count(dat_ms, c("-99","-98","N/A"))
common_na_numbers
miss_scan_count(dat_ms, common_na_numbers)



cleanEx()
nameEx("common_na_strings")
### * common_na_strings

flush(stderr()); flush(stdout())

### Name: common_na_strings
### Title: Common string values for NA
### Aliases: common_na_strings
### Keywords: datasets

### ** Examples


dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                          1,   "A",   -100,
                          3,   "N/A", -99,
                          NA,  NA,    -98,
                          -99, "E",   -101,
                          -98, "F",   -1)

miss_scan_count(dat_ms, -99)
miss_scan_count(dat_ms, c(-99,-98))
miss_scan_count(dat_ms, c("-99","-98","N/A"))
common_na_numbers
miss_scan_count(dat_ms, common_na_strings)



cleanEx()
nameEx("gather_shadow")
### * gather_shadow

flush(stderr()); flush(stdout())

### Name: gather_shadow
### Title: Long form representation of a shadow matrix
### Aliases: gather_shadow

### ** Examples


gather_shadow(airquality)




cleanEx()
nameEx("geom_miss_point")
### * geom_miss_point

flush(stderr()); flush(stdout())

### Name: geom_miss_point
### Title: geom_miss_point
### Aliases: geom_miss_point

### ** Examples


library(ggplot2)

# using regular geom_point()
ggplot(airquality,
       aes(x = Ozone,
           y = Solar.R)) +
geom_point()

# using  geom_miss_point()
ggplot(airquality,
       aes(x = Ozone,
           y = Solar.R)) +
 geom_miss_point()

 # using facets

ggplot(airquality,
       aes(x = Ozone,
           y = Solar.R)) +
 geom_miss_point() +
 facet_wrap(~Month)




cleanEx()
nameEx("gg_miss_case")
### * gg_miss_case

flush(stderr()); flush(stdout())

### Name: gg_miss_case
### Title: Plot the number of missings per case (row)
### Aliases: gg_miss_case

### ** Examples


gg_miss_case(airquality)
library(ggplot2)
gg_miss_case(airquality) + labs(x = "Number of Cases")
gg_miss_case(airquality, show_pct = TRUE)
gg_miss_case(airquality, order_cases = FALSE)
gg_miss_case(airquality, facet = Month)
gg_miss_case(airquality, facet = Month, order_cases = FALSE)
gg_miss_case(airquality, facet = Month, show_pct = TRUE)




cleanEx()
nameEx("gg_miss_case_cumsum")
### * gg_miss_case_cumsum

flush(stderr()); flush(stdout())

### Name: gg_miss_case_cumsum
### Title: Plot of cumulative sum of missing for cases
### Aliases: gg_miss_case_cumsum

### ** Examples


gg_miss_case_cumsum(airquality)
library(ggplot2)
gg_miss_case_cumsum(riskfactors, breaks = 50) + theme_bw()



cleanEx()
nameEx("gg_miss_fct")
### * gg_miss_fct

flush(stderr()); flush(stdout())

### Name: gg_miss_fct
### Title: Plot the number of missings for each variable, broken down by a
###   factor
### Aliases: gg_miss_fct

### ** Examples


gg_miss_fct(x = riskfactors, fct = marital)
library(ggplot2)
gg_miss_fct(x = riskfactors, fct = marital) + labs(title = "NA in Risk Factors and Marital status")





cleanEx()
nameEx("gg_miss_span")
### * gg_miss_span

flush(stderr()); flush(stdout())

### Name: gg_miss_span
### Title: Plot the number of missings in a given repeating span
### Aliases: gg_miss_span

### ** Examples


miss_var_span(pedestrian, hourly_counts, span_every = 3000)
library(ggplot2)
gg_miss_span(pedestrian, hourly_counts, span_every = 3000)
gg_miss_span(pedestrian, hourly_counts, span_every = 3000, facet = sensor_name)
# works with the rest of ggplot
gg_miss_span(pedestrian, hourly_counts, span_every = 3000) + labs(x = "custom")
gg_miss_span(pedestrian, hourly_counts, span_every = 3000) + theme_dark()

gg_miss_span(pedestrian, hourly_counts, span_every = 3000, facet = sensor_name)



cleanEx()
nameEx("gg_miss_upset")
### * gg_miss_upset

flush(stderr()); flush(stdout())

### Name: gg_miss_upset
### Title: Plot the pattern of missingness using an upset plot.
### Aliases: gg_miss_upset

### ** Examples


## Not run: 
##D gg_miss_upset(airquality)
##D gg_miss_upset(riskfactors)
##D gg_miss_upset(riskfactors, nsets = 10)
##D gg_miss_upset(riskfactors, nsets = 10, nintersects = 10)
## End(Not run)



cleanEx()
nameEx("gg_miss_var")
### * gg_miss_var

flush(stderr()); flush(stdout())

### Name: gg_miss_var
### Title: Plot the number of missings for each variable
### Aliases: gg_miss_var

### ** Examples


gg_miss_var(airquality)
library(ggplot2)
gg_miss_var(airquality) + labs(y = "Look at all the missing ones")
gg_miss_var(airquality, Month)
gg_miss_var(airquality, Month, show_pct = TRUE)
gg_miss_var(airquality, Month, show_pct = TRUE) + ylim(0, 100)




cleanEx()
nameEx("gg_miss_var_cumsum")
### * gg_miss_var_cumsum

flush(stderr()); flush(stdout())

### Name: gg_miss_var_cumsum
### Title: Plot of cumulative sum of missing value for each variable
### Aliases: gg_miss_var_cumsum

### ** Examples


gg_miss_var_cumsum(airquality)



cleanEx()
nameEx("gg_miss_which")
### * gg_miss_which

flush(stderr()); flush(stdout())

### Name: gg_miss_which
### Title: Plot which variables contain a missing value
### Aliases: gg_miss_which

### ** Examples


gg_miss_which(airquality)
library(ggplot2)




cleanEx()
nameEx("group_by_fun")
### * group_by_fun

flush(stderr()); flush(stdout())

### Name: group_by_fun
### Title: Group By Helper
### Aliases: group_by_fun

### ** Examples


## Not run: 
##D miss_case_table.grouped_df <- function(data){
##D group_by_fun(data,.fun = miss_case_table)
##D }
##D airquality %>%
##D group_by(Month) %>%
##D miss_case_table()
## End(Not run)




cleanEx()
nameEx("impute_below_all")
### * impute_below_all

flush(stderr()); flush(stdout())

### Name: impute_below_all
### Title: Impute data with values shifted 10 percent below range.
### Aliases: impute_below_all

### ** Examples


# you can impute data like so:
airquality %>%
  impute_below_all()

# However, this does not show you WHERE the missing values are.
# to keep track of them, you want to use `bind_shadow()` first.

airquality %>%
  bind_shadow() %>%
  impute_below_all()

# This identifies where the missing values are located, which means you
# can do things like this:

## Not run: 
##D library(ggplot2)
##D airquality %>%
##D   bind_shadow() %>%
##D   impute_below_all() %>%
##D   # identify where there are missings across rows.
##D   add_label_shadow() %>%
##D   ggplot(aes(x = Ozone,
##D              y = Solar.R,
##D              colour = any_missing)) +
##D   geom_point()
##D # Note that this ^^ is a long version of `geom_miss_point()`.
## End(Not run)




cleanEx()
nameEx("impute_below_at")
### * impute_below_at

flush(stderr()); flush(stdout())

### Name: impute_below_at
### Title: Scoped variants of 'impute_below'
### Aliases: impute_below_at

### ** Examples

# select variables starting with a particular string.
library(dplyr)
impute_below_at(airquality,
                .vars = c("Ozone", "Solar.R"))

impute_below_at(airquality,
                .vars = 1:2)
#'
impute_below_at(airquality,
                .vars = vars(Ozone))

## Not run: 
##D library(ggplot2)
##D airquality %>%
##D   bind_shadow() %>%
##D   impute_below_at(vars(Ozone, Solar.R)) %>%
##D   add_label_shadow() %>%
##D   ggplot(aes(x = Ozone,
##D              y = Solar.R,
##D              colour = any_missing)) +
##D          geom_point()
## End(Not run)




cleanEx()
nameEx("impute_below_if")
### * impute_below_if

flush(stderr()); flush(stdout())

### Name: impute_below_if
### Title: Scoped variants of 'impute_below'
### Aliases: impute_below_if

### ** Examples


airquality %>%
  impute_below_if(.predicate = is.numeric)




cleanEx()
nameEx("impute_mean")
### * impute_mean

flush(stderr()); flush(stdout())

### Name: impute_mean
### Title: Impute the mean value into a vector with missing values
### Aliases: impute_mean impute_mean.default impute_mean.factor

### ** Examples


vec <- rnorm(10)

vec[sample(1:10, 3)] <- NA

impute_mean(vec)




cleanEx()
nameEx("impute_median")
### * impute_median

flush(stderr()); flush(stdout())

### Name: impute_median
### Title: Impute the median value into a vector with missing values
### Aliases: impute_median impute_median.default impute_median.factor

### ** Examples


vec <- rnorm(10)

vec[sample(1:10, 3)] <- NA

impute_median(vec)




cleanEx()
nameEx("is_shade")
### * is_shade

flush(stderr()); flush(stdout())

### Name: is_shade
### Title: Detect if this is a shade
### Aliases: is_shade are_shade any_shade

### ** Examples


xs <- shade(c(NA, 1, 2, "3"))

is_shade(xs)
are_shade(xs)
any_shade(xs)

aq_s <- as_shadow(airquality)

is_shade(aq_s)
are_shade(aq_s)
any_shade(aq_s)
any_shade(airquality)





cleanEx()
nameEx("is_shadow")
### * is_shadow

flush(stderr()); flush(stdout())

### Name: is_shadow
### Title: Test if input is or are shadow variables
### Aliases: is_shadow is_nabular

### ** Examples


aq_sh <- as_shadow(airquality)
aq_bind <- bind_shadow(airquality)

is_shadow(aq_sh)
is_shadow(airquality)
is_shadow(aq_bind)
is_nabular(aq_bind)




cleanEx()
nameEx("label_miss_1d")
### * label_miss_1d

flush(stderr()); flush(stdout())

### Name: label_miss_1d
### Title: Label a missing from one column
### Aliases: label_miss_1d

### ** Examples


label_miss_1d(airquality$Ozone)




cleanEx()
nameEx("label_miss_2d")
### * label_miss_2d

flush(stderr()); flush(stdout())

### Name: label_miss_2d
### Title: label_miss_2d
### Aliases: label_miss_2d

### ** Examples


label_miss_2d(airquality$Ozone, airquality$Solar.R)




cleanEx()
nameEx("label_missings")
### * label_missings

flush(stderr()); flush(stdout())

### Name: label_missings
### Title: Is there a missing value in the row of a dataframe?
### Aliases: label_missings

### ** Examples


label_missings(airquality)

library(dplyr)

airquality %>%
  mutate(is_missing = label_missings(airquality)) %>%
  head()

airquality %>%
  mutate(is_missing = label_missings(airquality,
                                     missing = "definitely missing",
                                     complete = "absolutely complete")) %>%
  head()




cleanEx()
nameEx("miss_case_cumsum")
### * miss_case_cumsum

flush(stderr()); flush(stdout())

### Name: miss_case_cumsum
### Title: Summarise the missingness in each case
### Aliases: miss_case_cumsum

### ** Examples


miss_case_cumsum(airquality)

library(dplyr)

airquality %>%
  group_by(Month) %>%
  miss_case_cumsum()




cleanEx()
nameEx("miss_case_summary")
### * miss_case_summary

flush(stderr()); flush(stdout())

### Name: miss_case_summary
### Title: Summarise the missingness in each case
### Aliases: miss_case_summary

### ** Examples


# works with group_by from dplyr
library(dplyr)
airquality %>%
  group_by(Month) %>%
  miss_case_summary()

miss_case_summary(airquality)




cleanEx()
nameEx("miss_case_table")
### * miss_case_table

flush(stderr()); flush(stdout())

### Name: miss_case_table
### Title: Tabulate missings in cases.
### Aliases: miss_case_table

### ** Examples


miss_case_table(airquality)
library(dplyr)
airquality %>%
  group_by(Month) %>%
  miss_case_table()




cleanEx()
nameEx("miss_prop_summary")
### * miss_prop_summary

flush(stderr()); flush(stdout())

### Name: miss_prop_summary
### Title: Proportions of missings in data, variables, and cases.
### Aliases: miss_prop_summary

### ** Examples


miss_prop_summary(airquality)
library(dplyr)
airquality %>% group_by(Month) %>% miss_prop_summary()




cleanEx()
nameEx("miss_scan_count")
### * miss_scan_count

flush(stderr()); flush(stdout())

### Name: miss_scan_count
### Title: Search and present different kinds of missing values
### Aliases: miss_scan_count

### ** Examples


dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                         1,   "A",   -100,
                         3,   "N/A", -99,
                         NA,  NA,    -98,
                         -99, "E",   -101,
                         -98, "F",   -1)

miss_scan_count(dat_ms,-99)
miss_scan_count(dat_ms,c(-99,-98))
miss_scan_count(dat_ms,c("-99","-98","N/A"))
miss_scan_count(dat_ms,common_na_strings)




cleanEx()
nameEx("miss_summary")
### * miss_summary

flush(stderr()); flush(stdout())

### Name: miss_summary
### Title: Collate summary measures from naniar into one tibble
### Aliases: miss_summary

### ** Examples


s_miss <- miss_summary(airquality)
s_miss$miss_df_prop
s_miss$miss_case_table
s_miss$miss_var_summary
# etc, etc, etc.

library(dplyr)
s_miss_group <- group_by(airquality, Month) %>% miss_summary()
s_miss_group$miss_df_prop
s_miss_group$miss_case_table
# etc, etc, etc.





cleanEx()
nameEx("miss_var_cumsum")
### * miss_var_cumsum

flush(stderr()); flush(stdout())

### Name: miss_var_cumsum
### Title: Cumulative sum of the number of missings in each variable
### Aliases: miss_var_cumsum

### ** Examples


miss_var_cumsum(airquality)

library(dplyr)

# respects dplyr::group_by

airquality %>%
  group_by(Month) %>%
  miss_var_cumsum()




cleanEx()
nameEx("miss_var_run")
### * miss_var_run

flush(stderr()); flush(stdout())

### Name: miss_var_run
### Title: Find the number of missing and complete values in a single run
### Aliases: miss_var_run

### ** Examples


miss_var_run(pedestrian, hourly_counts)

library(dplyr)

# find the number of runs missing/complete for each month

pedestrian %>%
  group_by(month) %>%
  miss_var_run(hourly_counts)

library(ggplot2)

# explore the number of missings in a given run
miss_var_run(pedestrian, hourly_counts) %>%
  filter(is_na == "missing") %>%
  count(run_length) %>%
  ggplot(aes(x = run_length,
             y = n)) +
      geom_col()

# look at the number of missing values and the run length of these.
miss_var_run(pedestrian, hourly_counts) %>%
  ggplot(aes(x = is_na,
             y = run_length)) +
      geom_boxplot()

# using group_by
 pedestrian %>%
   group_by(month) %>%
   miss_var_run(hourly_counts)





cleanEx()
nameEx("miss_var_span")
### * miss_var_span

flush(stderr()); flush(stdout())

### Name: miss_var_span
### Title: Summarise the number of missings for a given repeating span on a
###   variable
### Aliases: miss_var_span

### ** Examples


miss_var_span(data = pedestrian,
             var = hourly_counts,
             span_every = 168)

 library(dplyr)
 pedestrian %>%
   group_by(month) %>%
     miss_var_span(var = hourly_counts,
                   span_every = 168)




cleanEx()
nameEx("miss_var_summary")
### * miss_var_summary

flush(stderr()); flush(stdout())

### Name: miss_var_summary
### Title: Summarise the missingness in each variable
### Aliases: miss_var_summary

### ** Examples


miss_var_summary(airquality)
miss_var_summary(oceanbuoys, order = TRUE)

# works with group_by from dplyr
library(dplyr)
airquality %>%
  group_by(Month) %>%
  miss_var_summary()




cleanEx()
nameEx("miss_var_table")
### * miss_var_table

flush(stderr()); flush(stdout())

### Name: miss_var_table
### Title: Tabulate the missings in the variables
### Aliases: miss_var_table

### ** Examples


miss_var_table(airquality)

library(dplyr)
airquality %>%
  group_by(Month) %>%
  miss_var_table()




cleanEx()
nameEx("miss_var_which")
### * miss_var_which

flush(stderr()); flush(stdout())

### Name: miss_var_which
### Title: Which variables contain missing values?
### Aliases: miss_var_which

### ** Examples

miss_var_which(airquality)

miss_var_which(iris)




cleanEx()
nameEx("n-var-case-complete")
### * n-var-case-complete

flush(stderr()); flush(stdout())

### Name: n-var-case-complete
### Title: The number of variables with complete values
### Aliases: n-var-case-complete n_var_complete n_case_complete

### ** Examples


# how many variables contain complete values?
n_var_complete(airquality)
n_case_complete(airquality)




cleanEx()
nameEx("n-var-case-miss")
### * n-var-case-miss

flush(stderr()); flush(stdout())

### Name: n-var-case-miss
### Title: The number of variables or cases with missing values
### Aliases: n-var-case-miss n_var_miss n_case_miss

### ** Examples

# how many variables contain missing values?
n_var_miss(airquality)
n_case_miss(airquality)




cleanEx()
nameEx("n_complete")
### * n_complete

flush(stderr()); flush(stdout())

### Name: n_complete
### Title: Return the number of complete values
### Aliases: n_complete

### ** Examples


n_complete(airquality)
n_complete(airquality$Ozone)




cleanEx()
nameEx("n_complete_row")
### * n_complete_row

flush(stderr()); flush(stdout())

### Name: n_complete_row
### Title: Return a vector of the number of complete values in each row
### Aliases: n_complete_row

### ** Examples


n_complete_row(airquality)




cleanEx()
nameEx("n_miss")
### * n_miss

flush(stderr()); flush(stdout())

### Name: n_miss
### Title: Return the number of missing values
### Aliases: n_miss

### ** Examples


n_miss(airquality)
n_miss(airquality$Ozone)




cleanEx()
nameEx("n_miss_row")
### * n_miss_row

flush(stderr()); flush(stdout())

### Name: n_miss_row
### Title: Return a vector of the number of missing values in each row
### Aliases: n_miss_row

### ** Examples


n_miss_row(airquality)




cleanEx()
nameEx("nabular")
### * nabular

flush(stderr()); flush(stdout())

### Name: nabular
### Title: Convert data into nabular form by binding shade to it
### Aliases: nabular

### ** Examples


aq_nab <- nabular(airquality)
aq_s <- bind_shadow(airquality)

all.equal(aq_nab, aq_s)




cleanEx()
nameEx("oceanbuoys")
### * oceanbuoys

flush(stderr()); flush(stdout())

### Name: oceanbuoys
### Title: West Pacific Tropical Atmosphere Ocean Data, 1993 & 1997.
### Aliases: oceanbuoys
### Keywords: datasets

### ** Examples


# explore the missingness with vis_miss
library(naniar)

vis_miss(oceanbuoys)

# Look at the missingness in the variables
miss_var_summary(oceanbuoys)

# Look at the missingness in air temperature and humidity
library(ggplot2)
p <-
ggplot(oceanbuoys,
       aes(x = air_temp_c,
           y = humidity)) +
     geom_miss_point()

 p

 # for each year?
 p + facet_wrap(~year)

 # this shows that there are more missing values in humidity in 1993, and
 # more air temperature missing values in 1997

 # what if we explore the value of air temperature and humidity based on
 # the missingness of each

 oceanbuoys %>%
   bind_shadow() %>%
   ggplot(aes(x = air_temp_c,
              fill = humidity_NA)) +
       geom_histogram()

 oceanbuoys %>%
   bind_shadow() %>%
   ggplot(aes(x = humidity,
              fill = air_temp_c_NA)) +
       geom_histogram()




cleanEx()
nameEx("pct-miss-complete-case")
### * pct-miss-complete-case

flush(stderr()); flush(stdout())

### Name: pct-miss-complete-case
### Title: Percentage of cases that contain a missing or complete values.
### Aliases: pct-miss-complete-case pct_miss_case pct_complete_case

### ** Examples


pct_miss_case(airquality)
pct_complete_case(airquality)




cleanEx()
nameEx("pct-miss-complete-var")
### * pct-miss-complete-var

flush(stderr()); flush(stdout())

### Name: pct-miss-complete-var
### Title: Percentage of variables containing missings or complete values
### Aliases: pct-miss-complete-var pct_miss_var pct_complete_var

### ** Examples


prop_miss_var(riskfactors)
prop_miss_var(oceanbuoys)
prop_complete_var(riskfactors)
prop_complete_var(oceanbuoys)




cleanEx()
nameEx("pct_complete")
### * pct_complete

flush(stderr()); flush(stdout())

### Name: pct_complete
### Title: Return the percent of complete values
### Aliases: pct_complete

### ** Examples


pct_complete(airquality)
pct_complete(airquality$Ozone)




cleanEx()
nameEx("pct_miss")
### * pct_miss

flush(stderr()); flush(stdout())

### Name: pct_miss
### Title: Return the percent of missing values
### Aliases: pct_miss

### ** Examples


pct_miss(airquality)
pct_miss(airquality$Ozone)




cleanEx()
nameEx("pedestrian")
### * pedestrian

flush(stderr()); flush(stdout())

### Name: pedestrian
### Title: Pedestrian count information around Melbourne for 2016
### Aliases: pedestrian
### Keywords: datasets

### ** Examples

## Not run: 
##D # explore the missingness with vis_miss
##D library(naniar)
##D 
##D vis_miss(pedestrian)
##D 
##D # Look at the missingness in the variables
##D miss_var_summary(pedestrian)
##D 
##D # There is only missingness in hourly_counts
##D # Look at the missingness over a rolling window
##D library(ggplot2)
##D gg_miss_span(pedestrian, hourly_counts, span_every = 3000)
## End(Not run)



cleanEx()
nameEx("prop-miss-complete-case")
### * prop-miss-complete-case

flush(stderr()); flush(stdout())

### Name: prop-miss-complete-case
### Title: Proportion of cases that contain a missing or complete values.
### Aliases: prop-miss-complete-case prop_miss_case prop_complete_case

### ** Examples


prop_miss_case(airquality)
prop_complete_case(airquality)




cleanEx()
nameEx("prop-miss-complete-var")
### * prop-miss-complete-var

flush(stderr()); flush(stdout())

### Name: prop-miss-complete-var
### Title: Proportion of variables containing missings or complete values
### Aliases: prop-miss-complete-var prop_miss_var prop_complete_var

### ** Examples


prop_miss_var(riskfactors)
prop_miss_var(oceanbuoys)
prop_complete_var(riskfactors)
prop_complete_var(oceanbuoys)




cleanEx()
nameEx("prop_complete")
### * prop_complete

flush(stderr()); flush(stdout())

### Name: prop_complete
### Title: Return the proportion of complete values
### Aliases: prop_complete

### ** Examples


prop_complete(airquality)
prop_complete(airquality$Ozone)




cleanEx()
nameEx("prop_complete_row")
### * prop_complete_row

flush(stderr()); flush(stdout())

### Name: prop_complete_row
### Title: Return a vector of the proportion of missing values in each row
### Aliases: prop_complete_row

### ** Examples


prop_complete_row(airquality)




cleanEx()
nameEx("prop_miss")
### * prop_miss

flush(stderr()); flush(stdout())

### Name: prop_miss
### Title: Return the proportion of missing values
### Aliases: prop_miss

### ** Examples


prop_miss(airquality)
prop_miss(airquality$Ozone)




cleanEx()
nameEx("prop_miss_row")
### * prop_miss_row

flush(stderr()); flush(stdout())

### Name: prop_miss_row
### Title: Return a vector of the proportion of missing values in each row
### Aliases: prop_miss_row

### ** Examples


prop_miss_row(airquality)




cleanEx()
nameEx("recode_shadow")
### * recode_shadow

flush(stderr()); flush(stdout())

### Name: recode_shadow
### Title: Add special missing values to the shadow matrix
### Aliases: recode_shadow

### ** Examples


## Not run: 
##D df <- tibble::tribble(
##D ~wind, ~temp,
##D -99,    45,
##D 68,    NA,
##D 72,    25
##D )
##D 
##D dfs <- bind_shadow(df)
##D 
##D dfs
##D 
##D recode_shadow(dfs, temp = .where(wind == -99 ~ "bananas"))
##D 
##D # need to debug this
##D 
##D recode_shadow(dfs,
##D               temp = .where(wind == -99 ~ "bananas")) %>%
##D recode_shadow(wind = .where(wind == -99 ~ "apples"))
## End(Not run)




cleanEx()
nameEx("replace_with_na")
### * replace_with_na

flush(stderr()); flush(stdout())

### Name: replace_with_na
### Title: Replace values with missings
### Aliases: replace_with_na

### ** Examples


dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                         1,   "A",   -100,
                         3,   "N/A", -99,
                         NA,  NA,    -98,
                         -99, "E",   -101,
                         -98, "F",   -1)

replace_with_na(dat_ms,
               replace = list(x = -99))

replace_with_na(dat_ms,
             replace = list(x = -98))

replace_with_na(dat_ms,
             replace = list(x = c(-99, -98)))

replace_with_na(dat_ms,
             replace = list(x = c(-99, -98),
                          y = c("N/A")))

replace_with_na(dat_ms,
             replace = list(x = c(-99, -98),
                          y = c("N/A"),
                          z = c(-101)))



cleanEx()
nameEx("replace_with_na_all")
### * replace_with_na_all

flush(stderr()); flush(stdout())

### Name: replace_with_na_all
### Title: Replace all values with NA where a certain condition is met
### Aliases: replace_with_na_all

### ** Examples

dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                          1,   "A",   -100,
                          3,   "N/A", -99,
                          NA,  NA,    -98,
                          -99, "E",   -101,
                          -98, "F",   -1)

dat_ms
#replace all instances of -99 with NA
replace_with_na_all(data = dat_ms,
                    condition = ~.x == -99)

# replace all instances of -98 with NA
replace_with_na_all(data = dat_ms,
                    condition = ~.x == -98)

# replace all instances of -99 or -98 with NA
replace_with_na_all(dat_ms,
                    condition = ~.x %in% c(-99, -98))

# replace all instances of -99 or -98, or "N/A" with NA
replace_with_na_all(dat_ms,
                    condition = ~.x %in% c(-99, -98, "N/A"))
# replace all instances of common na strings
replace_with_na_all(dat_ms,
                    condition = ~.x %in% common_na_strings)

# where works with functions
replace_with_na_all(airquality, ~ sqrt(.x) < 5)




cleanEx()
nameEx("replace_with_na_at")
### * replace_with_na_at

flush(stderr()); flush(stdout())

### Name: replace_with_na_at
### Title: Replace specified variables with NA where a certain condition is
###   met
### Aliases: replace_with_na_at

### ** Examples


dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                          1,   "A",   -100,
                          3,   "N/A", -99,
                          NA,  NA,    -98,
                          -99, "E",   -101,
                          -98, "F",   -1)

dat_ms

replace_with_na_at(data = dat_ms,
                 .vars = "x",
                 condition = ~.x == -99)

replace_with_na_at(data = dat_ms,
                 .vars = c("x","z"),
                 condition = ~.x == -99)

# replace using values in common_na_strings
replace_with_na_at(data = dat_ms,
                 .vars = c("x","z"),
                 condition = ~.x %in% common_na_strings)





cleanEx()
nameEx("replace_with_na_if")
### * replace_with_na_if

flush(stderr()); flush(stdout())

### Name: replace_with_na_if
### Title: Replace values with NA based on some condition, for variables
###   that meet some predicate
### Aliases: replace_with_na_if

### ** Examples


dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                          1,   "A",   -100,
                          3,   "N/A", -99,
                          NA,  NA,    -98,
                          -99, "E",   -101,
                          -98, "F",   -1)

dat_ms

replace_with_na_if(data = dat_ms,
                 .predicate = is.character,
                 condition = ~.x == "N/A")
replace_with_na_if(data = dat_ms,
                   .predicate = is.character,
                   condition = ~.x %in% common_na_strings)

replace_with_na(dat_ms,
              to_na = list(x = c(-99, -98),
                           y = c("N/A"),
                           z = c(-101)))





cleanEx()
nameEx("riskfactors")
### * riskfactors

flush(stderr()); flush(stdout())

### Name: riskfactors
### Title: The Behavioral Risk Factor Surveillance System (BRFSS) Survey
###   Data, 2009.
### Aliases: riskfactors
### Keywords: datasets

### ** Examples


# explore the missingness with vis_miss
library(naniar)

vis_miss(riskfactors)

# Look at the missingness in the variables
miss_var_summary(riskfactors)

# and now as a plot
gg_miss_var(riskfactors)

# Look at the missingness in bmi and poor health
library(ggplot2)
p <-
ggplot(riskfactors,
       aes(x = health_poor,
           y = bmi)) +
     geom_miss_point()

 p

 # for each sex?
 p + facet_wrap(~sex)
 # for each education bracket?
 p + facet_wrap(~education)




cleanEx()
nameEx("scoped-impute_mean")
### * scoped-impute_mean

flush(stderr()); flush(stdout())

### Name: scoped-impute_mean
### Title: Scoped variants of 'impute_mean'
### Aliases: scoped-impute_mean impute_mean_all impute_mean_at
###   impute_mean_if

### ** Examples

# select variables starting with a particular string.
library(dplyr)
impute_mean_all(airquality)

impute_mean_at(airquality,
               .vars = c("Ozone", "Solar.R"))

impute_mean_at(airquality,
                .vars = vars(Ozone))

impute_mean_if(airquality,
                .predicate = is.numeric)

## Not run: 
##D library(ggplot2)
##D airquality %>%
##D   bind_shadow() %>%
##D   impute_mean_all() %>%
##D   add_label_shadow() %>%
##D   ggplot(aes(x = Ozone,
##D              y = Solar.R,
##D              colour = any_missing)) +
##D          geom_point()
## End(Not run)




cleanEx()
nameEx("scoped-impute_median")
### * scoped-impute_median

flush(stderr()); flush(stdout())

### Name: scoped-impute_median
### Title: Scoped variants of 'impute_median'
### Aliases: scoped-impute_median impute_median_all impute_median_at
###   impute_median_if

### ** Examples

# select variables starting with a particular string.
library(dplyr)
impute_median_all(airquality)

impute_median_at(airquality,
               .vars = c("Ozone", "Solar.R"))

impute_median_at(airquality,
                .vars = vars(Ozone))

impute_median_if(airquality,
                .predicate = is.numeric)

## Not run: 
##D library(ggplot2)
##D airquality %>%
##D   bind_shadow() %>%
##D   impute_median_all() %>%
##D   add_label_shadow() %>%
##D   ggplot(aes(x = Ozone,
##D              y = Solar.R,
##D              colour = any_missing)) +
##D          geom_point()
## End(Not run)




cleanEx()
nameEx("shade")
### * shade

flush(stderr()); flush(stdout())

### Name: shade
### Title: Create new levels of missing
### Aliases: shade

### ** Examples

df <- tibble::tribble(
  ~wind, ~temp,
  -99,    45,
  68,    NA,
  72,    25
  )

shade(df$wind)

shade(df$wind,
      inst_fail = -99)

shade(df$wind,
      inst_fail = 100)




cleanEx()
nameEx("shadow_expand_relevel")
### * shadow_expand_relevel

flush(stderr()); flush(stdout())

### Name: shadow_expand_relevel
### Title: Expand and relevel a shadow column with a new suffix
### Aliases: shadow_expand_relevel

### ** Examples

## Not run: 
##D df <- tibble::tribble(
##D   ~wind, ~temp,
##D   -99,    45,
##D   68,    NA,
##D   72,    25
##D )
##D 
##D dfs <- bind_shadow(df)
##D 
##D test_shade <- dfs$wind_NA
##D 
##D shadow_expand_relevel(test_shade, "weee")
##D 
##D dfs %>%
##D   mutate(temp_NA = shadow_expand_relevel(temp_NA, "weee"))
##D 
##D 
##D # test that this breaks
##D shadow_expand_relevel(airquality, "weee")
## End(Not run)



cleanEx()
nameEx("shadow_long")
### * shadow_long

flush(stderr()); flush(stdout())

### Name: shadow_long
### Title: Reshape shadow data into a long format
### Aliases: shadow_long

### ** Examples


aq_shadow <- bind_shadow(airquality)

shadow_long(aq_shadow)

# then filter only on Ozone
shadow_long(aq_shadow, Ozone)

shadow_long(aq_shadow, Ozone, Solar.R)





cleanEx()
nameEx("shadow_shift")
### * shadow_shift

flush(stderr()); flush(stdout())

### Name: shadow_shift
### Title: Shift missing values to facilitate missing data
###   exploration/visualisation
### Aliases: shadow_shift

### ** Examples

airquality$Ozone
shadow_shift(airquality$Ozone)
library(dplyr)
airquality %>%
    mutate(Ozone_shift = shadow_shift(Ozone))




cleanEx()
nameEx("test_if_dataframe")
### * test_if_dataframe

flush(stderr()); flush(stdout())

### Name: test_if_dataframe
### Title: Test if input is a data.frame
### Aliases: test_if_dataframe

### ** Examples

## Not run: 
##D # success
##D test_if_dataframe(airquality)
##D #fail
##D my_test <- matrix(10)
##D test_if_dataframe(my_test)
## End(Not run)




cleanEx()
nameEx("test_if_missing")
### * test_if_missing

flush(stderr()); flush(stdout())

### Name: test_if_missing
### Title: Test if the input is Missing
### Aliases: test_if_missing

### ** Examples

## Not run: 
##D # success
##D my_test <- x
##D test_if_null(my_test)
##D #fail
##D test_if_missing()
## End(Not run)



cleanEx()
nameEx("test_if_null")
### * test_if_null

flush(stderr()); flush(stdout())

### Name: test_if_null
### Title: Test if the input is NULL
### Aliases: test_if_null

### ** Examples

## Not run: 
##D # success
##D test_if_null(airquality)
##D #fail
##D my_test <- NULL
##D test_if_null(my_test)
## End(Not run)



cleanEx()
nameEx("unbinders")
### * unbinders

flush(stderr()); flush(stdout())

### Name: unbinders
### Title: Unbind (remove) shadow from data, and vice versa
### Aliases: unbinders unbind_shadow unbind_data

### ** Examples


# bind shadow columns
aq_sh <- bind_shadow(airquality)

# print data
aq_sh

# remove shadow columns
unbind_shadow(aq_sh)

# remove data
unbind_data(aq_sh)

# errors when you don't use data with shadows
## Not run: 
##D  unbind_data(airquality)
##D  unbind_shadow(airquality)
## End(Not run)




cleanEx()
nameEx("update_shadow")
### * update_shadow

flush(stderr()); flush(stdout())

### Name: update_shadow
### Title: Expand all shadow levels
### Aliases: update_shadow

### ** Examples

 ## Not run: 
##D df <- tibble::tribble(
##D ~wind, ~temp,
##D -99,    45,
##D 68,    NA,
##D 72,    25
##D )
##D 
##D 
##D dfs <- bind_shadow(df)
##D 
##D update_shadow(dfs, "weee")
##D update_shadow(dfs, "weee") %>% what_levels()
## End(Not run)




cleanEx()
nameEx("where")
### * where

flush(stderr()); flush(stdout())

### Name: where
### Title: Split a call into two components with a useful verb name
### Aliases: where .where

### ** Examples


## Not run: 
##D df <- tibble::tribble(
##D ~wind, ~temp,
##D -99,    45,
##D 68,    NA,
##D 72,    25
##D )
##D 
##D dfs <- bind_shadow(df)
##D 
##D recode_shadow(dfs,
##D               temp = .where(wind == -99 ~ "bananas"))
##D 
## End(Not run)




cleanEx()
nameEx("where_na")
### * where_na

flush(stderr()); flush(stdout())

### Name: where_na
### Title: Which rows and cols contain missings?
### Aliases: where_na

### ** Examples


where_na(airquality)
where_na(oceanbuoys$sea_temp_c)




cleanEx()
nameEx("which_are_shade")
### * which_are_shade

flush(stderr()); flush(stdout())

### Name: which_are_shade
### Title: Which variables are shades?
### Aliases: which_are_shade

### ** Examples


df_shadow <- bind_shadow(airquality)

which_are_shade(df_shadow)




cleanEx()
nameEx("which_na")
### * which_na

flush(stderr()); flush(stdout())

### Name: which_na
### Title: Which elements contain missings?
### Aliases: which_na

### ** Examples


which_na(airquality)




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
