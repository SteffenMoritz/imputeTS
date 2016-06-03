[![Build](https://travis-ci.org/SteffenMoritz/imputeTS.svg?branch=master)](https://travis-ci.org/SteffenMoritz/imputeTS)
[![Coverage Status](https://coveralls.io/repos/github/SteffenMoritz/imputeTS/badge.svg?branch=master)](https://coveralls.io/github/SteffenMoritz/imputeTS?branch=master)
[![CRAN Version](http://www.r-pkg.org/badges/version/imputeTS)](https://cran.r-project.org/package=imputeTS)
[![Downloads](http://cranlogs.r-pkg.org/badges/imputeTS)](https://cran.r-project.org/package=imputeTS)


# imputeTS: Time Series Missing Value Imputation

The imputeTS package specializes on (univariate) time series imputation. 
It offers several different imputation algorithm implementations. Beyond the imputation algorithms the package also provides plotting and printing functions of time series missing data statistics. Additionally three time series datasets for imputation experiments are included.

### Installation

The imputeTS package can be found on [CRAN]. For installation execute in R:

```
 install.packages("imputeTS")
```

### Usage
- ###### Imputation

  To impute (fill all missing values) in a time series ***x***, run the following
  command:
  ```
   na.interpolation(x)
  ```
  Output is the time series ***x*** with all NA's replaced by reasonable values.

  > This is just one example for an imputation algorithm. 
  > In this case interpolation was the algorithm of choice for 
  > calculating the NA replacements. There are several  other 
  > algorithms (see also under caption "Imputation Algorithms"). All 
  > imputation functions   are named alike starting with
  > na. followed by a algorithm label  e.g. na.mean, na.kalman, ...


- ###### Plotting

  To plot missing data statistics for a time series ***x***, run the following
  command:
  ```
   plotNA.distribution(x)
  ```
  > This is also just one example for a plot. Overall there are four different types 
  > of missing data plots. (see also under caption "Missing Data Plots"). 

- ###### Printing

  To print statistics about the missing data in a time series ***x***, run the
  following command:
  ```
   statsNA(x)
  ```
  
- ###### Datasets

  To load the *'heating'* time series (with missing values)  into  a variable ***y*** and the *'heating'* time series (without missing values) into a variable ***z***, run: 
  ```
   y <- tsHeating
   z <- tsHeatingComplete
  ```
  > There are three datasets provided with the package, the *'tsHeating'*, the 
  > *'tsAirgap'* and the *'tsNH4'* time series. (see also under caption "Datasets"). 

### Imputation Algorithms

Here is a table with available algorithms to choose from:

|      Function        |           Description                                      | 
| :--------------------|:-----------------------------------------------------------|
| na.interpolation     |Missing Value Imputation by Interpolation                   |
| na.kalman            |Missing Value Imputation by Kalman Smoothing                |
| na.locf              |Missing Value Imputation by Last Observation Carried Forward|
| na.ma                |Missing Value Imputation by Weighted Moving Average         |
| na.mean              |Missing Value Imputation by Mean Value                      | 
| na.random            |Missing Value Imputation by Random Sample                   | 
| na.remove            |Remove Missing Values                                       | 
| na.replace           |Replace Missing Values by a Defined Value                   | 
| na.seadec            |Seasonally Decomposed Missing Value Imputation              | 
| na.seasplit          |Seasonally Splitted Missing Value Imputation                | 

  > This is a rather broad overview. The functions itself mostly offer more
  > than just one algorithm. For example na.interpolation can be set to linear
  > or spline interpolation.

More detailed information about the algortihms and their options can be found in the [imputeTS reference manual].

### Missing Data Plots

Here is a table with available plots to choose from:

|      Function           |           Description                                 | 
| :-----------------------|:------------------------------------------------------|
| plotNA.distribution     |Visualize Distribution of Missing Values               |
| plotNA.distributionBar  |Visualize Distribution of Missing Values (Barplot)     |
| plotNA.gapsize          |Visualize Distribution of NA gapsizes                  |
| plotNA.imputations      |Visualize Imputed Values                               | 

More detailed information about the plots can be found in the [imputeTS reference manual].

### Datasets

There are two datasets (each in two versions) available:

|      Dataset     |             Description                                          | 
| :----------------|:-----------------------------------------------------------------|
| tsAirgap         |Time series of monthly airline passengers (with NAs)              |
| tsAirgapComplete |Time series of monthly airline passengers (complete)   |
| tsHeating        |Time series of a heating systems supply temperature (with NAs)    |
| tsHeatingComplete|Time series of a heating systems supply temperature (complete)    |
| tsNH4            |Time series of NH4 concentration in a wastewater system (with NAs)|
| tsNH4Complete    |Time series of NH4 concentration in a wastewater system (complete)| 

  > The tsAirgap, tsHeating and tsNH4 time series are with NAs. Their **complete** versions are
  > without NAs. Except the missing values their versions are identical.
  > The NAs for the time series were artifically inserted by simulating the missing
  > data pattern observed in similar non-complete time series from the same domain.
  > Having a complete and incomplete version of the same dataset is useful for
  > conducting experiments of imputation functions.
 
  More detailed information about the datasets can be found in the [imputeTS reference manual].
  
### Support
If you found a bug or have suggestions, feel free to get in contact via steffen.moritz10 at gmail.com

> All feedback is welcome

### Version
**1.6**

### License
GPL (>= 2)


   [CRAN]: <https://cran.r-project.org/package=imputeTS>  
   [imputeTS reference manual]: <https://cran.r-project.org/package=imputeTS>
 