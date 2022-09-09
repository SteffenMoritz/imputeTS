
# Changes in Version 3.3

Thanks to Sabrina Krys, Kevin Villalobos, Tracy Shen, hezhichao1991, englianhu for bug / issue reporting.
Thanks to RicardaP for fixing documentation error.
Thanks to Ronald Hause for the commit to optimize parameter pass trough from approx to na_interpolation.


* Renamed ggplot_na_intervals to ggplot_na_distribution2

* Updates to ggplot_na_gapsize: Space between the bars adjusted for better optics.
  Added parameters for directly choosing the bar border color and alpha value for
  filling of the bars.
  
* Improved notification message for na_seadec/na_seasplit when find_frequency couldn't find
  a seasonal pattern.

* Corrected error in na_kalman documentation - auto.arima was wrongly described as default parameter choice, while in reality it is StructTS 
  (reported by RicardaP)

* Changes for the error handling. (**These changes got reverted and did not make it into the CRAN release**). For some specific cases the input checks performed by imputeTS stop pipe workflows in their entirety. E.g. a problem when group_by leads to  all NA subsets - which fail the input check and then stop the whole pipe workflow. To prevent this, stop() is only called, when the user supplied imputeTS algorithm parameter options are wrong or misspelled. Unsupported input data will only give a warning() (and do not perform any action on the data). Thus, there is no call to stop(), that cancels the whole pipe workflow.  (issue reported by Sabrina Krys). This works fine, but after closer consideration we figured people fail to notice warnings way too often and thus it is more user friendly to clearly stop with an error for these issues. After all, the users data analysis clearly profits from taking a closer look in these specific cases. If you are anyhow interested in the version without the reverted changes, it can be installed from github with the following command: devtools::install_github("https://github.com/SteffenMoritz/imputeTS/commit/aaf759216b4091e36dee6e8e3a10185ff8f4647b")

* Improved error messages (especially for multivariate inputs) and unit tests for the warnings and errors.

* Corrected typo in 'Input data needs at least x non-NA data points' error message

*  Better parameter pass trough from approx to na_interpolation- Added capability to alter rule for linear extrapolation outside the interval [min(x), max(x)] 
   (commit by Ronald Hause)

* Improved na_interpolation documentation (more information about possible parameter pass 
  through from underlying spline, approx,stinterp functions)

* Additional unit tests

* Moved to Github Actions instead of TravisCI / AppVeyor.

* Bugfix for "Error in optim(init[mask], getLike, method = "L-BFGS-B", lower = rep(0, : L-BFGS-B needs finite values of 'fn'.", which comes for completely constant input to na_kalman e.g. 4,4,4,NA,4,4. (reported by Kevin Villalobos, Tracy Shen, hezhichao1991, englianhu)

* Improved na_seadec documentation (algorithm details)

* Changed R Version requirement in Description to R (>= 3.6) since imported packages like ggtext and also some testthat   tests were already requiring newer versions than the old R (≥ 3.0.1) requirement of imputeTS


# Changes in Version 3.2

Thanks to Mark J. Lamias for bug / issue reporting.
Thanks to Cyrus Mohammadian for bug reporting.
Thanks to Miroslaw Janik for issue reporting. 

* Fix to remove CRAN note - removed not used utils from DESCRIPTION imports

* Minor fix to ggplot_na_distribution (bars end now at max(timeseries)*1.05)

* Typo corrections in statsNA

* Specified ggplot2 (>= 3.3.0) in imports, to prevent errors with older ggplot2 versions (reported by Cyrus Mohammadian)

* Updated na_locf documentation to make behavior of na_remaining parameter more clear (issue reported by Mark J. Lamias)

* ggplot_na_intervals, has now percentages with % sign (e.g. 10%) on y-scale instead of just numbers (e.g. 0,1)
  (suggestion from Miroslaw Janik)
  
* Added some figures and the Cheat Sheet .pptx to .Rbuildignore to avoid CRAN warning about  
  package size. These files and figures were not needed for the CRAN version. 


# Changes in Version 3.1

Thanks to Johannes Menzel for bug reporting, Thanks to Jan (jmablans) for bug reporting. 
Thanks to Earo Wang for speedup of plotNA.gapsize.
Special Thanks to Sebastian Gatscha for plotting functions, new na_mean options, new unit tests.

* Plotting functions are all in ggplot now (way better looking). Additionally they got renamed accordingly ggplot_na_distribution, ggplot_na_intervals, ggplot_na_gapsize, ggplot_na_imputations.

* Speedup for plotNA_gapsize calculation (now renamed ggplot_na_gapsize)  (thx to Earo Wang) 
  
* Added harmonic and geometric mean as option for na_mean

* Removed bug in na_replace - it can now be used with all NA vectors since 
 it requires no minimum of non-NA values (reported by Jan - jmablans)
 
* Improved na.random input check (usable with all NA input now if upper and lower bound
  parameters are explicitly set to numeric values)

* Additional unit tests for the plotting functions

* Additional unit tests for the all imputation functions (testing all NA input)

* Update for testthat unit tests

* Fixed a mistake in README.md (reported by Johannes Menzel)

* Added to statsNA: Number of Gaps, Average Gap Size + reformatting of code + 
  compatibility with other ts objects

* Documentation improvements through newer roxygen version (Markup now possible in documentation)

* updated Readme + Vignette to new function names

* Added the imputeTS Cheat Sheet as Vignette

* Added new vignette Gallery Missing Data Visualizations

* Added revdep

# Changes in Version 3.0

Thanks to Jim Maas, shreydesai, Breza, CameronNemo for reporting bugs.
Thanks to  Sebastian Gatscha providing the (way faster) C++ na.ma() implementation.

 * tibble and tstibble compatibility
 
 * Reworked internal code documentation

 * na.ma speed up via C++ 

 * Changed vignette builder to R.rsp

 * Used R package styler package to optimize source code readability
 
 * Made some changes to better follow tidyverse style guide
 
 * Replaced na. with na_ e.g. na.mean with na_mean usw.This fits better to modern code
   style guidelines. The old function names will still work for a while,
   but give a warning.

 * Added findFrequency option to na.seadec and na.seasplit

 * Added maxgap option
 
 * Fixed bug for na.seadec - also imputed known values in some special cases
   (reported by CameronNemo)

 * Added doi: 10.32614/RJ-2017-009 to describtion, references, readme and citation file

 * Added StackExchange link to Readme

 * Moved stinepack from imports to suggested
 
 * Internal reorganization of imports - now always using pkg::function and importFrom pkg x1 x2 x3instead of just import pkg

* Fixed bug in na.ma when using xts time series with NA at the end

* Fixed error message in na.interpolation if wrong parameter is given       
   stop("Wrong parameter 'option' given. Value must be either 'linear', 'spline' or 'stine'.")
   (reported by Breza)

* Fixed spelling mistakes in na.seadec and na.seasplit (reported by shreydesai)

* Fixed bug with na.random() output (reported by Jim Maas)


# Changes in Version 2.7

  * Updated Description: Orcid Id added, packages required for unit test add as "Suggested"

  * Small correction in README.md, small update to citation file
  
  * Replaced NEWS with NEWS.md for better formatting
  

# Changes in Version 2.6

  * Updated citation file
  
  * Minor changes to vignette


# Changes in Version 2.5

  * Adjusted unit test to a update of forecast package
  
  

# Changes in Version 2.4

  * Small speed improvments for na.kalman
  
  * Improved input check for all functions

  * Bugfix for unit tests 

  * Changes to unit test (because of zoo update)
  

# Changes in Version 2.3

  * Bugfix for na.kalman with integer input
  
  * Readme Update

  * Improved error messages for na.seasplit and na.seadec

  * Minor vignette changes


# Changes in Version 2.2

  * Bugfix for na.locf (also concerned na.kalman) 
  

# Changes in Version 2.1

  * Fixed for problems with Solaris/Sparc
  
  * Fixes for problems with vignette on osx


# Changes in Version 2.0

  * Bugfix for plots without missing data
  
  * Increased performance for na.locf
  
  * Minor bugfixes for specific data.frame inputs
  
  * Minor bugfixes for specific xts object inputs

  * Improved Code Documentation
  
  * Added new software tests


# Changes in Version 1.9

  * Added Vignette


# Changes in Version 1.8

  * Computation time improvments for na.locf (up to 10000 times faster)
  
  * Computation time improvments for na.interpolation (up to 10000 times faster)
  
  * Computation time improvments for na.kalman (only slightly faster, under 10%)
  
  * Fixed unnecessary warning message with some na.kalman options
  
  * Adjusted default parameters for plotNA.distributionBar (using nclass.Sturges for breaks parameter)
  
  * Fixed issue with too sensitive input checking
  

# Changes in Version 1.7
  
  * Enabled usage of multivariate input (data.frame, mts, matrix,...) for all imputation 
    functions except na.remove. This means users do not have to loop through all columns 
    by themselfes anymore if they want to use the package with multivariate data. The
    imputation itself is still performend in univariate manner (column after column). 
  
  * Improved compatibility with different advanced time series objects like zoo and xts.
    Using the imputation functions with these time series objects should be possible now. 
    These series will not be explicitly named as possible input in the user documentation.
    Absence of errors can not be guaranteed. However, there are no known issues yet.
  
  * Added several things for unit tests with pkg 'testthat'
  
  * Added unit tests for every function
  
  * Adjusted error messages
  
  * Internal Coding style improvement: replaced all T with TRUE and all F with FALSE
  
  * Adjustment tsHeating / tsHeatingComplete datasets (set 1440 as frequency parameter)
  
  * Adjustment tsNH4 / tsNH4Complete datasets (set 144 as frequency parameter)
  
  * Fixes for grammar, spelling and citations in the whole documentation
  
  * Revised examples in the documentation for all functions
 
  * Restricted output of na.remove to vector only (issue with incorrect time information otherwise)
  
  * Added better x-axes labels for plotNA.distribution
  

# Changes in Version 1.6
  
  * Added github links to description file
  
  * Added citation file
  
  * Updated Readme (badges for travis ci and cran status)
  
  * Fix in documentation for na.interpolation 
   (due to outdated descriptions)
   
  * Fix in documentation plotNA.distribution / plotNA.distributionBar
    (due to interchanged descriptions)
    
  * Added references to used packages in na.kalman and na.interpolation documentation


# Changes in Version 1.5

  * Allows now also numeric vectors as input

  * Removed na.identifier parameter for all functions (too error prone, better handled 
    individually by the user)

  * Minor changes in na.interpolation with option = "stine"
  
  * Added na.ma imputation function

  * Replaced "data" in all function parameters with the more common "x"

  * Improvement of all code examples

  * Renamed heating/heatingComplete dataset to tsHeating/tsHeatingComplete

  * Renamed nh4/nh4Complete dataset to tsNH4/tsNH4Complete

  * Added tsAirgap / tsAirgapComplete datasets

  * Improved imputeTS-package documentation

  * Added na.kalman imputation function

  * Added README.md function

  * Added statsNA function

  * Added plotNA.gapsize function
  
  * Renamed vis.imputations to plotNA.imputations

  * Renamed vis.barMissing to plotNA.distributionBar

  * Renamed vis.missing to plotNA.distribution
  
  * Fixed issues with parameter pass through and legend for
    all plotting functions
    
  * Improved dataset documentation 


# Changes in Version 0.4

  * Update of vis.differences (better looking plot now)
  
  * Added vis.missing to visualize the distribution of missing data in a time series
  
  * Added vis.barMissing, which is especially suited to visualize missing data in very huge time series
  
  * Update na.interpolate (added Stineman interpolation and enabled ... parameter 
    for all interpolation algorithms to pass through parameters to the underlying functions)


# Changes in Version 0.3

  * Added two datasets of sensor data
  
  * vis.differences for plotting differences between real and imputed values
  

# Changes in Version 0.2

  * Removed internal functions from visible package documentation

  * Added additional algorithms: na.seasplit and na.seadec
  
  * internal function for algorithm selection
  

# Changes in Version 0.1

  * Created initial version of imputeTS package for univariate time series imputation

  * added the simple imputation functions: na.locf, na.mean, na.random, na.interpolation, 
    na.replace

  * added na.remove function for removing all NAs from a time series
