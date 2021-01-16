pkgname <- "specmine"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('specmine')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("aggregate_samples")
### * aggregate_samples

flush(stderr()); flush(stdout())

### Name: aggregate_samples
### Title: Aggregate samples
### Aliases: aggregate_samples
### Keywords: aggregation sample

### ** Examples

  ## Example of aggregating samples
  library(specmine.datasets)
  data(propolis)
  propolis_proc = missingvalues_imputation(propolis)
  dataset = aggregate_samples(propolis_proc, as.integer(propolis$metadata$seasons), "mean") 



cleanEx()
nameEx("aov_all_vars")
### * aov_all_vars

flush(stderr()); flush(stdout())

### Name: aov_all_vars
### Title: Analysis of variance
### Aliases: aov_all_vars
### Keywords: anova tukey

### ** Examples

  ## Example of ANOVA with TukeyHSD
  library(specmine.datasets)
  data(propolis)
  propolis_proc = missingvalues_imputation(propolis)
  propolis_proc = flat_pattern_filter(propolis_proc, "iqr", by.percent = TRUE,
	red.value = 75) 
  result = aov_all_vars(propolis_proc, "seasons", doTukey = FALSE)



cleanEx()
nameEx("apply_by_group")
### * apply_by_group

flush(stderr()); flush(stdout())

### Name: apply_by_group
### Title: Apply by group
### Aliases: apply_by_group
### Keywords: apply group

### ** Examples

     ## Example of applying a function to a group
	 library(specmine.datasets)
     data(cachexia)
     apply.group.result = apply_by_group(cachexia, mean, "Muscle.loss", 
			  "control")



cleanEx()
nameEx("apply_by_groups")
### * apply_by_groups

flush(stderr()); flush(stdout())

### Name: apply_by_groups
### Title: Apply by groups
### Aliases: apply_by_groups
### Keywords: groups apply

### ** Examples

     ## Example of applying a function to groups
	 library(specmine.datasets)
     data(cachexia)
     apply.groups.result = apply_by_groups(cachexia, "Muscle.loss", mean)



cleanEx()
nameEx("apply_by_sample")
### * apply_by_sample

flush(stderr()); flush(stdout())

### Name: apply_by_sample
### Title: Apply function to samples
### Aliases: apply_by_sample
### Keywords: apply sample

### ** Examples

     ## Example of applying a function to variables
	 library(specmine.datasets)
     data(cachexia)
     apply.samples.result = apply_by_sample(cachexia, mean)



cleanEx()
nameEx("apply_by_variable")
### * apply_by_variable

flush(stderr()); flush(stdout())

### Name: apply_by_variable
### Title: Apply function to variables
### Aliases: apply_by_variable
### Keywords: apply variable

### ** Examples

     ## Example of applying a function to variables
	 library(specmine.datasets)
     data(cachexia)
     apply.variables.result = apply_by_variable(cachexia, mean)



cleanEx()
nameEx("background_correction")
### * background_correction

flush(stderr()); flush(stdout())

### Name: background_correction
### Title: Background correction
### Aliases: background_correction
### Keywords: background correction

### ** Examples

  ## Example of background correction
  library(specmine.datasets)
  data(cachexia)
  cachexia.corrected = background_correction(cachexia)



cleanEx()
nameEx("boxplot_variables")
### * boxplot_variables

flush(stderr()); flush(stdout())

### Name: boxplot_variables
### Title: Boxplot of variables
### Aliases: boxplot_variables
### Keywords: boxplot variables

### ** Examples

  ## Example of showing the boxplot of a few variables
  library(specmine.datasets)
  data(cachexia)
  boxplot_variables(cachexia, variables = c("Creatine","Serine","Lactate"))



cleanEx()
nameEx("boxplot_vars_factor")
### * boxplot_vars_factor

flush(stderr()); flush(stdout())

### Name: boxplot_vars_factor
### Title: Boxplot of variables with metadata's variable factors
### Aliases: boxplot_vars_factor
### Keywords: ~boxplot ~factor

### ** Examples

  ## Example of showing the boxplot factors of a few variables
  library(specmine.datasets)
  data(cachexia)
  boxplot_vars_factor(cachexia, "Muscle.loss", variables = c("Creatine","Serine",
  "Lactate"))



cleanEx()
nameEx("check_dataset")
### * check_dataset

flush(stderr()); flush(stdout())

### Name: check_dataset
### Title: Check dataset
### Aliases: check_dataset
### Keywords: dataset

### ** Examples

  ## Example checking a dataset
  library(specmine.datasets)
  data(cachexia)
  check_dataset(cachexia)



cleanEx()
nameEx("clustering")
### * clustering

flush(stderr()); flush(stdout())

### Name: clustering
### Title: Perform cluster analysis
### Aliases: clustering
### Keywords: clustering kmeans hclust

### ** Examples

  ## Example of kmeans and hierarchical clustering
  library(specmine.datasets)
  data(cachexia)
  hc.result = clustering(cachexia, method = "hc", 
	      distance = "euclidean")
  kmeans.result = clustering(cachexia, method = "kmeans", 
		  num.clusters = 4)



cleanEx()
nameEx("compare_regions_by_sample")
### * compare_regions_by_sample

flush(stderr()); flush(stdout())

### Name: compare_regions_by_sample
### Title: Compare regions by sample
### Aliases: compare_regions_by_sample
### Keywords: region sample

### ** Examples

     ## Example of comparing regions by sample
	 library(specmine.datasets)
     data(cachexia)
     subset1 = subset_x_values(cachexia, 1:31, by.index = TRUE)
     subset2 = subset_x_values(cachexia, 32:63, by.index = TRUE)
     comp.regions.result = compare_regions_by_sample(subset1, subset2, 
			   mean)



cleanEx()
nameEx("convert_chebi_to_kegg")
### * convert_chebi_to_kegg

flush(stderr()); flush(stdout())

### Name: convert_chebi_to_kegg
### Title: Convert CHEBI codes to KEGG codes.
### Aliases: convert_chebi_to_kegg

### ** Examples

keggs=convert_hmdb_to_kegg(c("CHEBI:15377", "CHEBI:26078", "CHEBI:30168"))
keggs



cleanEx()
nameEx("convert_hmdb_to_kegg")
### * convert_hmdb_to_kegg

flush(stderr()); flush(stdout())

### Name: convert_hmdb_to_kegg
### Title: Convert HMDB codes to KEGG codes.
### Aliases: convert_hmdb_to_kegg

### ** Examples

keggs=convert_hmdb_to_kegg(c("HMDB0000001", "HMDB0000008", "HMDB0000246"))
keggs



cleanEx()
nameEx("convert_multiple_spcmnm_to_kegg")
### * convert_multiple_spcmnm_to_kegg

flush(stderr()); flush(stdout())

### Name: convert_multiple_spcmnm_to_kegg
### Title: Convert specmine metabolite codes to KEGG codes.
### Aliases: convert_multiple_spcmnm_to_kegg

### ** Examples

keggs=convert_multiple_spcmnm_to_kegg(c("SPCMNM2111", "SPCMNM2142", "SPCMNM069774"))
keggs



cleanEx()
nameEx("correlation_test")
### * correlation_test

flush(stderr()); flush(stdout())

### Name: correlation_test
### Title: Correlation test of two variables or samples
### Aliases: correlation_test
### Keywords: correlation test

### ** Examples

  ## Example of correlations test of variables
  library(specmine.datasets)
  data(cachexia)
  corr.result = correlation_test(cachexia, "Serine", "Creatine", method = "pearson", 
		by.var = TRUE)



cleanEx()
nameEx("correlations_dataset")
### * correlations_dataset

flush(stderr()); flush(stdout())

### Name: correlations_dataset
### Title: Dataset correlations
### Aliases: correlations_dataset
### Keywords: correlation spearman kendall pearson

### ** Examples

  ## Example of correlations of variables
  library(specmine.datasets)
  data(cachexia)
  corr.result = correlations_dataset(cachexia, 
		method = "pearson", by.var = TRUE)



cleanEx()
nameEx("correlations_test")
### * correlations_test

flush(stderr()); flush(stdout())

### Name: correlations_test
### Title: Correlations test
### Aliases: correlations_test
### Keywords: correlation test

### ** Examples




cleanEx()
nameEx("count_missing_values")
### * count_missing_values

flush(stderr()); flush(stdout())

### Name: count_missing_values
### Title: Count missing values
### Aliases: count_missing_values
### Keywords: missing values

### ** Examples

  ## Example of counting the missing values
  library(specmine.datasets)
  data(cachexia)
  count_missing_values(cachexia)



cleanEx()
nameEx("count_missing_values_per_sample")
### * count_missing_values_per_sample

flush(stderr()); flush(stdout())

### Name: count_missing_values_per_sample
### Title: Count missing values per sample
### Aliases: count_missing_values_per_sample
### Keywords: missing values

### ** Examples

  ## Example of counting the missing values on each sample
  library(specmine.datasets)
  data(cachexia)
  cachexia$data[10,10] = NA
  count_missing_values_per_sample(cachexia)



cleanEx()
nameEx("count_missing_values_per_variable")
### * count_missing_values_per_variable

flush(stderr()); flush(stdout())

### Name: count_missing_values_per_variable
### Title: Count missing values per variable
### Aliases: count_missing_values_per_variable
### Keywords: missing values

### ** Examples

  ## Example of counting the missing values on each variable
  library(specmine.datasets)
  data(cachexia)
  cachexia$data[10,10] = NA
  count_missing_values_per_variable(cachexia)



cleanEx()
nameEx("cubic_root_transform")
### * cubic_root_transform

flush(stderr()); flush(stdout())

### Name: cubic_root_transform
### Title: Cubic root transformation
### Aliases: cubic_root_transform
### Keywords: cubicroot transformation

### ** Examples

  ## Example of cubic root transformation
  library(specmine.datasets)
  data(cachexia)
  datamat.cubic = cubic_root_transform(cachexia$data)



cleanEx()
nameEx("dataset_from_peaks")
### * dataset_from_peaks

flush(stderr()); flush(stdout())

### Name: dataset_from_peaks
### Title: Dataset from peaks
### Aliases: dataset_from_peaks
### Keywords: peaklist dataset

### ** Examples




cleanEx()
nameEx("dendrogram_plot")
### * dendrogram_plot

flush(stderr()); flush(stdout())

### Name: dendrogram_plot
### Title: Plot dendrogram
### Aliases: dendrogram_plot
### Keywords: clustering dendrogram hclust

### ** Examples




cleanEx()
nameEx("dendrogram_plot_col")
### * dendrogram_plot_col

flush(stderr()); flush(stdout())

### Name: dendrogram_plot_col
### Title: Plot dendrogram
### Aliases: dendrogram_plot_col
### Keywords: clustering dendrogram hclust

### ** Examples

  ## Example of colored dendrogram
  library(specmine.datasets)
  data(cachexia)
  hc.result = hierarchical_clustering(cachexia)
  dendrogram_plot_col(cachexia, hc.result, "Muscle.loss", 
  title = "Example")



cleanEx()
nameEx("feature_selection")
### * feature_selection

flush(stderr()); flush(stdout())

### Name: feature_selection
### Title: Perform feature selection
### Aliases: feature_selection
### Keywords: featureselection filters wrappers

### ** Examples




cleanEx()
nameEx("filter_feature_selection")
### * filter_feature_selection

flush(stderr()); flush(stdout())

### Name: filter_feature_selection
### Title: Perform selection by filter
### Aliases: filter_feature_selection
### Keywords: sbf filters

### ** Examples




cleanEx()
nameEx("find_equal_samples")
### * find_equal_samples

flush(stderr()); flush(stdout())

### Name: find_equal_samples
### Title: Find equal samples
### Aliases: find_equal_samples
### Keywords: equal sample

### ** Examples

    ## Example of finding equal samples
	library(specmine.datasets)
    data(propolisSampleList)
    equal.samples = find_equal_samples(propolisSampleList)



cleanEx()
nameEx("flat_pattern_filter")
### * flat_pattern_filter

flush(stderr()); flush(stdout())

### Name: flat_pattern_filter
### Title: Flat pattern filter
### Aliases: flat_pattern_filter
### Keywords: flatpattern filter

### ** Examples

  ## Example of flat pattern filter
  library(specmine.datasets)
  data(propolis)
  dataset.filtered = flat_pattern_filter(propolis, "iqr", by.percent = TRUE, 
		     red.value = 20)



cleanEx()
nameEx("fold_change")
### * fold_change

flush(stderr()); flush(stdout())

### Name: fold_change
### Title: Fold change analysis
### Aliases: fold_change
### Keywords: foldchange

### ** Examples

  ## Example of fold change
  library(specmine.datasets)
  data(cachexia)
  fold.change.results = fold_change(cachexia, "Muscle.loss", 
			"control", write.file = FALSE)



cleanEx()
nameEx("fold_change_var")
### * fold_change_var

flush(stderr()); flush(stdout())

### Name: fold_change_var
### Title: Fold change applied on two variables
### Aliases: fold_change_var
### Keywords: foldchange reverse

### ** Examples

  ## Example of fold change reverse
  library(specmine.datasets)
  data(cachexia)
  fold.change.results = fold_change_var(cachexia, "Muscle.loss", 
			c("Creatine", "Serine"))



cleanEx()
nameEx("get_OrganismsCodes")
### * get_OrganismsCodes

flush(stderr()); flush(stdout())

### Name: get_OrganismsCodes
### Title: Get all organisms in KEGG.
### Aliases: get_OrganismsCodes

### ** Examples




cleanEx()
nameEx("get_cpd_names")
### * get_cpd_names

flush(stderr()); flush(stdout())

### Name: get_cpd_names
### Title: Get the names of the compounds that correspond to the kegg codes
###   given.
### Aliases: get_cpd_names

### ** Examples




cleanEx()
nameEx("get_data")
### * get_data

flush(stderr()); flush(stdout())

### Name: get_data
### Title: Get data
### Aliases: get_data
### Keywords: matrix dataset

### ** Examples

  ## Example of getting the data matrix
  library(specmine.datasets)
  data(cachexia)
  cachexia.dm = get_data(cachexia)



cleanEx()
nameEx("get_data_as_df")
### * get_data_as_df

flush(stderr()); flush(stdout())

### Name: get_data_as_df
### Title: Get data as data frame
### Aliases: get_data_as_df
### Keywords: matrix dataframe

### ** Examples

  ## Example of getting the data matrix as data frame
  library(specmine.datasets)
  data(cachexia)
  cachexia.dt = get_data_as_df(cachexia)



cleanEx()
nameEx("get_data_value")
### * get_data_value

flush(stderr()); flush(stdout())

### Name: get_data_value
### Title: Get data value
### Aliases: get_data_value
### Keywords: dataset value

### ** Examples

  ## Example of getting a data value from the dataset
  library(specmine.datasets)
  data(cachexia)
  data.value = get_data_value(cachexia, "Creatine", "PIF_178", 
	       by.index = FALSE)



cleanEx()
nameEx("get_data_values")
### * get_data_values

flush(stderr()); flush(stdout())

### Name: get_data_values
### Title: Get data values
### Aliases: get_data_values
### Keywords: data values

### ** Examples

  ## Example of getting a metadata value
  library(specmine.datasets)
  data(cachexia)
  data.values = get_data_values(cachexia, c("Creatine","Serine","Lactate"), 
		by.index = FALSE)



cleanEx()
nameEx("get_files_list_assay")
### * get_files_list_assay

flush(stderr()); flush(stdout())

### Name: get_files_list_per_assay
### Title: Get list of files per assay for MetaboLights study.
### Aliases: get_files_list_per_assay

### ** Examples




cleanEx()
nameEx("get_metabolights_study_files_assay")
### * get_metabolights_study_files_assay

flush(stderr()); flush(stdout())

### Name: get_metabolights_study_files_assay
### Title: Download data files from an assay of MetaboLights study
### Aliases: get_metabolights_study_files_assay

### ** Examples




cleanEx()
nameEx("get_metabolights_study_metadata_assay")
### * get_metabolights_study_metadata_assay

flush(stderr()); flush(stdout())

### Name: get_metabolights_study_metadata_assay
### Title: Download metadata file from an assay of MetaboLights study
### Aliases: get_metabolights_study_metadata_assay

### ** Examples




cleanEx()
nameEx("get_metabolights_study_samples_files")
### * get_metabolights_study_samples_files

flush(stderr()); flush(stdout())

### Name: get_metabolights_study_samples_files
### Title: Get list of files from an assay of the MetaboLights study and
###   saves it in a csv file.
### Aliases: get_metabolights_study_samples_files

### ** Examples




cleanEx()
nameEx("get_metadata")
### * get_metadata

flush(stderr()); flush(stdout())

### Name: get_metadata
### Title: Get metadata
### Aliases: get_metadata
### Keywords: metadata dataset

### ** Examples

  ## Example of getting the metadata
  library(specmine.datasets)
  data(cachexia)
  cachexia.mt = get_metadata(cachexia)



cleanEx()
nameEx("get_metadata_value")
### * get_metadata_value

flush(stderr()); flush(stdout())

### Name: get_metadata_value
### Title: Get metadata value
### Aliases: get_metadata_value
### Keywords: metadata value

### ** Examples

  ## Example of getting a metadata value
  library(specmine.datasets)
  data(cachexia)
  metadata.value = get_metadata_value(cachexia, "Muscle.loss", "PIF_178")



cleanEx()
nameEx("get_metadata_var")
### * get_metadata_var

flush(stderr()); flush(stdout())

### Name: get_metadata_var
### Title: Get metadata variable
### Aliases: get_metadata_var
### Keywords: metadata variable

### ** Examples

  ## Example of getting a metadata variable
  library(specmine.datasets)
  data(cachexia)
  metadata.variable = get_metadata_var(cachexia, "Muscle.loss")



cleanEx()
nameEx("get_paths_with_cpds_org")
### * get_paths_with_cpds_org

flush(stderr()); flush(stdout())

### Name: get_paths_with_cpds_org
### Title: Get only the paths of the organism that contain one or more of
###   the given compounds.
### Aliases: get_paths_with_cpds_org

### ** Examples




cleanEx()
nameEx("get_peak_values")
### * get_peak_values

flush(stderr()); flush(stdout())

### Name: get_peak_values
### Title: Get peak values
### Aliases: get_peak_values
### Keywords: peak sample

### ** Examples

  ## Example of getting the peak values
  library(specmine.datasets)
  data(propolis)
  peak.values = get_peak_values(propolis$data, 2.11)



cleanEx()
nameEx("get_sample_names")
### * get_sample_names

flush(stderr()); flush(stdout())

### Name: get_sample_names
### Title: Get sample names
### Aliases: get_sample_names
### Keywords: sample dataset

### ** Examples

  ## Example of getting the sample names
  library(specmine.datasets)
  data(cachexia)
  sample.names = get_sample_names(cachexia)



cleanEx()
nameEx("get_type")
### * get_type

flush(stderr()); flush(stdout())

### Name: get_type
### Title: Get type of data
### Aliases: get_type
### Keywords: type dataset

### ** Examples

  ## Example of getting the type of the data
  library(specmine.datasets)
  data(cachexia)
  type = get_type(cachexia)



cleanEx()
nameEx("get_value_label")
### * get_value_label

flush(stderr()); flush(stdout())

### Name: get_value_label
### Title: Get value label
### Aliases: get_value_label
### Keywords: label dataset

### ** Examples

  ## Example of getting the value label
  library(specmine.datasets)
  data(cassavaPPD)
  value.label = get_value_label(propolis)



cleanEx()
nameEx("get_x_label")
### * get_x_label

flush(stderr()); flush(stdout())

### Name: get_x_label
### Title: Get x-axis label
### Aliases: get_x_label
### Keywords: label xaxis

### ** Examples

  ## Example of getting the x-axis label
  library(specmine.datasets)
  data(cassavaPPD)
  x.label = get_x_label(propolis)



cleanEx()
nameEx("get_x_values_as_num")
### * get_x_values_as_num

flush(stderr()); flush(stdout())

### Name: get_x_values_as_num
### Title: Get x-axis values as numbers
### Aliases: get_x_values_as_num
### Keywords: dataset xaxis

### ** Examples

  ## Example of getting the x-axis values as numbers
  library(specmine.datasets)
  data(propolis)
  xvalues.numeric = get_x_values_as_num(propolis)



cleanEx()
nameEx("get_x_values_as_text")
### * get_x_values_as_text

flush(stderr()); flush(stdout())

### Name: get_x_values_as_text
### Title: Get x-axis values as text
### Aliases: get_x_values_as_text
### Keywords: xaxis dataset

### ** Examples

  ## Example of getting the x-axis values as text
  library(specmine.datasets)
  data(propolis)
  xvalues.text = get_x_values_as_text(propolis)



cleanEx()
nameEx("group_peaks")
### * group_peaks

flush(stderr()); flush(stdout())

### Name: group_peaks
### Title: Group peaks
### Aliases: group_peaks
### Keywords: peak alignment

### ** Examples




cleanEx()
nameEx("heatmap_correlations")
### * heatmap_correlations

flush(stderr()); flush(stdout())

### Name: heatmap_correlations
### Title: Correlations heatmap
### Aliases: heatmap_correlations
### Keywords: correlations heatmap

### ** Examples

  ## Example of correlations heatmap
  library(specmine.datasets)
  data(cachexia)
  correlations = correlations_dataset(cachexia)
  heatmap_correlations(correlations)



cleanEx()
nameEx("hierarchical_clustering")
### * hierarchical_clustering

flush(stderr()); flush(stdout())

### Name: hierarchical_clustering
### Title: Perform hierarchical clustering analysis
### Aliases: hierarchical_clustering
### Keywords: clustering hclust

### ** Examples

  ## Example of hierarchical clustering
  library(specmine.datasets)
  data(cachexia)
  hc.result = hierarchical_clustering(cachexia, 
	      distance = "euclidean", clustMethod = "complete", 
	      hc.type = "samples")



cleanEx()
nameEx("impute_nas_knn")
### * impute_nas_knn

flush(stderr()); flush(stdout())

### Name: impute_nas_knn
### Title: Impute missing values with KNN
### Aliases: impute_nas_knn
### Keywords: missing values

### ** Examples

  ## Example of NA imputation with knn
  library(specmine.datasets)
  data(propolis)
  dataset = impute_nas_knn(propolis, k=10)



cleanEx()
nameEx("impute_nas_linapprox")
### * impute_nas_linapprox

flush(stderr()); flush(stdout())

### Name: impute_nas_linapprox
### Title: Impute missing values with linear approximation
### Aliases: impute_nas_linapprox
### Keywords: missing values

### ** Examples

  ## Example of NA imputation with linear approximation
  library(specmine.datasets)
  data(propolis)
  dataset = impute_nas_linapprox(propolis)



cleanEx()
nameEx("impute_nas_mean")
### * impute_nas_mean

flush(stderr()); flush(stdout())

### Name: impute_nas_mean
### Title: Impute missing values with mean
### Aliases: impute_nas_mean
### Keywords: missing values

### ** Examples

  ## Example of NA imputation with mean
  library(specmine.datasets)
  data(propolis)
  propolis = impute_nas_mean(propolis)



cleanEx()
nameEx("impute_nas_median")
### * impute_nas_median

flush(stderr()); flush(stdout())

### Name: impute_nas_median
### Title: Impute missing values with median
### Aliases: impute_nas_median
### Keywords: missing values

### ** Examples

  ## Example of NA imputation with median
  library(specmine.datasets)
  data(propolis)
  propolis = impute_nas_median(propolis)



cleanEx()
nameEx("impute_nas_value")
### * impute_nas_value

flush(stderr()); flush(stdout())

### Name: impute_nas_value
### Title: Impute missing values with value replacement
### Aliases: impute_nas_value
### Keywords: missing values

### ** Examples

  ## Example of NA imputation with value replacing
  library(specmine.datasets)
  data(propolis)
  propolis = impute_nas_value(propolis, 0.0005)



cleanEx()
nameEx("indexes_to_xvalue_interval")
### * indexes_to_xvalue_interval

flush(stderr()); flush(stdout())

### Name: indexes_to_xvalue_interval
### Title: Get the x-values of a vector of indexes
### Aliases: indexes_to_xvalue_interval
### Keywords: xvalues indexes

### ** Examples

  ## Example of getting the interval of x-values from indexes
  library(specmine.datasets)
  data(propolis)
  xvalue.interval = indexes_to_xvalue_interval(propolis, c(10,50))



cleanEx()
nameEx("is_spectra")
### * is_spectra

flush(stderr()); flush(stdout())

### Name: is_spectra
### Title: Check type of data
### Aliases: is_spectra
### Keywords: dataset spectral

### ** Examples

  ## Example of checking if the dataset is from spectral data
  library(specmine.datasets)
  data(propolis)
  is_spectra(propolis)



cleanEx()
nameEx("kmeans_clustering")
### * kmeans_clustering

flush(stderr()); flush(stdout())

### Name: kmeans_clustering
### Title: Perform k-means clustering analysis
### Aliases: kmeans_clustering
### Keywords: clustering kmeans

### ** Examples

  ## Example of kmeans clustering
  library(specmine.datasets)
  data(cachexia)
  kmeans.result = kmeans_clustering(cachexia, 
		  num.clusters = 4, type = "samples")



cleanEx()
nameEx("kmeans_plot")
### * kmeans_plot

flush(stderr()); flush(stdout())

### Name: kmeans_plot
### Title: Plot kmeans clusters
### Aliases: kmeans_plot
### Keywords: clustering kmeans plotting

### ** Examples

  ## Example of kmeans plot - dataset filtered for performance purposes
  library(specmine.datasets)
  data(cachexia)
  kmeans.result = kmeans_clustering(cachexia, 
		  num.clusters = 4, type = "samples")
  kmeans_plot(cachexia, kmeans.result)



cleanEx()
nameEx("kmeans_result_df")
### * kmeans_result_df

flush(stderr()); flush(stdout())

### Name: kmeans_result_df
### Title: Show cluster's members
### Aliases: kmeans_result_df
### Keywords: clustering kmeans

### ** Examples

  ## Example of showing kmeans cluster's members
  library(specmine.datasets)
  data(cachexia)
  kmeans.result = kmeans_clustering(cachexia, 
		  num.clusters = 4, type = "samples")
  kmeans_result_df(kmeans.result)



cleanEx()
nameEx("kruskalTest_dataset")
### * kruskalTest_dataset

flush(stderr()); flush(stdout())

### Name: kruskalTest_dataset
### Title: Kruskal-Wallis tests on dataset
### Aliases: kruskalTest_dataset
### Keywords: Kruskal-Wallis

### ** Examples

  ## Example of ks-Tests on dataset
  library(specmine.datasets)
  data(cachexia)
  kruskaltests.result = kruskalTest_dataset(cachexia, "Muscle.loss", 
		  write.file = FALSE)



cleanEx()
nameEx("ksTest_dataset")
### * ksTest_dataset

flush(stderr()); flush(stdout())

### Name: ksTest_dataset
### Title: Kolmogorov-Smirnov tests on dataset
### Aliases: ksTest_dataset
### Keywords: Kolmogorov-Smirnov

### ** Examples

  ## Example of ks-Tests on dataset
  library(specmine.datasets)
  data(cachexia)
  kstests.result = ksTest_dataset(cachexia, "Muscle.loss", 
		  write.file = FALSE)



cleanEx()
nameEx("log_transform")
### * log_transform

flush(stderr()); flush(stdout())

### Name: log_transform
### Title: Logarithmic transformation.
### Aliases: log_transform
### Keywords: log transformation

### ** Examples

  ## Example of logarithmic transformation
  library(specmine.datasets)
  data(propolis)
  propolis_proc = missingvalues_imputation(propolis)
  datamat.log = log_transform(propolis_proc$data)



cleanEx()
nameEx("merge_data_metadata")
### * merge_data_metadata

flush(stderr()); flush(stdout())

### Name: merge_data_metadata
### Title: Merge data and metadata
### Aliases: merge_data_metadata
### Keywords: data metadata

### ** Examples

  ## Example of merging data and metadata
  library(specmine.datasets)
  data(cachexia)
  dt.merged = merge_data_metadata(cachexia)



cleanEx()
nameEx("metabolights_studies_list")
### * metabolights_studies_list

flush(stderr()); flush(stdout())

### Name: metabolights_studies_list
### Title: List the study IDs available in the MetaboLights database.
### Aliases: metabolights_studies_list

### ** Examples

## metabolights_studies_list()



cleanEx()
nameEx("metadata_as_variables")
### * metadata_as_variables

flush(stderr()); flush(stdout())

### Name: metadata_as_variables
### Title: Metadata as variables
### Aliases: metadata_as_variables
### Keywords: metadata variable

### ** Examples

  ## Example of using a metadata variable as data variable
  library(specmine.datasets)
  data(propolis)
  propolis = metadata_as_variables(propolis, "seasons", by.index = FALSE)



cleanEx()
nameEx("missingvalues_imputation")
### * missingvalues_imputation

flush(stderr()); flush(stdout())

### Name: missingvalues_imputation
### Title: Missing values imputation
### Aliases: missingvalues_imputation
### Keywords: missing values

### ** Examples

  ## Example of impute missing values
  library(specmine.datasets)
  data(propolis)
  dataset = missingvalues_imputation(propolis, method = "value", 
	    value = 0.0005)



cleanEx()
nameEx("multifactor_aov_all_vars")
### * multifactor_aov_all_vars

flush(stderr()); flush(stdout())

### Name: multifactor_aov_all_vars
### Title: Multifactor ANOVA
### Aliases: multifactor_aov_all_vars
### Keywords: anova multifactor

### ** Examples

  ## Example of multifactor ANOVA on all variables 
  library(specmine.datasets)
  data(propolis)
  propolis = missingvalues_imputation(propolis, "value", value = 0.00005)
  m.aov.results = multifactor_aov_all_vars(propolis, 
		  c("seasons","agroregions"), "seasons*agroregions")



cleanEx()
nameEx("multifactor_aov_pvalues_table")
### * multifactor_aov_pvalues_table

flush(stderr()); flush(stdout())

### Name: multifactor_aov_pvalues_table
### Title: Multifactor ANOVA p-values table
### Aliases: multifactor_aov_pvalues_table
### Keywords: multifactor anova

### ** Examples

  ## Example of multifactor ANOVA p-values table
  library(specmine.datasets)
  data(propolis)
  propolis = missingvalues_imputation(propolis, "value", value = 0.00005)
  m.aov.results = multifactor_aov_all_vars(propolis, 
		  c("seasons","agroregions"), "seasons*agroregions")
  m.aov.pvalues = multifactor_aov_pvalues_table(m.aov.results)



cleanEx()
nameEx("multifactor_aov_varexp_table")
### * multifactor_aov_varexp_table

flush(stderr()); flush(stdout())

### Name: multifactor_aov_varexp_table
### Title: Multifactor ANOVA variability explained table
### Aliases: multifactor_aov_varexp_table
### Keywords: varexp anova

### ** Examples

  ## Example of multifactor ANOVA variability explained table
  library(specmine.datasets)
  data(propolis)
  propolis = missingvalues_imputation(propolis, "value", value = 0.00005)
  m.aov.results = multifactor_aov_all_vars(propolis, 
		  c("seasons","agroregions"), "seasons*agroregions")
  m.aov.varepx = multifactor_aov_varexp_table(m.aov.results)



cleanEx()
nameEx("multiplot")
### * multiplot

flush(stderr()); flush(stdout())

### Name: multiplot
### Title: Multiplot
### Aliases: multiplot
### Keywords: multiplot ggplot

### ** Examples

     ## Example of multiplot
	 library(specmine.datasets)
     data(cachexia)
     pca.result = pca_analysis_dataset(cachexia)
     plot1 = pca_scoresplot2D(cachexia, pca.result, "Muscle.loss", 
	ellipses = TRUE)
     plot2 = pca_scoresplot2D(cachexia, pca.result, "Muscle.loss", 
	ellipses = FALSE, labels = TRUE)
     plts = list(plot1,plot2)
     multiplot(plts, cols = 2)



cleanEx()
nameEx("nmr_identification")
### * nmr_identification

flush(stderr()); flush(stdout())

### Name: nmr_identification
### Title: NMR metabolite identification
### Aliases: nmr_identification

### ** Examples




cleanEx()
nameEx("num_samples")
### * num_samples

flush(stderr()); flush(stdout())

### Name: num_samples
### Title: Get number of samples
### Aliases: num_samples
### Keywords: samples dataset

### ** Examples

  ## Example of getting the number of samples
  library(specmine.datasets)
  data(cachexia)
  number.of.samples = num_samples(cachexia)



cleanEx()
nameEx("num_x_values")
### * num_x_values

flush(stderr()); flush(stdout())

### Name: num_x_values
### Title: Get number of x values
### Aliases: num_x_values
### Keywords: dataset xaxis

### ** Examples

  ## Example of getting the number of x-axis values
  library(specmine.datasets)
  data(propolis)
  number.x.values = num_x_values(propolis)



cleanEx()
nameEx("pathway_analysis")
### * pathway_analysis

flush(stderr()); flush(stdout())

### Name: pathway_analysis
### Title: Creates the metabolic pathway wanted. If any of the given
###   compounds is present in the pathway, it is coloured differently.
### Aliases: pathway_analysis

### ** Examples




cleanEx()
nameEx("pca_analysis_dataset")
### * pca_analysis_dataset

flush(stderr()); flush(stdout())

### Name: pca_analysis_dataset
### Title: PCA analysis (classical)
### Aliases: pca_analysis_dataset
### Keywords: pca unsupervised

### ** Examples

  ## Example of performing a classical PCA analysis
  library(specmine.datasets)
  data(cachexia)
  pca.results = pca_analysis_dataset(cachexia)



cleanEx()
nameEx("pca_biplot")
### * pca_biplot

flush(stderr()); flush(stdout())

### Name: pca_biplot
### Title: PCA biplot
### Aliases: pca_biplot
### Keywords: pca biplot

### ** Examples

  ## Example of a PCA biplot
  library(specmine.datasets)
  data(cachexia)
  pca.result = pca_analysis_dataset(cachexia)
  pca_biplot(cachexia, pca.result, cex = 0.8)



cleanEx()
nameEx("pca_biplot3D")
### * pca_biplot3D

flush(stderr()); flush(stdout())

### Name: pca_biplot3D
### Title: 3D PCA biplot (interactive)
### Aliases: pca_biplot3D
### Keywords: pca biplot

### ** Examples




cleanEx()
nameEx("pca_importance")
### * pca_importance

flush(stderr()); flush(stdout())

### Name: pca_importance
### Title: PCA importance
### Aliases: pca_importance
### Keywords: pca importance

### ** Examples

  ## Example of performing a classical PCA analysis
  library(specmine.datasets)
  data(cachexia)
  pca.result = pca_analysis_dataset(cachexia)
  pca_importance(pca.result, pcs = 1:5)



cleanEx()
nameEx("pca_kmeans_plot2D")
### * pca_kmeans_plot2D

flush(stderr()); flush(stdout())

### Name: pca_kmeans_plot2D
### Title: 2D PCA k-means plot
### Aliases: pca_kmeans_plot2D
### Keywords: pca kmeans

### ** Examples




cleanEx()
nameEx("pca_kmeans_plot3D")
### * pca_kmeans_plot3D

flush(stderr()); flush(stdout())

### Name: pca_kmeans_plot3D
### Title: 3D PCA k-means plot (interactive)
### Aliases: pca_kmeans_plot3D
### Keywords: kmeans pca

### ** Examples




cleanEx()
nameEx("pca_pairs_kmeans_plot")
### * pca_pairs_kmeans_plot

flush(stderr()); flush(stdout())

### Name: pca_pairs_kmeans_plot
### Title: PCA k-means pairs plot
### Aliases: pca_pairs_kmeans_plot
### Keywords: kmeans pairs

### ** Examples




cleanEx()
nameEx("pca_pairs_plot")
### * pca_pairs_plot

flush(stderr()); flush(stdout())

### Name: pca_pairs_plot
### Title: PCA pairs plot
### Aliases: pca_pairs_plot
### Keywords: pairs pca

### ** Examples

  ## Example of a PCA pairs plot
  library(specmine.datasets)
  data(cachexia)
  pca.result = pca_analysis_dataset(cachexia)
  pca_pairs_plot(cachexia, pca.result, "Muscle.loss", pcas = c(1,2,3))



cleanEx()
nameEx("pca_plot_3d")
### * pca_plot_3d

flush(stderr()); flush(stdout())

### Name: pca_plot_3d
### Title: 3D pca plot
### Aliases: pca_plot_3d
### Keywords: pca pls

### ** Examples




cleanEx()
nameEx("pca_robust")
### * pca_robust

flush(stderr()); flush(stdout())

### Name: pca_robust
### Title: PCA analysis (robust)
### Aliases: pca_robust
### Keywords: pca robust

### ** Examples

  ## Example of performing a robust PCA analysis
  library(specmine.datasets)
  data(cachexia)
  pca.results = pca_robust(cachexia, center = "mean", scale = "mad", 
		k = 10)



cleanEx()
nameEx("pca_scoresplot2D")
### * pca_scoresplot2D

flush(stderr()); flush(stdout())

### Name: pca_scoresplot2D
### Title: 2D PCA scores plot
### Aliases: pca_scoresplot2D
### Keywords: scoresplot pca

### ** Examples

  ## Example of a 2D PCA scores plot
  library(specmine.datasets)
  data(cachexia)
  pca.result = pca_analysis_dataset(cachexia)
  pca_scoresplot2D(cachexia, pca.result, "Muscle.loss", pcas = c(1,2), 
    		   ellipses = TRUE)



cleanEx()
nameEx("pca_scoresplot3D")
### * pca_scoresplot3D

flush(stderr()); flush(stdout())

### Name: pca_scoresplot3D
### Title: 3D PCA scores plot
### Aliases: pca_scoresplot3D
### Keywords: scoresplot pca

### ** Examples




cleanEx()
nameEx("pca_scoresplot3D_rgl")
### * pca_scoresplot3D_rgl

flush(stderr()); flush(stdout())

### Name: pca_scoresplot3D_rgl
### Title: 3D PCA scores plot (interactive)
### Aliases: pca_scoresplot3D_rgl
### Keywords: scoresplot pca

### ** Examples




cleanEx()
nameEx("pca_screeplot")
### * pca_screeplot

flush(stderr()); flush(stdout())

### Name: pca_screeplot
### Title: PCA scree plot
### Aliases: pca_screeplot
### Keywords: screeplot pca

### ** Examples

  ## Example of a scree plot
  library(specmine.datasets)
  data(cachexia)
  pca.result = pca_analysis_dataset(cachexia)
  pca_screeplot(pca.result)



cleanEx()
nameEx("peaks_per_sample")
### * peaks_per_sample

flush(stderr()); flush(stdout())

### Name: peaks_per_sample
### Title: Peaks per sample
### Aliases: peaks_per_sample
### Keywords: peak sample

### ** Examples

    ## Example of counting the peaks in a sample
	library(specmine.datasets)
    data(propolisSampleList)
    num.peaks.sample = peaks_per_sample(propolisSampleList, 4)



cleanEx()
nameEx("peaks_per_samples")
### * peaks_per_samples

flush(stderr()); flush(stdout())

### Name: peaks_per_samples
### Title: Peaks per samples
### Aliases: peaks_per_samples
### Keywords: peak sample

### ** Examples

    ## Example of counting the peaks in each sample
	library(specmine.datasets)
    data(propolisSampleList)
    num.peaks.samples = peaks_per_samples(propolisSampleList)



cleanEx()
nameEx("plot_anova")
### * plot_anova

flush(stderr()); flush(stdout())

### Name: plot_anova
### Title: Plot ANOVA results
### Aliases: plot_anova
### Keywords: anova plot

### ** Examples

    ## Example of plotting the ANOVA results - first filter the 
    ## dataset to reduce computation time
	library(specmine.datasets)
	data(propolis)
    propolis_proc = missingvalues_imputation(propolis)
    propolis_proc = flat_pattern_filter(propolis_proc, "iqr", by.percent = TRUE,
	red.value = 75) 
    anova.results = aov_all_vars(propolis_proc, "seasons", doTukey = FALSE)
    plot_anova(propolis_proc, anova.results)



cleanEx()
nameEx("plot_fold_change")
### * plot_fold_change

flush(stderr()); flush(stdout())

### Name: plot_fold_change
### Title: Plot fold change results
### Aliases: plot_fold_change
### Keywords: foldchange plot

### ** Examples

    ## Example of plotting the fold change results
	library(specmine.datasets)
    data(cachexia)
    fc.results = fold_change(cachexia, "Muscle.loss", 
			"control")
    plot_fold_change(cachexia, fc.results, 2)



cleanEx()
nameEx("plot_kruskaltest")
### * plot_kruskaltest

flush(stderr()); flush(stdout())

### Name: plot_kruskaltest
### Title: Plot Kruskal-Wallis tests results
### Aliases: plot_kruskaltest
### Keywords: Kruskal-Wallis plot

### ** Examples

    ## Example of plotting the Kolmogorov-Smirnov tests results
	library(specmine.datasets)
    data(cachexia)
    kr.results = kruskalTest_dataset(cachexia, "Muscle.loss", 
	write.file = FALSE)
    plot_kruskaltest(cachexia, kr.results, 0.05)



cleanEx()
nameEx("plot_kstest")
### * plot_kstest

flush(stderr()); flush(stdout())

### Name: plot_kstest
### Title: Plot Kolmogorov-Smirnov tests results
### Aliases: plot_kstest
### Keywords: Kolmogorov-Smirnov plot

### ** Examples

    ## Example of plotting the Kolmogorov-Smirnov tests results
	library(specmine.datasets)
    data(cachexia)
    ks.results = ksTest_dataset(cachexia, "Muscle.loss", 
	write.file = FALSE)
    plot_kstest(cachexia, ks.results, 0.05)



cleanEx()
nameEx("plot_peaks")
### * plot_peaks

flush(stderr()); flush(stdout())

### Name: plot_peaks
### Title: Plot the peaks of a MS or NMR dataset.
### Aliases: plot_peaks

### ** Examples

  library(specmine.datasets)
  data(propolis)
  plot_peaks(propolis, "seasons", variable.bounds = c(0,3), samples=c("XX_au", "XX_sm", "XX_wi"))



cleanEx()
nameEx("plot_ttests")
### * plot_ttests

flush(stderr()); flush(stdout())

### Name: plot_ttests
### Title: Plot t-tests results
### Aliases: plot_ttests
### Keywords: ttest plot

### ** Examples

    ## Example of plotting the t-tests results
	library(specmine.datasets)
    data(cachexia)
    ttests.results = tTests_dataset(cachexia, "Muscle.loss")
    plot_ttests(cachexia, ttests.results, 0.05)



cleanEx()
nameEx("plotvar_twofactor")
### * plotvar_twofactor

flush(stderr()); flush(stdout())

### Name: plotvar_twofactor
### Title: Plot variable distribution on two factors
### Aliases: plotvar_twofactor
### Keywords: plot factor

### ** Examples

  ## Example of plotting a variable's distribution with 2 factors
  library(specmine.datasets)
  data(propolis)
  propolis_proc = missingvalues_imputation(propolis)
  plotvar_twofactor(propolis_proc, "0.46", "seasons", "agroregions")



cleanEx()
nameEx("predict_samples")
### * predict_samples

flush(stderr()); flush(stdout())

### Name: predict_samples
### Title: Predict samples
### Aliases: predict_samples
### Keywords: predict samples

### ** Examples




cleanEx()
nameEx("recursive_feature_elimination")
### * recursive_feature_elimination

flush(stderr()); flush(stdout())

### Name: recursive_feature_elimination
### Title: Perform recursive feature elimination
### Aliases: recursive_feature_elimination
### Keywords: rfe wrappers

### ** Examples




cleanEx()
nameEx("remove_data")
### * remove_data

flush(stderr()); flush(stdout())

### Name: remove_data
### Title: Remove data
### Aliases: remove_data
### Keywords: remove data

### ** Examples

  ## Example of removing data
  library(specmine.datasets)
  data(cachexia)
  dataset = remove_data(cachexia, c("Creatine","Serine"), type = "data", 
	    by.index = FALSE)



cleanEx()
nameEx("remove_data_variables")
### * remove_data_variables

flush(stderr()); flush(stdout())

### Name: remove_data_variables
### Title: Remove data variables
### Aliases: remove_data_variables
### Keywords: remove variable

### ** Examples

  ## Example of removing data variables
  library(specmine.datasets)
  data(cachexia)
  dataset = remove_data_variables(cachexia, c("Creatine","Serine"), 
	    by.index = FALSE)



cleanEx()
nameEx("remove_metadata_variables")
### * remove_metadata_variables

flush(stderr()); flush(stdout())

### Name: remove_metadata_variables
### Title: Remove metadata's variables
### Aliases: remove_metadata_variables
### Keywords: remove metadata

### ** Examples

  ## Example of removing metadata's variables
  library(specmine.datasets)
  data(propolis)
  dataset = remove_metadata_variables(propolis, c("seasons"))



cleanEx()
nameEx("remove_peaks_interval")
### * remove_peaks_interval

flush(stderr()); flush(stdout())

### Name: remove_peaks_interval
### Title: Remove interval of peaks
### Aliases: remove_peaks_interval
### Keywords: peak sample

### ** Examples

  ## Example of removing a interval of peaks
  library(specmine.datasets)
  data(propolisSampleList)
  samples.df = remove_peaks_interval(propolisSampleList[[1]], 2, 8.05)



cleanEx()
nameEx("remove_peaks_interval_sample_list")
### * remove_peaks_interval_sample_list

flush(stderr()); flush(stdout())

### Name: remove_peaks_interval_sample_list
### Title: Remove interval of peaks (sample list)
### Aliases: remove_peaks_interval_sample_list
### Keywords: peak sample

### ** Examples

  ## Example of removing a interval of peaks in a sample list
  library(specmine.datasets)
  data(propolisSampleList)
  samples.list = remove_peaks_interval_sample_list(propolisSampleList, 2, 8.05)



cleanEx()
nameEx("remove_samples")
### * remove_samples

flush(stderr()); flush(stdout())

### Name: remove_samples
### Title: Remove samples
### Aliases: remove_samples
### Keywords: remove sample

### ** Examples

  ## Example of removing samples
  library(specmine.datasets)
  data(cachexia)
  cachexia = remove_samples(cachexia, c("PIF_178","PIF_090"))



cleanEx()
nameEx("remove_samples_by_na_metadata")
### * remove_samples_by_na_metadata

flush(stderr()); flush(stdout())

### Name: remove_samples_by_na_metadata
### Title: Remove samples by NA on metadata
### Aliases: remove_samples_by_na_metadata
### Keywords: remove metadata

### ** Examples

  ## Example of removing samples that have NAs on metadata's variable
  library(specmine.datasets)
  data(cachexia)
  cachexia$metadata$Muscle.loss[10] = NA
  cachexia = remove_samples_by_na_metadata(cachexia, "Muscle.loss")



cleanEx()
nameEx("remove_samples_by_nas")
### * remove_samples_by_nas

flush(stderr()); flush(stdout())

### Name: remove_samples_by_nas
### Title: Remove samples by NAs
### Aliases: remove_samples_by_nas
### Keywords: remove missingvalue

### ** Examples

  ## Example of removing samples by NAs
  library(specmine.datasets)
  data(propolis)
  propolis = remove_samples_by_nas(propolis, 40, by.percent = TRUE)



cleanEx()
nameEx("remove_variables_by_nas")
### * remove_variables_by_nas

flush(stderr()); flush(stdout())

### Name: remove_variables_by_nas
### Title: Remove variables by NAs
### Aliases: remove_variables_by_nas
### Keywords: remove missingvalue

### ** Examples

  ## Example of removing variables by NAs
  library(specmine.datasets)
  data(propolis)
  propolis = remove_variables_by_nas(propolis, 40, by.percent = TRUE)



cleanEx()
nameEx("replace_data_value")
### * replace_data_value

flush(stderr()); flush(stdout())

### Name: replace_data_value
### Title: Replace data value
### Aliases: replace_data_value
### Keywords: dataset value

### ** Examples

  ## Example of replacing a data value from the dataset
  library(specmine.datasets)
  data(cachexia)
  dataset = replace_data_value(cachexia, "Creatine", "PIF_178", 10.3, 
	    by.index = FALSE)



cleanEx()
nameEx("replace_metadata_value")
### * replace_metadata_value

flush(stderr()); flush(stdout())

### Name: replace_metadata_value
### Title: Replace metadata's value
### Aliases: replace_metadata_value
### Keywords: metadata dataset

### ** Examples

  ## Example of replacing metadata's variable value of a sample
  library(specmine.datasets)
  data(cachexia)
  dataset = replace_metadata_value(cachexia, "Muscle.loss", "PIF_178", 
	    "control")



cleanEx()
nameEx("set_metadata")
### * set_metadata

flush(stderr()); flush(stdout())

### Name: set_metadata
### Title: Set new metadata
### Aliases: set_metadata
### Keywords: metadata dataset

### ** Examples

  ## Example of setting a new metadata to the dataset
  library(specmine.datasets)
  data(cachexia)
  new.metadata = c(rep("meta1", 39), rep("meta2", 38)) 
  new.metadata = data.frame(var_meta = new.metadata)
  rownames(new.metadata) = get_sample_names(cachexia)
  cachexia = set_metadata(cachexia, new.metadata)



cleanEx()
nameEx("set_sample_names")
### * set_sample_names

flush(stderr()); flush(stdout())

### Name: set_sample_names
### Title: Set samples names
### Aliases: set_sample_names
### Keywords: sample dataset

### ** Examples

  ## Example of setting a new value label to the dataset
  library(specmine.datasets)
  data(cachexia)
  new.samples.names = as.character(1:77)
  cachexia = set_sample_names(cachexia, new.samples.names)



cleanEx()
nameEx("set_value_label")
### * set_value_label

flush(stderr()); flush(stdout())

### Name: set_value_label
### Title: Set value label
### Aliases: set_value_label
### Keywords: label dataset

### ** Examples

  ## Example of setting a new value label to the dataset
  library(specmine.datasets)
  data(cachexia)
  cachexia = set_value_label(cachexia, "new value label")



cleanEx()
nameEx("set_x_label")
### * set_x_label

flush(stderr()); flush(stdout())

### Name: set_x_label
### Title: Set x-label
### Aliases: set_x_label
### Keywords: dataset label

### ** Examples

  ## Example of setting a new x-label to the dataset
  library(specmine.datasets)
  data(cachexia)
  cachexia = set_x_label(cachexia, "new x-label")



cleanEx()
nameEx("set_x_values")
### * set_x_values

flush(stderr()); flush(stdout())

### Name: set_x_values
### Title: Set new x-values
### Aliases: set_x_values
### Keywords: xvalues dataset

### ** Examples

  ## Example of setting new x-values to the dataset
  library(specmine.datasets)
  data(cachexia)
  new.xvalues = 1:63
  cachexia = set_x_values(cachexia, new.xvalues, new.x.label = NULL)



cleanEx()
nameEx("spectra_options")
### * spectra_options

flush(stderr()); flush(stdout())

### Name: spectra_options
### Title: Information on the library of NMR reference spectra in our
###   package.
### Aliases: spectra_options
### Keywords: datasets

### ** Examples

data(spectra_options)



cleanEx()
nameEx("stats_by_sample")
### * stats_by_sample

flush(stderr()); flush(stdout())

### Name: stats_by_sample
### Title: Statistics of samples
### Aliases: stats_by_sample
### Keywords: statistic sample

### ** Examples

     ## Example of getting stats of samples
	 library(specmine.datasets)
     data(cachexia)
     samples.stats.result = stats_by_sample(cachexia)



cleanEx()
nameEx("stats_by_variable")
### * stats_by_variable

flush(stderr()); flush(stdout())

### Name: stats_by_variable
### Title: Statistics of variables
### Aliases: stats_by_variable
### Keywords: statistic variable

### ** Examples

     ## Example of getting stats of variables
	 library(specmine.datasets)
     data(cachexia)
     variable.stats.result = stats_by_variable(cachexia)



cleanEx()
nameEx("subset_by_samples_and_xvalues")
### * subset_by_samples_and_xvalues

flush(stderr()); flush(stdout())

### Name: subset_by_samples_and_xvalues
### Title: Subset by samples and x-values
### Aliases: subset_by_samples_and_xvalues
### Keywords: subset sample xvalue

### ** Examples

  ## Example of subsetting samples and x-values
  library(specmine.datasets)
  data(cachexia)
  subset = subset_by_samples_and_xvalues(cachexia, c("PIF_178","NETL_022_V1"), 
	   variables = c("Creatine","Serine"))



cleanEx()
nameEx("subset_metadata")
### * subset_metadata

flush(stderr()); flush(stdout())

### Name: subset_metadata
### Title: Subset metadata
### Aliases: subset_metadata
### Keywords: subset metadata

### ** Examples

  ## Example of subsetting samples
  library(specmine.datasets)
  data(propolis)
  subset = subset_metadata(propolis, c("seasons"))



cleanEx()
nameEx("subset_random_samples")
### * subset_random_samples

flush(stderr()); flush(stdout())

### Name: subset_random_samples
### Title: Subset random samples
### Aliases: subset_random_samples
### Keywords: random subset

### ** Examples

  ## Example of subsetting random samples
  library(specmine.datasets)
  data(cachexia)
  subset = subset_random_samples(cachexia, 15)



cleanEx()
nameEx("subset_samples")
### * subset_samples

flush(stderr()); flush(stdout())

### Name: subset_samples
### Title: Subset samples
### Aliases: subset_samples
### Keywords: subset sample

### ** Examples

  ## Example of subsetting samples
  library(specmine.datasets)
  data(cachexia)
  subset = subset_samples(cachexia, c("PIF_178","PIF_132"))



cleanEx()
nameEx("subset_samples_by_metadata_values")
### * subset_samples_by_metadata_values

flush(stderr()); flush(stdout())

### Name: subset_samples_by_metadata_values
### Title: Subset samples by metadata values
### Aliases: subset_samples_by_metadata_values
### Keywords: subset metadata

### ** Examples

  ## Example of subsetting samples by metadata's values
  library(specmine.datasets)
  data(propolis)
  subset_samples_by_metadata_values(propolis, "seasons", 
                                  c("sm","au"))



cleanEx()
nameEx("subset_x_values")
### * subset_x_values

flush(stderr()); flush(stdout())

### Name: subset_x_values
### Title: Subset x-values
### Aliases: subset_x_values
### Keywords: subset xvalues

### ** Examples

  ## Example of subsetting x-values
  library(specmine.datasets)
  data(cachexia)
  subset = subset_x_values(cachexia, c(1,2,10,20), by.index = TRUE)



cleanEx()
nameEx("subset_x_values_by_interval")
### * subset_x_values_by_interval

flush(stderr()); flush(stdout())

### Name: subset_x_values_by_interval
### Title: Subset x-values by interval
### Aliases: subset_x_values_by_interval
### Keywords: subset interval

### ** Examples

  ## Example of subsetting x-values by an interval
  library(specmine.datasets)
  data(propolis)
  subset = subset_x_values_by_interval(propolis, 1, 3)



cleanEx()
nameEx("sum_dataset")
### * sum_dataset

flush(stderr()); flush(stdout())

### Name: sum_dataset
### Title: Dataset summary
### Aliases: sum_dataset
### Keywords: dataset summary

### ** Examples

## Don't show: 
  ## Example of summarizing a dataset
  library(specmine.datasets)
  data(cachexia)
  sum_dataset(cachexia, stats = TRUE)
## End(Don't show)



cleanEx()
nameEx("summary_var_importance")
### * summary_var_importance

flush(stderr()); flush(stdout())

### Name: summary_var_importance
### Title: Summary of variables importance
### Aliases: summary_var_importance
### Keywords: vip summary

### ** Examples




cleanEx()
nameEx("tTests_dataset")
### * tTests_dataset

flush(stderr()); flush(stdout())

### Name: tTests_dataset
### Title: t-Tests on dataset
### Aliases: tTests_dataset
### Keywords: ttest

### ** Examples

  ## Example of t-Tests on dataset
  library(specmine.datasets)
  data(cachexia)
  ttests.result = tTests_dataset(cachexia, "Muscle.loss", 
		  write.file = FALSE)



cleanEx()
nameEx("train_and_predict")
### * train_and_predict

flush(stderr()); flush(stdout())

### Name: train_and_predict
### Title: Train and predict
### Aliases: train_and_predict
### Keywords: train predict

### ** Examples

  ## Example of training and predicting
  library(specmine.datasets)
  data(cachexia)
  result = train_and_predict(cachexia, new.samples = cachexia$data,
	 "Muscle.loss", "pls", "cv")



cleanEx()
nameEx("train_classifier")
### * train_classifier

flush(stderr()); flush(stdout())

### Name: train_classifier
### Title: Train classifier
### Aliases: train_classifier
### Keywords: train classifier

### ** Examples

  ## Example of training a classifier
  library(specmine.datasets)
  data(cachexia)
  result = train_classifier(cachexia, "Muscle.loss", "pls", "cv")



cleanEx()
nameEx("train_models_performance")
### * train_models_performance

flush(stderr()); flush(stdout())

### Name: train_models_performance
### Title: Train models
### Aliases: train_models_performance
### Keywords: train model

### ** Examples




cleanEx()
nameEx("transform_data")
### * transform_data

flush(stderr()); flush(stdout())

### Name: transform_data
### Title: Transform data
### Aliases: transform_data
### Keywords: log cubicroot

### ** Examples


  ## Example of logarithmic transformation
  library(specmine.datasets)
  data(cachexia)
  dataset.log = transform_data(cachexia, "log")




cleanEx()
nameEx("values_per_peak")
### * values_per_peak

flush(stderr()); flush(stdout())

### Name: values_per_peak
### Title: Values per peak
### Aliases: values_per_peak
### Keywords: peak sample

### ** Examples

  ## Example of getting the number of values for each peak
  library(specmine.datasets)
  data(propolis)
  num.peaks = values_per_peak(propolis$data)



cleanEx()
nameEx("values_per_sample")
### * values_per_sample

flush(stderr()); flush(stdout())

### Name: values_per_sample
### Title: Values per peak
### Aliases: values_per_sample
### Keywords: values sample

### ** Examples

  ## Example of getting the number of values for each sample
  library(specmine.datasets)
  data(propolis)
  num.values.samples = values_per_sample(propolis$data)



cleanEx()
nameEx("variables_as_metadata")
### * variables_as_metadata

flush(stderr()); flush(stdout())

### Name: variables_as_metadata
### Title: Variables as metadata
### Aliases: variables_as_metadata
### Keywords: metadata variable

### ** Examples

  ## Example of using a variable as metadata variable
  library(specmine.datasets)
  data(cachexia)
  dataset = variables_as_metadata(cachexia, "Creatine", by.index = FALSE)



cleanEx()
nameEx("volcano_plot_fc_tt")
### * volcano_plot_fc_tt

flush(stderr()); flush(stdout())

### Name: volcano_plot_fc_tt
### Title: Volcano plot
### Aliases: volcano_plot_fc_tt
### Keywords: volcano plot

### ** Examples

    ## Example of a volcano plot
	library(specmine.datasets)
    data(cachexia)
    foldchange.results = fold_change(cachexia, "Muscle.loss", "control")
    ttests.results = tTests_dataset(cachexia, "Muscle.loss")
    volcano_plot_fc_tt(cachexia, foldchange.results, ttests.results, 
		       fc.threshold = 2, tt.threshold = 0.01)



cleanEx()
nameEx("x_values_to_indexes")
### * x_values_to_indexes

flush(stderr()); flush(stdout())

### Name: x_values_to_indexes
### Title: Get x-values indexes
### Aliases: x_values_to_indexes
### Keywords: indexes xvalues

### ** Examples

  ## Example of getting the indexes of the x-values
  library(specmine.datasets)
  data(propolis)
  indexes = x_values_to_indexes(propolis, c(1.3, 3.51, 6.03))



cleanEx()
nameEx("xvalue_interval_to_indexes")
### * xvalue_interval_to_indexes

flush(stderr()); flush(stdout())

### Name: xvalue_interval_to_indexes
### Title: Get indexes of an interval of x-values
### Aliases: xvalue_interval_to_indexes
### Keywords: indexes interval

### ** Examples

  ## Example of getting the indexes of an interval of x-values
  library(specmine.datasets)
  data(propolis)
  indexes.interval = xvalue_interval_to_indexes(propolis, 2.0, 5.5)



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
