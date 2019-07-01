#include <Rcpp.h>
using namespace Rcpp;


struct pow_wrapper {
  public: double operator()(double a, double b) {
    return ::pow(a, b);
  }
};

NumericVector vecpow(const IntegerVector base, const NumericVector exp) {
  NumericVector out(base.size());
  std::transform(base.cbegin(), base.cend(), exp.cbegin(), out.begin(), pow_wrapper());
  return out;
}



// [[Rcpp::export]]
Rcpp::NumericVector ma(NumericVector x, int k, String weighting) {
  Rcpp::NumericVector tempdata = clone(x);
  Rcpp::NumericVector out = clone(x);
  
  int n = tempdata.size();
  
  for (int i = 0; i < n; i++ ) {
    // check for interrupt every 1024 iterations
    if (i % 1024 == 0) {Rcpp::checkUserInterrupt();}
    
    // If Value is NA -> impute it based on selected method
    if (ISNAN(tempdata[i])) {
      int ktemp = k;
      IntegerVector usedIndices = seq(i - ktemp, i + ktemp);
      usedIndices = usedIndices[usedIndices >= 0];
      usedIndices = usedIndices[usedIndices < n];
      NumericVector t = tempdata[usedIndices];
      
      // Search for at least 2 NA-values
      while (sum(!is_na(t)) < 2) {
        ktemp = ktemp + 1;
        usedIndices = seq(i - ktemp, i + ktemp);
        usedIndices = usedIndices[usedIndices >= 0];
        usedIndices = usedIndices[usedIndices < n];
        t = tempdata[usedIndices];
      }
      
      if (weighting =="simple") {
        // Calculate mean value
        NumericVector noNAs = wrap(na_omit(t));
        out[i] = mean(noNAs);
      } 
      else if(weighting == "linear") {
        // Calculate weights based on indices 1/(distance from current index+1)
        // Set weights where data is NA to 0
        // Sum up all weights (needed later) to norm it
        // Create weighted data (weights*data)
        // Sum up
        NumericVector weightsData = 1 / (abs(usedIndices - i) + 1);
        LogicalVector naCheck = !is_na(t);
        weightsData = weightsData * as<NumericVector>(naCheck);
        double sumWeights = sum(weightsData);
        NumericVector weightedData = (t * weightsData) / sumWeights;
        NumericVector noNAs = wrap(na_omit(weightedData));
        out[i] = sum(noNAs);
      } 
      else if (weighting == "exponential") {
        // Calculate weights based on indices 1/ 2 ^ (distance from current index)
        // Set weights where data is NA to 0
        // Sum up all weights (needed later) to norm it
        // Create weighted data (weights*data)
        // Sum up
        NumericVector expo = abs(usedIndices - i);
        IntegerVector base = Rcpp::rep(2, expo.size());
        NumericVector weightsData = 1 / (vecpow(base, expo));
        LogicalVector naCheck = !is_na(t);
        weightsData = weightsData * as<NumericVector>(naCheck);
        double sumWeights = sum(weightsData);
        NumericVector weightedData = (t * weightsData) / sumWeights;
        NumericVector noNAs = wrap(na_omit(weightedData));
        out[i] = sum(noNAs);
      } else {
        stop("Wrong input for parameter weighting. Has to be \"simple\",\"linear\" or \"exponential\"." );
      }
    }
  }
  
  return out;
}

