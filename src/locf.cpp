#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector locf(NumericVector x, bool reverse) 
{
  long n = x.size();
  
  if (!reverse) {
    for(long i = 0; i < n; i++ ) {
      
      if(i > 0 && !R_finite(x[i]) && R_finite(x[i-1])) 
      {
        x[i] = x[i-1];
      }
    }
  }
  else {
    for(long i = n-1; i >= 0; i-- ) {
      
      if(i < n-1 && !R_finite(x[i]) && R_finite(x[i+1])) 
      {
        x[i] = x[i+1];
      }
    }
    
  }
 
  return x;
}


