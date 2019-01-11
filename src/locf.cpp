#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::NumericVector locf(NumericVector x, bool reverse) 
{

  Rcpp::NumericVector z = clone(x);
  
  long n = z.size();
  
  if (!reverse) {
    for(long i = 0; i < n; i++ ) {
      
      if (i % 1024 == 0) {Rcpp::checkUserInterrupt();}
      
      
      if(i > 0 && !R_finite(z[i]) && R_finite(z[i-1])) 
      {
        z[i] = z[i-1];
      }
    }
  }
  else {
    for(long i = n-1; i >= 0; i-- ) {
      
      if (i % 1024 == 0) {Rcpp::checkUserInterrupt();}
        
        
      if(i < n-1 && !R_finite(z[i]) && R_finite(z[i+1])) 
      {
        z[i] = z[i+1];
      }
    }
    
  }
return z;
 
}


