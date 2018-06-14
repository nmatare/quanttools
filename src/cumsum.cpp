#include <Rcpp.h>
#include <vector> 
using namespace Rcpp;

//'   Returns a vector whose elements are the cumulative sums of the inputted
//'   numeric vector. The argument 'threshold' controls the max/min 
//'   ceiling/floor of the running sum before it is reset; i.e., if the 
//'   threshold is set to (+/-) Inf, one would, essentially, compute the base R 
//'   cumsum', albeit slower
//' 
//' @param x          A numeric vector
//' @param threshold  A numeric scalar specifying the cumulative 
//'                   threshold(reset)
//' @export
// [[Rcpp::export]]
std::vector<double> cumsum_reset(NumericVector x, double threshold){

  int n = x.size();
  double runsum = 0;
  std::vector<double> out;

  for(int i = 0; i < n; i++){
    runsum += x[i];
    if(runsum > threshold)
      runsum = x[i];
    out.push_back(runsum);
  }
  return out;
}
