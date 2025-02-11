#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector one_or_exp(NumericVector x) {
  int n = x.size();
  NumericVector y(n);
  for(int i = 0; i < n; i++){
    y[i] = exp(std::max(0.0, x[i]));
  }
  return y;
}