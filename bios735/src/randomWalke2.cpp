#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List random_walk2 (int n, double lambda) {
  NumericVector x(n);
  NumericVector y(n);
  x[0] = 0;
  y[0] = 0;
  for (int i = 1; i < n; i++)
  {
    x[i] = x[i - 1] + lambda * (2.0 * Rf_rbinom(1, 0.5)-1.0);
    y[i] = y[i - 1] + lambda * (2.0 * Rf_rbinom(1,0.5) - 1.0);
  }
  List L = List::create(_["x"] =x, _["y"]=y);
  return L;
}