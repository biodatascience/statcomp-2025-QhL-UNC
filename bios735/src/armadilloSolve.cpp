#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat armadillo_solve(arma::mat A, arma::vec b) {
    int n = A.n_cols;
    arma::mat Z(n, 1);
    Z = arma::solve(A, b);
    return Z;
}
