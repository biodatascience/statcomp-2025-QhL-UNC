#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat col_ridge_2(arma::mat Y, arma::mat X, arma::vec lambda) {
    int m = X.n_cols;
    int n = Y.n_cols;
    arma::mat Z(m, n);
    arma::mat IdentityM(m, m, arma::fill::eye);
    for (int i = 0; i < n; i++)
    {
        Z.col(i) = arma::inv(X.t()*X + lambda[i] * IdentityM)*X.t()*Y.col(i);
    }
    return Z;
}
