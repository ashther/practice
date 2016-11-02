#include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]

// [[Rcpp::export]]
arma::mat mtxMultArm(arma::mat A, arma::mat B) {
  return A*B; // Matrix multiplication, not element-wise
}

/***R

mtxMultArm(temp, tdmtx)
*/