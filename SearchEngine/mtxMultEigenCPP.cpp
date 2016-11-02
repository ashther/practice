#include<RcppEigen.h>
//[[Rcpp::depends(RcppEigen)]]

using Eigen::Map;
using Eigen::MatrixXd;

//[[Rcpp::export]]
MatrixXd mtxMultEigen(Map<MatrixXd> A, Map<MatrixXd> B) {
  return A * B;
}

/***R
mtxMultEigen(temp, tdmtx)
*/