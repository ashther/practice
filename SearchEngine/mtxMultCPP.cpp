#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix mtxMultCPP(NumericMatrix x, NumericMatrix y) {
  int x_nrow = x.nrow();
  int x_ncol = x.ncol();
  int y_ncol = y.ncol();
  NumericMatrix result(x_nrow, y_ncol);
  
  for (int i = 0; i < x_nrow; i++) {
    for (int j = 0; j < y_ncol; j++) {
      double temp = 0;
      for (int k = 0; k < x_ncol; k++) {
        temp += x(i, k) * y(k, j);
      }
      result(i, j) = temp;
    }
  }
  return result;
}

/*** R
temp %*% tdmtx

mtxMultCPP(temp, tdmtx)
*/
