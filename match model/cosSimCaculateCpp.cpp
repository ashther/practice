#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cosSimCaculateCpp(NumericMatrix x, NumericMatrix termVecSentMtx) {
  int nrow = termVecSentMtx.nrow(), ncol = termVecSentMtx.ncol(), nrow_x = x.nrow();
  NumericMatrix result(nrow_x, nrow);
  
  for (int k = 0; k < nrow_x; k++) {
    double x_sum_sqrt = 0.0;
    
    for (int j = 0; j < ncol; j++) {
      x_sum_sqrt += x(k, j) * x(k, j);
    }
    x_sum_sqrt = sqrt(x_sum_sqrt);
    // Rcout << "x_sum_sqrt: " << x_sum_sqrt<<std::endl;
    
    for (int i = 0; i < nrow; i++) {
      double temp = 0.0;
      double termVecSent_sum_sqrt = 0.0;
      for (int j = 0; j < ncol; j++) {
        temp += termVecSentMtx(i, j) * x(k, j);
        termVecSent_sum_sqrt += termVecSentMtx(i, j) * termVecSentMtx(i, j);
      }
      // Rcout << "termVecSent_sum_sqrt in " << i << ": " << termVecSent_sum_sqrt << std::endl;
      result(k, i) = temp/(sqrt(termVecSent_sum_sqrt) * x_sum_sqrt);
    }
  }
  
  return result;
}

/*** R
x <- 1:4
y <- matrix(1:12, 3)
z <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4), 3)
cosSimCaculateCpp(matrix(x, nrow = 1), y)
cosSimCaculateCpp(z, y)
*/
