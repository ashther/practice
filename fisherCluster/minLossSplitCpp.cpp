#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector minLossSplitCpp(NumericVector dMtx, int n, int k) {
  NumericVector value(n - k + 1);
  
  // n_cpp = n_r - 2
  // j_cpp = j_r - 1
  if (k == 2) {
    for (int j = k; j <= n; j++) {
      if (j == 2) {
        value[j - k] = dMtx(n - 2, j - 1);
      } else if (j == n) {
        value[j - k] = dMtx(j - 2, 0);
      } else {
        value[j - k] = dMtx(j - 2, 0) + dMtx(n - 2, j - 1);
      }
    }
  } else {
    for (int j = k; j <= n; j++) {
      if (j == n) {
        value[j - k] = min(minLossSplitCpp(dMtx, j - 1, k - 1));
      } else {
        value[j - k] = min(minLossSplitCpp(dMtx, j - 1, k - 1)) + dMtx(n - 2, j - 1);
      }
    }
  }
  
  // double min_value = value[0];
  // for (double i  = 0; i < value.size(); i ++) {
  //   if (value[i] < min_value) {
  //     min_value = value[i];
  //   }
  // }
  return value;
}



/*** R
minLossSplitCpp(dMtx, 11, 3)
*/
