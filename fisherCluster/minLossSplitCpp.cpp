#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List minLossSplitCpp(NumericMatrix dMtx, NumericMatrix min_loss_mtx, int n, int k) {
  List result;
  NumericVector::iterator it;
  NumericVector value(n - k + 1);
  
  // n_cpp = n_r - 2
  // j_cpp = j_r - 1
  if (k == 2) {
    for (int j = k; j <= n; j++) {
      if (j == 2) {
        value[j - k] = dMtx(n - 2, j - 1);
      } else if (j == n) {
        value[j - k] = dMtx(j - 3, 0);
      } else {
        value[j - k] = dMtx(j - 3, 0) + dMtx(n - 2, j - 1);
      }
    }
  } else {
    for (int j = k; j <= n; j++) {
      if (j == n) {
        if ((j - 4) < 0 || (k - 3) < 0) {
          value[j - k] = 0;
        } else {
          value[j - k] = min_loss_mtx(j - 4, k - 3);
        }
      } else {
        if ((j - 4) < 0 || (k - 3) < 0) {
          value[j - k] = dMtx(n - 2, j - 1);
        } else {
          value[j - k] = min_loss_mtx(j - 4, k - 3) + dMtx(n - 2, j - 1);
        }
      }
    }
  }
  
  double min_value = min(value);
  int idx = which_min(value) + k;
  result["value"] = min_value;
  result["idx"] = idx;
  return result;
}



/*** R
minLossSplitCpp(dMtx, min_loss_mtx, 11, 3)
*/
