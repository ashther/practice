#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double jacSimCaculateCpp(CharacterVector x, CharacterVector y) {
  int a = x.size();
  int b = y.size();
  if (a == 0 || b == 0) {
    return 0;
  }
  int c = 0;
  for (int i = 0; i < a; i++) {
    for (int j = 0; j < b; j++) {
      if (x[i] == y[j]) {
        c += 1;
      }
    }
  }
  if (c == 0) {
    return  0;
  }
  double denominator = log(a) + log(b);
  if (denominator < 1e-12) {
    return 0;
  }
  return c / denominator;
}

// [[Rcpp::export]]
NumericMatrix simMtxCreateCpp(List sentences) {
  // Environment env = Environment::global_env();
  // Function jacSimCaculate = env["jacSimCaculate"];
  int n = sentences.size();
  NumericMatrix simMtx(n, n);
  for (int i = 0; i < n; i++) {
    CharacterVector x = sentences[i];
    // if (i % 100 == 0) {
    //   Rcout << i <<std::endl;
    // }
    for (int j = 0; j < i; j++) {
      CharacterVector y = sentences[j];
      // simMtx(i, j) = as<double>(jacSimCaculate(x, y));
      simMtx(i, j) = jacSimCaculateCpp(x, y);
    }
  }
  return simMtx;
}
