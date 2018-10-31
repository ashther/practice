#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector neighborCosSimCaculate(NumericMatrix x) {
  int n_row = x.nrow();
  NumericVector result(n_row - 1), x_sqrt(n_row);
  
  for (int i = 0; i < n_row; i++) {
    float temp = 0.0;
    for (int j = 0; j < x.ncol(); j++) {
      temp += pow(x(i, j), 2);
    }
    x_sqrt(i) = sqrt(temp);
  }
  
  for (int i = 0; i < n_row - 1; i++) {
    float temp = 0.0;
    for (int j = 0; j < x.ncol(); j++) {
      temp += x(i, j) * x(i + 1, j);
    }
    result(i) = temp / x_sqrt(i) / x_sqrt(i + 1);
  }
  
  return result;
}

// [[Rcpp::export]]
NumericVector mult2oneCosSimCaculate(NumericMatrix x, NumericVector y) {
  int n_row = x.nrow();
  NumericVector result(n_row);
  
  float temp = 0.0, sqrt_y;
  for (int i = 0; i < x.ncol(); i++) {
    temp += pow(y(i), 2);
  }
  sqrt_y = sqrt(temp);
  
  for (int i = 0; i < n_row; i++) {
    float temp_xy = 0.0, temp_xx = 0.0;
    for (int j = 0; j < x.ncol(); j++) {
      temp_xx += x(i, j) * x(i, j);
      temp_xy += x(i, j) * y(j);
    }
    
    result(i) = temp_xy / sqrt(temp_xx) / sqrt_y;
  }
  
  return result;
}

// [[Rcpp::export]]
float cosSim(NumericVector x, NumericVector y) {
  float temp = 0.0, sum_x = 0.0, sum_y = 0.0;
  
  for (int i = 0; i < x.size(); i++) {
    temp += x(i) * y(i);
    sum_x += pow(x(i), 2);
    sum_y += pow(y(i), 2);
  }
  
  temp = temp / sqrt(sum_x) / sqrt(sum_y);
  return temp;
} 

/*** R
test_mtx <- matrix(sample(9), 3)
test_vec <- sample(9, 3)
for (i in 1:(nrow(test_mtx) - 1)) {
  print(
    sum(test_mtx[i, ] * test_mtx[i + 1, ]) / 
      (sqrt(sum(test_mtx[i, ] ^ 2)) * sqrt(sum(test_mtx[i + 1, ] ^ 2)))
  )
}
neighborCosSimCaculate(test_mtx)
################

for (i in 1:nrow(test_mtx)) {
  print(
    sum(test_mtx[i, ] * test_vec) / 
      (sqrt(sum(test_mtx[i, ] ^ 2)) * sqrt(sum(test_vec ^ 2)))
  )
}
mult2oneCosSimCaculate(test_mtx, test_vec)
################
x <- sample(10)
y <- sample(10)
print(sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2))))
cosSim(x, y)

*/
