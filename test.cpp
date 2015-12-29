#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;
using namespace std;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double meanC(NumericVector x) {
    double res = 0;
    int n = x.size();
    for (int i = 0; i < n; i++) {
        res += x[i];
    }
    return res / n;
}

// [[Rcpp::export]]
bool allC(LogicalVector x) {
    int n = x.size();
    for(int i = 0; i < n; i++) {
        if (!x[i]) {
            return false;
        }
    }
    return true;
}

// [[Rcpp::export]]
NumericVector cumsumC(NumericVector x) {
    int n = x.size();
    double temp = 0;
    NumericVector result(n);
    for (int i = 0; i < n; i++) {
        temp += x[i];
        //cout<<temp<<endl;
        result[i] = temp;
    }
    return result;
}

// [[Rcpp::export]]
NumericVector cumprodC(NumericVector x) {
    int n = x.size();
    double temp = 1;
    NumericVector result(n);
    for (int i = 0; i < n; i++) {
        temp *= x[i];
        result[i] = temp;
    }
    return result;
}

// [[Rcpp::export]]
NumericVector cumminC(NumericVector x) {
    int n = x.size();
    NumericVector result(1);
    result[0] = x[0];
    for (int i = 1; i < n; i++) {
        if (x[i] <= min(result)) {
            result.push_back(x[i]);
        } else {
            result.push_back(result[i-1]);
        }
    }
    return result;
}

// [[Rcpp::export]]
NumericVector cummaxC(NumericVector x) {
    int n = x.size();
    NumericVector result(1);
    result[0] = x[0];
    for (int i = 1; i < n; i++) {
        if (x[i] >= max(result)) {
            result.push_back(x[i]);
        } else {
            result.push_back(result[i-1]);
        }
    }
    return result;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
require(microbenchmark)
x_mean <- runif(1e3)
microbenchmark(meanC(x_mean), mean(x_mean))

x_all <- 1:10
microbenchmark(all(x_all < 15), allC(x_all < 15))

x_cumsum <- 1:10
microbenchmark(cumsum(x_cumsum), cumsumC(x_cumsum))

x_cumprod <- 1:10
microbenchmark(cumprod(x_cumprod), cumprodC(x_cumprod))

x_cummin <- c(3:1, 2:0, 4:2)
microbenchmark(cummin(x_cummin), cumminC(x_cummin))

x_cummax <- c(3:1, 2:0, 4:2)
microbenchmark(cummax(x_cummax), cummaxC(x_cummax))
*/




