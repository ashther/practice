#include <Rcpp.h>
using namespace Rcpp;

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


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
require(microbenchmark)
x_mean <- runif(1e3)
microbenchmark(meanC(x_mean), mean(x_mean))
x_all <- 1:10
allC(x_all < 15)
*/
