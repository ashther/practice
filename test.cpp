//[[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <numeric>
#include <iostream>
#include <algorithm>
#include <unordered_set>
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
double meanC(NumericVector x, bool na_rm) {
    double result = 0;
    int n = x.size();
    int temp_n = 0;
    
    for (int i = 0; i < n; i++) {
        if (NumericVector::is_na(x[i])) {
            if (na_rm) {
                temp_n += 1;
            } else {
                return NA_REAL;
            }
        } else {
            result += x[i];
        }
    }
    return result / (n - temp_n);
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
NumericVector cumsumC(NumericVector x, bool na_rm) {
    int n = x.size(), j = 0, na_n = 0;
    double temp = 0;
    NumericVector res(n);
    for (int i = 0; i < n; i++) {
        if (!NumericVector::is_na(x[i]) | !na_rm) {
            temp += x[i];
            res[j] = temp;
            j++;
        } else if (NumericVector::is_na(x[i]) & na_rm) {
            na_n += 1;
            continue;
        }
    }
    NumericVector result(n - na_n);
    for (int i = 0; i < (n - na_n); i++) {
        result[i] = res[i];
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

// [[Rcpp::export]]
NumericVector diffC(NumericVector x, int lag) {
    int n = x.size();
    if (lag >= n) {
        return false;
    }
    int m = n - lag;
    NumericVector result(m);
    for (int i = 0; i < m; i++) {
        result[i] = x[i + lag] - x[i];
    }
    return result;
}

//[[Rcpp::export]]
double sumC(NumericVector x) {
    double result = 0;
    NumericVector::iterator it;
    for (it = x.begin(); it != x.end(); ++it) {
        result += *it;
    }
    return result;
}

//[[Rcpp::export]]
double sumC_std(NumericVector x) {
    return std::accumulate(x.begin(), x.end(), 0.0);
}

//[[Rcpp::export]]
IntegerVector findintervalC(NumericVector x, NumericVector breaks) {
    IntegerVector result(x.size());
    NumericVector::iterator it, pos;
    IntegerVector::iterator out;
    
    for (it = x.begin(), out = result.begin(); it != x.end(); ++it, ++out) {
        pos = std::upper_bound(breaks.begin(), breaks.end(), *it);
        *out = std::distance(breaks.begin(), pos);
    }
    return result;
}

//[[Rcpp::export]]
List rleC(NumericVector x) {
    std::vector<double> len_res;
    std::vector<int> val_res;
    NumericVector::iterator it;
    int i = 0;
    double temp = x[0];
    len_res.push_back(1);
    val_res.push_back(temp);
    
    for (it = x.begin() + 1; it != x.end(); ++it) {
        if (*it != temp) {
            val_res.push_back(*it);
            len_res.push_back(1);
            temp = *it;
            i++;
        } else {
            len_res[i] += 1;
        }
    }
    return List::create(_["length"] = len_res, _["value"] = val_res);
}

//[[Rcpp::export]]
double medianC(NumericVector x) {
    int n = x.size();
    std::partial_sort(x.begin(), x.end(), x.end());
    if (n % 2 == 1) {
        return x[n / 2];
    } else {
        return (x[n / 2 - 1] + x[n / 2]) / 2;
    }
}

//[[Rcpp::export]]
LogicalVector valueMatchC(NumericVector x, NumericVector y) {
    std::unordered_set<double> temp;
    LogicalVector result;
    NumericVector::iterator it;
    int pos;
    for (it = y.begin(); it != y.end(); ++it) {
        temp.insert(*it);
    }
    for (it = x.begin(); it != x.end(); ++it) {
        pos = temp.count(*it);
        if (pos == 0) {
            result.push_back(false);
        } else {
            result.push_back(true);
        }
    }
    return result;
}

//[[Rcpp::export]]
unordered_set<double> uniqueC(NumericVector x) {
    std::unordered_set<double> out;
    NumericVector::iterator it;
    for (it = x.begin(); it != x.end(); ++it) {
        out.insert(*it);
    }
    return out;
}

//[[Rcpp::export]]
List minmaxC(NumericVector x) {
    double min_x = x[0], max_x = x[0];
    NumericVector::iterator it;
    for (it = x.begin(); it != x.end(); ++it) {
        min_x = std::min(min_x, *it);
        max_x = std::max(max_x, *it);
    }
    return List::create(_["min"] = min_x, _["max"] = max_x);
}

//[[Rcpp::export]]
double whichminC(NumericVector x) {
    double temp = *std::min_element(x.begin(), x.end());
    for (int i = 0; i < x.size(); ++i) {
        if (x[i] == temp) {
            return i;
        }
    }
    return 0;
}

// [[Rcpp::export]]
NumericMatrix cppgibbs(int N, int thin) {
    NumericMatrix mat(N, 2);
    double x = 0, y = 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < thin; ++j) {
            x = ::Rf_rgamma(3.0, 1.0 / (y * y + 4));
            y = ::Rf_rnorm(1.0 / (x + 1), 1.0 / sqrt(2 * (x + 1)));
        }
        mat(i, 0) = x;
        mat(i, 1) = y;
    }
    return mat;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
whichminC(c(6:1, 2:3))
*/













