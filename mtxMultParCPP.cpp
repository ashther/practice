#include <RcppParallel.h>
#include <Rcpp.h>
//[[Rcpp::depends(RcppParallel)]]

using namespace Rcpp;
using namespace RcppParallel;

struct MtxMult: public Worker {
  const RMatrix<double> x;
  const RMatrix<double> y;
  
  RMatrix<double> result;
  
  
  MtxMult(const NumericMatrix x, const NumericMatrix y, NumericMatrix result)
    : x(x), y(y), result(result) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    
    for (std::size_t j = begin; j < end; j++) {
      for (int i = 0; i < x.nrow(); i++) {
        double temp = 0;
        for (int k = 0; k < x.ncol(); k++) {
          temp += x(i, k) * y(k, j);
        }
        result(i, j) = temp;
      }
    }
    
  }
};

//[[Rcpp::export]]
NumericMatrix mtxMultParCPP(NumericMatrix x, NumericMatrix y) {
  NumericMatrix result(x.nrow(), y.ncol());
  
  MtxMult mtxMult(x, y, result);
  
  parallelFor(0, y.ncol(), mtxMult);
  
  return result;
}

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