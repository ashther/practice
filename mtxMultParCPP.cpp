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
    
    for (std::size_t i = begin; i < end; i++) {
      for (int j = 0; j < y.ncol(); j++) {
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
  
  parallelFor(0, x.nrow(), mtxMult);
  
  return result;
}