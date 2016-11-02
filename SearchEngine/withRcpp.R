
library(Rcpp)
# sourceCpp('mtxMultCPP.cpp')
# sourceCpp('mtxMultArmCPP.cpp')
# sourceCpp('mtxMultEigenCPP.cpp')
sourceCpp('mtxMultParCPP.cpp')

benchmark <- function(seed = 2016) {
  set.seed(seed)
  temp <- matrix(1, 1, 1e3)
  t_temp <- t(temp)
  test <- matrix(1, 1e3, 1e5)
  
  print(
    microbenchmark::microbenchmark(
      mtxMultCPP(temp, test),
      mtxMultParCPP(temp, test),
      crossprod(t_temp, test), 
      times = 10)
  )
  
  rm(temp, t_temp, test)
  invisible(gc())
  
  # Unit: milliseconds
  #                       expr      min       lq     mean   median       uq       max neval
  #     mtxMultCPP(temp, test) 92.32242 93.35680 94.61640 94.45920 95.45292  99.63526   100
  #  mtxMultParCPP(temp, test) 67.34526 67.66152 68.34254 68.04931 68.67653  71.57881   100
  #    crossprod(t_temp, test) 92.30385 93.55285 94.72991 94.56995 95.49165 100.31737   100
}






