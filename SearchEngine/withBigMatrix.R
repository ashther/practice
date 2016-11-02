library(bigmemory)
library(bigalgebra)
library(Matrix)
library(Rcpp)
sourceCpp('mtxMultParCPP.cpp')

set.seed(1)
temp <- rnorm(1e3*1e4, mean = -1, sd = 1)
temp[temp < 0] <- 0

baseMtx <- matrix(temp, nrow = 1e3, ncol = 1e4)
bigMtx <- as.big.matrix(baseMtx, type = 'double',
                        backingfile = 'test.bin', descriptorfile = 'test.desc')
spMtx <- Matrix(baseMtx, sparse = TRUE)
rm(temp)

query <- matrix(1, 1e3)
query_for_big <- t(query)

microbenchmark::microbenchmark(
  baseResult = crossprod(query, baseMtx),
  baseResultCpp = mtxMultCPP(query_for_big, baseMtx),
  baseResultCppPar = mtxMultParCPP(query_for_big, baseMtx), 
  bigResult = query_for_big %*% bigMtx,
  bigResultPar = {
    # cl <- makeCluster(detectCores() - 1, type = 'FORK')
    parSapply(cl, seq_len(ncol(bigMtx)), function(x) {
      sum(query[, 1] * bigMtx[, x])
    })
    # stopCluster(cl)
  }, 
  spResult = crossprod(query, spMtx)
)

