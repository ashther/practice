library(parallel)
library(foreach)
library(doMC)
bigMtx <- bigmemory::attach.big.matrix('test.desc')

# -------- parallel
cl <- makeCluster(detectCores() - 1, type = 'FORK')

system.time(
  parSapply(cl, seq_len(ncol(bigMtx)), function(x) {
    sum(query[, 1] * bigMtx[, x])
  })
)

stopCluster(cl)

# -------- doMC
registerDoMC(detectCores() - 1)

system.time(
  foreach(i = seq_len(ncol(bigMtx)), .combine = c) %dopar% {
    sum(query[, 1] * bigMtx[, i])
  }
)

system.time(
  sapply(seq_len(ncol(bigMtx)), function(i) {
    sum(query[, 1] * bigMtx[, i])
  })
)