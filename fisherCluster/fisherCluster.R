
library(magrittr)

getMtxValue <- function(mtx, i, j) {
  i <- as.character(i)
  j <- as.character(j)
  if (!i %in% rownames(mtx) | !j %in% colnames(mtx)) {
    return(0)
  } else {
    return(mtx[i, j])
  }
}

# seq_data <- c(9.3, 1.8, 1.9, 1.7, 1.5, 1.3, 1.4, 2.0, 1.9, 2.3, 2.1)
dijCaculate <- function(seq_data, i, j) {
  avg <- mean(seq_data[i:j], na.rm = TRUE)
  round(sum((seq_data[i:j] - avg) ^ 2), 3)
}

dMtxCreate <- function(seq_data) {
  require(pbapply)
  len <- length(seq_data) - 1
  dMtx <- matrix(0, len, len, dimnames = list(2:(len + 1), seq_len(len)))
  
  pb <- startpb(0, len)
  on.exit(closepb(pb))
  for (k in seq_len(len)) {
    for (n in k:len) {
      dMtx[n, k] <- dijCaculate(seq_data, k, n + 1)
    }
    setpb(pb, k)
  }
  return(dMtx)
}

minLossSplit <- function(dMtx, min_loss_mtx, n, k) {
  j_seq <- seq(k, n)
  if (k == 2L) {
    result <- sapply(j_seq, function(j) {
      if (1 == (j - 1)) {
        return(dMtx[as.character(n), as.character(j)])
      } else if (n == j) {
        return(dMtx[as.character(j - 1), '1'])
      } else {
        return(dMtx[as.character(j - 1), '1'] + dMtx[as.character(n), as.character(j)])
      }
    })
  } else {
    result <- sapply(j_seq, function(j) {
      if (j == n) {
        return(getMtxValue(min_loss_mtx, j - 1, k - 1))
      } else {
        return(getMtxValue(min_loss_mtx, j - 1, k - 1) + 
                 dMtx[as.character(n), as.character(j)])
      }
    })
  }
  return(list(value = min(result), idx = j_seq[which.min(result)]))
}

lossMtxCreate <- function(dMtx) {
  require(pbapply)
  len <- nrow(dMtx) - 1
  min_loss_mtx <- matrix(0, len, len, dimnames = list(rownames(dMtx)[-1], colnames(dMtx)[-1]))
  min_loss_idx <- min_loss_mtx
  
  pb <- startpb(0, len)
  on.exit(closepb(pb))
  for(k in colnames(min_loss_mtx)) {
    for(n in rownames(min_loss_mtx)[which(colnames(min_loss_mtx) == k):length(rownames(min_loss_mtx))]) {
      # min_loss <- minLossSplitRcr(dMtx, as.integer(n), as.integer(k))
      # min_loss_mtx[n, k] <- min_loss$value
      # min_loss_idx[n, k] <- min_loss$idx
      
      min_loss <- minLossSplit(dMtx, min_loss_mtx, as.integer(n), as.integer(k))
      min_loss_mtx[n, k] <- min_loss$value
      min_loss_idx[n, k] <- min_loss$idx
    }
    setpb(pb, which(k == colnames(min_loss_mtx)))
  }
  return(list(value = min_loss_mtx, idx = min_loss_idx))
}

lossMtxCreateCpp <- function(dMtx) {
  require(pbapply)
  len <- nrow(dMtx) - 1
  min_loss_mtx <- matrix(0, len, len, dimnames = list(rownames(dMtx)[-1], colnames(dMtx)[-1]))
  min_loss_idx <- min_loss_mtx
  
  pb <- startpb(0, len)
  on.exit(closepb(pb))
  for(k in colnames(min_loss_mtx)) {
    for(n in rownames(min_loss_mtx)[which(colnames(min_loss_mtx) == k):length(rownames(min_loss_mtx))]) {
      min_loss <- minLossSplitCpp(dMtx, min_loss_mtx, as.integer(n), as.integer(k))
      min_loss_mtx[n, k] <- min_loss$value
      min_loss_idx[n, k] <- min_loss$idx
    }
    setpb(pb, which(k == colnames(min_loss_mtx)))
  }
  return(list(value = min_loss_mtx, idx = min_loss_idx))
}

fisherClust <- function(lossMtx, k = 3) {
  name_mtx <- rownames(lossMtx$idx)
  len_rownames <- length(name_mtx)
  n <- name_mtx[len_rownames]
  cluster <- list()
  loss <- 0
  while (k > 1) {
    if (k == as.integer(n)) {
      for (i in rev(seq_len(k))) {
        cluster[[as.character(i)]] <- as.character(i)
      }
      break
    }
    
    c_start <- lossMtx$idx[n, as.character(k)]
    if (c_start == 2L) {
      cluster[[as.character(k)]] <- c('2', name_mtx[1:which(name_mtx == n)])
      loss <- loss + lossMtx$value[n, as.character(k)]
      cluster[['1']] <- '1'
      break
    } else {
      cluster[[as.character(k)]] <- 
        name_mtx[which(name_mtx == as.character(c_start)):which(name_mtx == n)]
      loss <- loss + lossMtx$value[n, as.character(k)]
      k <- k - 1
      n <- name_mtx[which(name_mtx == as.character(c_start)) - 1]
      if (k == 1L) {
        if (identical(n, character(0))) {
          cluster[['1']] <- as.character(1:(c_start - 1))
        } else {
          cluster[['1']] <- c('1', '2', name_mtx[1:which(name_mtx == n)])
        }
        break
      }
    }
  }
  return(list(cluster = cluster, loss = loss))
}

# minLossSplitRcr <- function(dMtx, n, k) {
#   j_seq <- seq(k, n)
#   if (k == 2L) {
#     result <- sapply(j_seq, function(j) {
#       if (1 == (j - 1)) {
#         return(dMtx[as.character(n), as.character(j)])
#       } else if (n == j) {
#         return(dMtx[as.character(j - 1), '1'])
#       } else {
#         return(dMtx[as.character(j - 1), '1'] + dMtx[as.character(n), as.character(j)])
#       }
#     })
#   } else {
#     result <- sapply(j_seq, function(j) {
#       if (j == n) {
#         return(minLossSplit(dMtx, j - 1, k - 1)$value)
#       } else {
#         return(minLossSplit(dMtx, j - 1, k - 1)$value + dMtx[as.character(n), as.character(j)])
#       }
#     })
#   }
#   return(list(value = min(result), idx = j_seq[which.min(result)]))
# }
