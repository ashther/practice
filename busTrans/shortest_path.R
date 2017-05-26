
###############################################################################
# get top k shortest path based on mat
###############################################################################

spFindTopK <- function(mat, from, to, k) {
  require(igraph)
  result <- list()
  variants <- list()
  i <- 1
  g <- graph.adjacency(mat, mode = 'directed', weighted = TRUE)
  
  spd <- shortest.paths(g, from, to, mode = 'out', algorithm = 'dijkstra')
  if (!is.infinite(spd)) {
    k1 <- get.shortest.paths(g, from, to, mode = 'out', output = 'both')
    result <- list(list(g = g, 
                        vert = k1$vpath, 
                        path = k1$epath, 
                        dist = spd))
  } else {
    cat('no shortest path found')
    return(result)
  }
  while(i < k) {
    n <- length(result)
    variants <- variantCaculate(variants, result[[n]], from, to)
    spIdx <- spIdxSelect(variants)
    if (!identical(spIdx, integer(0))) {
      result[[n + 1]] <- variants[[spIdx]]
      variants <- variants[-spIdx]
      i <- i + 1
    } else {
      break
    }
  }
  return(result)
}

spFindTopKResume <- function(mat, from, to, k, result = list(), variants = list()) {
  require(igraph)
  i <- 1
  
  if ((n <- length(result)) == 0) {
    g <- graph.adjacency(mat, mode = 'directed', weighted = TRUE)
    # plot(g, edge.label = E(g)$weight)
    spd <- shortest.paths(g, from, to, mode = 'out', algorithm = 'dijkstra')
    if (!is.infinite(spd)) {
      k1 <- get.shortest.paths(g, from, to, mode = 'out', output = 'both')
      result <- list(list(g = g, 
                          vert = k1$vpath, 
                          path = k1$epath, 
                          dist = spd))
    } else {
      cat('no shortest path found')
      return(result)
    }
  } else {
    k <- k + 1
  }
  
  while(i < k) {
    n <- length(result)
    variants <- variantCaculate(variants, result[[n]], from, to)
    spIdx <- spIdxSelect(variants)
    if (!identical(spIdx, integer(0))) {
      result[[n + 1]] <- variants[[spIdx]]
      variants <- variants[-spIdx]
      i <- i + 1
    } else {
      break
    }
  }
  return(list(result = result, variants = variants))
}

variantCaculate <- function(variants, variant, from, to) {
  g <- variant$g
  for (j in unlist(variant$path)) {
    new_g <- delete.edges(g, j)
    sp <- get.shortest.paths(new_g, from, to, mode = 'out', output = 'both')
    spd <- shortest.paths(new_g, from, to, mode = 'out', algorithm = 'dijkstra')
    if (!is.infinite(spd)) {
      if (!spContains(variants, sp)) {
        variants[[length(variants) + 1]] <- 
          list(g = new_g, vert = sp$vpath, path = sp$epath, dist = spd)
      }
    }
  }
  return(variants)
}

spContains <- function(variants, sp) {
  any(unlist(lapply(variants, function(x)identical(unlist(x$vert), unlist(sp$vpath)))))
}

spIdxSelect <- function(variants) {
  which.min(sapply(variants, function(x)x$dist))
}

###############################################################################
# filter and sort shortest path
###############################################################################

mtxValueGet <- function(vec, mat) {
  if (length(vec) <= 1) {
    return(0)
  }
  vec <- as.character(vec)
  mat[cbind(vec[1:(length(vec) - 1)], vec[2:length(vec)])]
}

# sp <- spFindTopKResume()
spFilter <- function(sp, n, matTrans) {
  idx <- sapply(sp$result, function(x) {
    temp <- unlist(x$vert)
    temp <- temp[-c(1, length(temp))]
    (length(temp) <= n) | ((length(temp) <= (n + 1)) & any(mtxValueGet(temp, matTrans) == 1))
  })
  lapply(sp$result[idx], function(x)list(vert = unname(unlist(x$vert)), dist = x$dist))
}

# sp <- spFilter()
spSort <- function(sp, matTime, matTrans) {
  time_tran <- t(sapply(sp, function(x) {
    vec <- x$vert
    time <- sum(mtxValueGet(vec, matTime))
    tran <- length(vec) - sum(mtxValueGet(vec[-c(1, length(vec))], matTrans)) - 2
    return(c(time = time, tran = tran))
  }))
  orders <- order(time_tran[, 'tran'], time_tran[, 'time'])
  result <- lapply(seq_along(sp), function(x) {
    list(vert = sp[[x]]$vert, 
         tran = time_tran[x, 'tran'], 
         time = time_tran[x, 'time'])
  })
  return(result[orders])
}
