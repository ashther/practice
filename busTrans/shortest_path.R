
topKSp <- function(mat, from, to, k) {
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
