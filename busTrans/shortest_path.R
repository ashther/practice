
suppressWarnings2 <- function(expr, regex = character()) {
  withCallingHandlers(expr, warning = function(w) {
    if (any(grepl(regex, w))) {
      invokeRestart('muffleWarning')
    }
  })
}

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
      k1 <- suppressWarnings2(
        get.shortest.paths(g, from, to, mode = 'out', output = 'both'), 
        regex = "Couldn't reach some vertices"
      )
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

variantCaculate <- memoise(
  function(variants, variant, from, to) {
    g <- variant$g
    for (j in unlist(variant$path)) {
      new_g <- delete.edges(g, j)
      sp <- suppressWarnings2(
        get.shortest.paths(new_g, from, to, mode = 'out', output = 'both'), 
        regex = "Couldn't reach some vertices"
      )
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
)

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
    temp <- names(unlist(x$vert))
    temp <- temp[-c(1, length(temp))]
    (length(temp) <= n) | 
      ((length(temp) <= (n + 1)) & any(mtxValueGet(temp, matTrans) == 1))
  })
  lapply(sp$result[idx], function(x)list(vert = unlist(x$vert), dist = x$dist))
}

# sp <- spFilter()
spSort <- function(sp, matTime, matTrans, by = c('tran', 'time')) {
  time_tran <- t(sapply(sp, function(x) {
    vec <- names(x$vert)
    time <- sum(mtxValueGet(vec, matTime))
    tran <- length(vec) - sum(mtxValueGet(vec, matTrans)) - 2
    # tran <- length(vec) - sum(mtxValueGet(vec[-c(1, length(vec))], matTrans)) - 2
    return(c(time = time, tran = tran))
  }))
  orders <- order(time_tran[, by[1]], time_tran[, by[2]])
  result <- lapply(seq_along(sp), function(x) {
    c(paste0(names(sp[[x]]$vert), collapse = ','),
      time_tran[x, 'tran'],
      time_tran[x, 'time'])
  }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    set_colnames(c('sp', 'tran', 'time'))
  result$tran <- as.integer(result$tran)
  result$time <- as.numeric(result$time)
  return(result[orders, ])
}

# sp <- spSort()
spTranslate <- function(sp, dfLines) {
  sp$lines <- apply(sp, 1, function(x) {
    temp <- unlist(strsplit(x[1], ','))
    temp <- cbind(temp[1:(length(temp) - 1)], 
                  temp[2:length(temp)])
    temp <- apply(temp, 1, function(x) {
      stationBetween(dfLines, x[1], x[2], loop_lines = c('2', '10')) %>% {
        names(.)[which.min(.)]
      }
    })
    if (length(temp) == 1) {
      return(paste0(temp, collapse = ','))
    } else if (any(temp[1:(length(temp) - 1)] == temp[2:length(temp)])) {
      return(NA)
    } else {
      return(paste0(temp, collapse = ','))
    }
  })
  distinct(filter(sp, !is.na(lines)))
}

start <- Sys.time()
sp <- spFindTopKResume(mat, '北苑', '安定门', 15)
cat(Sys.time() - start)
start <- Sys.time()
sp <- spFilter(sp, 2, matTrans)
cat(Sys.time() - start)
start <- Sys.time()
sp <- spSort(sp, matTime, matTrans)
cat(Sys.time() - start)
start <- Sys.time()
sp <- spTranslate(sp, dfLines)
cat(Sys.time() - start)

pbapply::pblapply(unique(dfLines$station), FUN = function(x) {
  lapply(unique(dfLines$station), function(y) {
    if (x == y) {
      return(NA)
    }
    sp <- spFindTopKResume(mat, x, y, 10)
    sp <- spFilter(sp, 2, matTrans)
    if (length(sp) <= 0) {
      return(NA)
    }
    sp <- spSort(sp, matTime, matTrans) 
    spTranslate(sp, dfLines)
  })
}) %>% 
  invisible()

