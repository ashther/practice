
library(dplyr)
library(magrittr)
library(memoise)

stopBetween <- function(dfLines, from, to, lines, loop_lines) {
  sapply(lines, function(l) {
    stop <- dfLines$station[dfLines$line == l]
    if (l %in% loop_lines) {
      min(
        abs(which(stop == from) - which(stop == to)), 
        (length(stop) - abs(which(stop == from) - which(stop == to)))
      )
    } else {
      abs(which(stop == from) - which(stop == to))
    }
  })
}

stopBetweenTest <- function(listLines, from, to, lines, loop_lines) {
  sapply(lines, function(l) {
    stop <- listLines[[l]]
    if (l %in% loop_lines) {
      min(
        abs(which(stop == from) - which(stop == to)), 
        (length(stop) - abs(which(stop == from) - which(stop == to)))
      )
    } else {
      abs(which(stop == from) - which(stop == to))
    }
  })
}

# from <- '西直门'
# to <- '东直门'

############################################################
#                                                          #
#                            0                             #
#                                                          #
############################################################

reachFind <- function(mat, from, to, matLines, listLines) {
  if (mat[from, to] == 1) {
    lines <- rownames(matLines)[matLines[, from] == 1 & matLines[, to] == 1]
    time <- stopBetweenTest(listLines, from, to, lines, loop_lines = c('2', '10'))
    return(
      matrix(c(rep(paste(from, to, sep = ','), length(lines)), 
               lines, rep(0, length(lines)), unname(time)), 
             ncol = 4, dimnames = list(NULL, c('desc', 'line', 'tran', 'time')))
    )
  } else {
    return(NULL)
  }
}

############################################################
#                                                          #
#                            1                             #
#                                                          #
############################################################

# from <- '立水桥'
# to <- '安定门'

tran1Find <- function(mat, from, to, matLines, listLines) {
  stops <- rownames(mat)[mat[from, ] == 1 & mat[, to] == 1]
  do.call(rbind, 
          lapply(stops, function(x) {
            line_a <- reachFind(mat, from, x, matLines, listLines)
            line_b <- reachFind(mat, x, to, matLines, listLines)
            temp <- as.matrix(expand.grid(line_a[, 'line'], line_b[, 'line']))
            temp <- temp[temp[, 1] != temp[, 2], , drop = FALSE]
            if (nrow(temp) == 0) {
              return(NULL)
            }
            time <- apply(temp, 1, function(y) {
              as.numeric(line_a[line_a[, 'line'] == y[1], 'time']) + 
                as.numeric(line_b[line_b[, 'line'] == y[2], 'time'])
            })
            cbind(rep(paste(from, x, to, sep = ','), nrow(temp)), 
                  paste(temp[, 1], temp[, 2], sep = ','), 
                  rep('1', nrow(temp)), 
                  time)
          }))
}

############################################################
#                                                          #
#                            2                             #
#                                                          #
############################################################

# from <- '北苑'
# to <- '安定门'

tran2Find <- function(mat, from, to, matLines, listLines) {
  stop_from <- rownames(mat)[mat[from, ] == 1]
  stop_to <- rownames(mat)[mat[, to] == 1]
  stop_combn <- as.matrix(expand.grid(stop_from, stop_to))
  stop_combn <- stop_combn[mat[stop_combn] == 1, ]
  do.call(rbind, 
          lapply(seq_len(nrow(stop_combn)), function(x) {
            line_a <- reachFind(mat, from, stop_combn[x, 1], matLines, listLines)
            line_b <- reachFind(mat, stop_combn[x, 1], stop_combn[x, 2], matLines, listLines)
            line_c <- reachFind(mat, stop_combn[x, 2], to, matLines, listLines)
            temp <- as.matrix(expand.grid(line_a[, 'line'], 
                                          line_b[, 'line'], 
                                          line_c[, 'line']))
            temp <- temp[temp[, 1] != temp[, 2] & 
                           temp[, 2] != temp[, 3], , drop = FALSE]
            if (nrow(temp) == 0) {
              return(NULL)
            }
            time <- apply(temp, 1, function(y) {
              as.numeric(line_a[line_a[, 'line'] == y[1], 'time']) + 
                as.numeric(line_b[line_b[, 'line'] == y[2], 'time']) + 
                as.numeric(line_c[line_c[, 'line'] == y[3], 'time'])
            })
            cbind(rep(paste(from, stop_combn[x, 1], stop_combn[x, 2], to, sep = ','), nrow(temp)), 
                  paste(temp[, 1], temp[, 2], temp[, 3], sep = ','), 
                  rep('2', nrow(temp)), 
                  time)
          }))
}

invisible(pbapply::pblapply(unique(dfLines$station)[1:10], function(x) {
  lapply(unique(dfLines$station)[1:10], function(y) {
    tryCatch(
      rbind(
        reachFind(mat, x, y, matLines, listLines),
        tran1Find(mat, x, y, matLines, listLines),
        tran2Find(mat, x, y, matLines, listLines)
      ), 
      error = function(e)cat(x, y, '\n')
    )
  })
}))

require(parallel)
cl <- makeCluster(4)

clusterExport(cl, 'mat', environment())
clusterExport(cl, 'matLines', environment())
clusterExport(cl, 'listLines', environment())
clusterExport(cl, 'dfLines', environment())
clusterEvalQ(cl, require(igraph))
clusterEvalQ(cl, require(magrittr))
clusterEvalQ(cl, require(dplyr))
clusterEvalQ(cl, source('parallel.R'))
