

reachFindPar <- function(mat, from, to, matLines, matTime) {
  if (mat[from, to] == 1) {
    lines <- rownames(matLines)[matLines[, from] == 1 & matLines[, to] == 1]
    time <- sapply(lines, function(x)matTime[[x]][from, to])
    return(
      matrix(c(rep(paste(from, to, sep = ','), length(lines)), 
               lines, rep(0, length(lines)), unname(time)), 
             ncol = 4, dimnames = list(NULL, c('desc', 'line', 'tran', 'time')))
    )
  } else {
    return(NULL)
  }
}


tran1FindPar <- function(mat, from, to, matLines, matTime, dfLines, matManDist, p) {
  stops <- rownames(mat)[mat[from, ] == 1 & mat[, to] == 1]
  from_to <- matManDist[from, to]
  stops <- stops[
    matManDist[from, stops] <= from_to * p & 
      matManDist[to, stops] <= from_to * p
    ]
  do.call(rbind, 
          lapply(stops, function(x) {
            line_a <- reachFindPar(mat, from, x, matLines, matTime)
            line_b <- reachFindPar(mat, x, to, matLines, matTime)
            temp <- expand.grid(line_a[, 'line'], line_b[, 'line'], 
                                stringsAsFactors = FALSE)
            temp <- temp[temp[, 1] != temp[, 2], , drop = FALSE]
            if (nrow(temp) == 0) {
              return(NULL)
            }
            time <- apply(temp, 1, function(y) {
              as.numeric(line_a[line_a[, 'line'] == y[1], 'time']) + 
                as.numeric(line_b[line_b[, 'line'] == y[2], 'time'])
            })
            cbind(desc = rep(paste(from, x, to, sep = ','), nrow(temp)), 
                  line = paste(temp[, 1], temp[, 2], sep = ','), 
                  tran = rep('1', nrow(temp)), 
                  time = time)
          }))
}


tran2FindPar <- function(mat, from, to, matLines, matTime, dfLines, matManDist, p) {
  stop_from <- rownames(mat)[mat[from, ] == 1]
  stop_to <- rownames(mat)[mat[, to] == 1]
  stop_combn <- as.matrix(expand.grid(stop_from, stop_to))
  stop_combn <- stop_combn[mat[stop_combn] == 1, ]
  
  from_to <- matManDist[from, to]
  from_stop1 <- matManDist[from, stop_combn[, 1]]
  from_stop2 <- matManDist[from, stop_combn[, 2]]
  stop1_to <- matManDist[stop_combn[, 1], to]
  stop2_to <- matManDist[stop_combn[, 2], to]
  stop1_stop2 <- matManDist[stop_combn]
  stop_combn <- stop_combn[
    from_stop1 <= from_stop2 * p & 
      stop1_stop2 <= from_stop2 * p &
      stop1_stop2 <= stop1_to * p &
      stop2_to <= stop1_to * p & 
      from_stop1 <= from_to * p & 
      stop1_to <= from_to * p & 
      from_stop2 <= from_to * p & 
      stop2_to <= from_to * p, 
    , drop = FALSE
    ]
  
  do.call(rbind, 
          lapply(seq_len(nrow(stop_combn)), function(x) {
            line_a <- reachFindPar(mat, from, stop_combn[x, 1], matLines, matTime)
            line_b <- reachFindPar(mat, stop_combn[x, 1], stop_combn[x, 2], matLines, matTime)
            line_c <- reachFindPar(mat, stop_combn[x, 2], to, matLines, matTime)
            temp <- expand.grid(line_a[, 'line'], 
                                line_b[, 'line'], 
                                line_c[, 'line'], stringsAsFactors = FALSE)
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
            cbind(desc = rep(paste(from, stop_combn[x, 1], stop_combn[x, 2], to, sep = ','), nrow(temp)), 
                  line = paste(temp[, 1], temp[, 2], temp[, 3], sep = ','), 
                  tran = rep('2', nrow(temp)), 
                  time = time)
          }))
}
