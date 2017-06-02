
# convert to distance matrix
stopFilter <- function(dfLines, from_to, to, stops, p = 1) {
  to_lon <- dfLines[dfLines$station == to, 2][1]
  to_lat <- dfLines[dfLines$station == to, 3][1]
  stops_lon <- unique(dfLines[dfLines$station %in% stops, 2])
  stops_lat <- unique(dfLines[dfLines$station %in% stops, 3])
  (abs(stops_lon - to_lon) + abs(stops_lat - to_lat)) <= from_to*p
}

# from <- '西直门'
# to <- '东直门'

############################################################
#                                                          #
#                            0                             #
#                                                          #
############################################################

# reachFind <- function(mat, from, to, matLines, listLines) {
#   if (mat[from, to] == 1) {
#     lines <- rownames(matLines)[matLines[, from] == 1 & matLines[, to] == 1]
#     time <- stopBetween(listLines, from, to, lines, loop_lines = c('2', '10'))
#     return(
#       matrix(c(rep(paste(from, to, sep = ','), length(lines)), 
#                lines, rep(0, length(lines)), unname(time)), 
#              ncol = 4, dimnames = list(NULL, c('desc', 'line', 'tran', 'time')))
#     )
#   } else {
#     return(NULL)
#   }
# }

reachFind <- function(mat, from, to, matLines, matTime) {
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

############################################################
#                                                          #
#                            1                             #
#                                                          #
############################################################

# from <- '立水桥'
# to <- '安定门'

tran1Find <- function(mat, from, to, matLines, matTime, dfLines) {
  stops <- rownames(mat)[mat[from, ] == 1 & mat[, to] == 1]
  from_to <- sum(abs(dfLines[dfLines$station == from, 2:3][1, ] -
                       dfLines[dfLines$station == to, 2:3][1, ]))
  stops <- stops[stopFilter(dfLines, from_to, to, stops, p = 1)]
  do.call(rbind, 
          lapply(stops, function(x) {
            line_a <- reachFind(mat, from, x, matLines, matTime)
            line_b <- reachFind(mat, x, to, matLines, matTime)
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

tran2Find <- function(mat, from, to, matLines, matTime, dfLines) {
  stop_from <- rownames(mat)[mat[from, ] == 1]
  stop_to <- rownames(mat)[mat[, to] == 1]
  stop_combn <- as.matrix(expand.grid(stop_from, stop_to))
  stop_combn <- stop_combn[mat[stop_combn] == 1, ]
  
  from_to <- sum(abs(dfLines[dfLines$station == from, 2:3][1, ] -
                       dfLines[dfLines$station == to, 2:3][1, ]))
  stop_combn <- stop_combn[stopFilter(dfLines, from_to, to, stop_combn[, 1], p = 1), ]
  
  do.call(rbind, 
          lapply(seq_len(nrow(stop_combn)), function(x) {
            line_a <- reachFind(mat, from, stop_combn[x, 1], matLines, matTime)
            line_b <- reachFind(mat, stop_combn[x, 1], stop_combn[x, 2], matLines, matTime)
            line_c <- reachFind(mat, stop_combn[x, 2], to, matLines, matTime)
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
            cbind(rep(paste(from, stop_combn[x, 1], stop_combn[x, 2], to, sep = ','), nrow(temp)), 
                  paste(temp[, 1], temp[, 2], temp[, 3], sep = ','), 
                  rep('2', nrow(temp)), 
                  time)
          }))
}

