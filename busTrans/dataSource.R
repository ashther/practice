
############################################################
#                                                          #
#              get mat, matTime and matTrans               #
#                                                          #
############################################################

library(igraph)
matTime <- as.matrix(read.table(text=
                              "node X1 X2 X3 X4 X5 X6
                            1  0  3  7  4 NA NA
                            2  3  0  2 NA NA  9
                            3  7  2  0  1  3  6
                            4  4 NA  1  0  3 NA
                            5 NA NA  3  3  0  3
                            6 NA  9  6 NA  3  0", header=T))
rownames(matTime) <- matTime[,1]
matTime <- matTime[, -1]
colnames(matTime) <- rownames(matTime)
matTime[is.na(matTime)] <- 0

mat <- matTime
mat[mat > 0] <- 1

matTrans <- matTime
matTrans <- matTrans * 0
matTrans[rbind(c('4', '3'), c('5', '6'))] <- 1


############################################################
#                                                          #
#             read beijing subway information              #
#                                                          #
############################################################

bj <- lapply(list.files('beijing_subway/',full.names = TRUE), function(x) {
  tryCatch(
    temp <- read.table(x, sep = ',', stringsAsFactors = FALSE), 
    error = function(e)cat(x)
  )
  temp$line <- gsub('line|\\.txt', '', basename(x))
  temp
}) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c('station', 'lon', 'lat', 'line'))

duplicated_station <- bj %>% 
  group_by(station) %>% 
  tally(sort = TRUE) %>% 
  filter(n > 1) %>% 
  extract2('station')

mat <- matrix(0, nrow = length(unique(bj$station)), ncol = length(unique(bj$station)), 
              dimnames = list(unique(bj$station), unique(bj$station)))
matTime <- mat
matTrans <- mat

invisible(lapply(unique(bj$line), function(x) {
  mat[t(combn(bj[bj$line == x, 'station'], 2))] <<- 1
}))
mat <- mat + t(mat)
mat[mat > 1] <- 1

stationBetween <- function(dfLines, i, j, loop_lines = NULL) {
  sapply(unique(dfLines$line), function(l) {
    stations <- dfLines$station[dfLines$line == l]
    if (all(c(i, j) %in% stations)) {
      if (!is.null(loop_lines) & l %in% loop_lines) {
        min(
          abs(which(stations == j) - which(stations == i)), 
          (length(stations) - abs(which(stations == j) - which(stations == i)))
        )
      } else {
        abs(which(stations == j) - which(stations == i))
      }
    } else {
      NA
    }
  })
}

for (i in rownames(matTime)) {
  for (j in colnames(matTime)) {
    if (mat[i, j] == 0) {
      next
    } else {
      temp <- stationBetween(bj, i, j, loop_lines = c('2', '10')) %>% 
        min(na.rm = TRUE)
      matTime[i, j] <- temp * 3
    }
  }
}

matLines <- matrix(0, nrow = length(unique(dfLines$line)), ncol = length(unique(dfLines$station)), 
                   dimnames = list(unique(dfLines$line), unique(dfLines$station)))
matLines[cbind(dfLines$line, dfLines$station)] <- 1

listLines <- lapply(unique(dfLines$line), function(x)dfLines$station[dfLines$line == x])
names(listLines) <- unique(dfLines$line)

############################################################
#                                                          #
#              compare REmap and RBaidulbs                 #
#                                                          #
############################################################

# readClipboard() %>% 
# gsub('[0-9]', '', .) %>% 
# stringr::str_trim() %>% 
# strsplit(' ') %>% 
# lapply(function(x)x[1]) %>% 
# unlist() %>% 
# writeLines('line0.txt')
bj_subway_files <- c('beijing_subway/line0.txt', 'beijing_subway/line1.txt', 
                     'beijing_subway/line2.txt', 'beijing_subway/line4.txt')
bj_subway <- lapply(bj_subway_files, function(x) {
  read.table(x, sep = ',', stringsAsFactors = FALSE)
}) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c('station', 'lon', 'lat')) %>% 
  filter(!duplicated(station))

remap_gps <- pbapply::pblapply(bj_subway$station, function(x) {
  get_city_coord(paste0('北京', x, '地铁站'))
}) %>% 
  do.call(rbind, .)
rbaidulbs_gps <- pbapply::pblapply(bj_subway$station, function(x) {
  temp <- getGeocoding(paste0(x, '地铁站'), '北京')
  if (nrow(temp) == 0) {
    return(c(NA, NA))
  }
  return(temp[, c('lng', 'lat')])
}) %>% 
  do.call(rbind, .)

bj_subway$remap_lon <- remap_gps[, 1]
bj_subway$remap_lat <- remap_gps[, 2]
bj_subway$rbaidulbs_lon <- rbaidulbs_gps$lng
bj_subway$rbaidulbs_lat <- rbaidulbs_gps$lat

remap_error <- apply(filter(bj_subway, !is.na(rbaidulbs_lat)), 1, function(x) {
  distm(as.numeric(x[c('lon', 'lat')]), as.numeric(x[c('remap_lon', 'remap_lat')]))
})
rbaidulbs_error <- apply(filter(bj_subway, !is.na(rbaidulbs_lat)), 1, function(x) {
  distm(as.numeric(x[c('lon', 'lat')]), as.numeric(x[c('rbaidulbs_lon', 'rbaidulbs_lat')]))
})
