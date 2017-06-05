
require(parallel)
cl <- makeCluster(4)

vec <- unique(dfLines$station)

clusterExport(cl, 'vec', environment())
clusterExport(cl, 'mat', environment())
clusterExport(cl, 'matLines', environment())
clusterExport(cl, 'dfLines', environment())
clusterExport(cl, 'matTime', environment())
clusterExport(cl, 'matManDist', environment())
clusterEvalQ(cl, require(magrittr))
clusterEvalQ(cl, source('parallel.R'))


test <- pbapply::pblapply(cl = cl, vec, function(from) {
  temp <- lapply(vec, function(to) {
    if (from == to) {
      return(NULL)
    }
    tryCatch({
      result <- rbind(
        reachFindPar(mat, from, to, matLines, matTime),
        tran1FindPar(mat, from, to, matLines, matTime, dfLines, matManDist, p = 1.1),
        tran2FindPar(mat, from, to, matLines, matTime, dfLines, matManDist, p = 1.1)
      )
      data.frame(
        desc = result[, 'desc'], 
        line = result[, 'line'], 
        tran = result[, 'tran'], 
        time = result[, 'time'], 
        stringsAsFactors = FALSE, 
        row.names = NULL
      )
    }, 
      error = function(e)cat(from, to, '\n')
    )
  })
  names(temp) <- vec
  temp
})
names(test) <- vec

# jsonlite::toJSON(test,pretty = TRUE) %>% writeLines('test.json', useBytes = TRUE)

test <- pbapply::pblapply(vec, function(from) {
  temp <- lapply(vec, function(to) {
    if (from == to) {
      return(NULL)
    }
    tryCatch({
      result <- rbind(
        reachFind(mat, from, to, matLines, matTime),
        tran1Find(mat, from, to, matLines, matTime, dfLines, matManDist, p = 1.1),
        tran2Find(mat, from, to, matLines, matTime, dfLines, matManDist, p = 1.1)
      )
      data.frame(
        desc = result[, 'desc'], 
        line = result[, 'line'], 
        tran = result[, 'tran'], 
        time = result[, 'time'], 
        stringsAsFactors = FALSE, 
        row.names = NULL
      )
    }, 
      error = function(e)cat(from, to, '\n', e$message, '\n')
    )
  })
  names(temp) <- vec
  temp
})
names(test) <- vec
