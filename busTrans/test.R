
require(parallel)
cl <- makeCluster(4)

vec <- unique(dfLines$station)[1:10]

clusterExport(cl, 'vec', environment())
clusterExport(cl, 'mat', environment())
clusterExport(cl, 'matLines', environment())
clusterExport(cl, 'dfLines', environment())
clusterExport(cl, 'matTime', environment())
clusterExport(cl, 'matManDist', environment())
clusterEvalQ(cl,require(magrittr))
clusterEvalQ(cl, source('based_on_database.R'))


test <- pbapply::pblapply(cl = cl, vec, function(from) {
  temp <- lapply(vec, function(to) {
    if (from == to) {
      return(NULL)
    }
    tryCatch(
      set_colnames(
        as.data.frame(
          rbind(
            reachFind(mat, from, to, matLines, matTime),
            tran1Find(mat, from, to, matLines, matTime, dfLines, matManDist, p = 1.1),
            tran2Find(mat, from, to, matLines, matTime, dfLines, matManDist, p = 1.1)
          ), stringsAsFactors = FALSE, row.names = FALSE
        ), c('desc', 'line', 'trans', 'time')
      ), 
      error = function(e)cat(from, to, '\n')
    )
  })
  names(temp) <- vec
  temp
})
names(test) <- vec

# toJSON(test,pretty = TRUE) %>% writeLines('test.json', useBytes = TRUE)
