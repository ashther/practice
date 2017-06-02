
require(parallel)
cl <- makeCluster(4)

clusterExport(cl, 'mat', environment())
clusterExport(cl, 'matLines', environment())
clusterExport(cl, 'dfLines', environment())
clusterExport(cl, 'matTime', environment())
clusterEvalQ(cl, source('based_on_database.R'))

test <- pbapply::pblapply(cl = cl, unique(dfLines$station), function(x) {
  lapply(unique(dfLines$station), function(y) {
    tryCatch(
      rbind(
        reachFind(mat, x, y, matLines, matTime),
        tran1Find(mat, x, y, matLines, matTime, dfLines),
        tran2Find(mat, x, y, matLines, matTime, dfLines)
      ), 
      error = function(e)cat(x, y, '\n')
    )
  })
})
