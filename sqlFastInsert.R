insertFast <- function(df, con) {
  library(DBI)
  library(luzlogr)
  
  tryCatch({
    qry <- sqlAppendTable(con, "script", df, row.names = FALSE)
    res <- dbSendStatement(con, qry)
    dbClearResult(res)
    printlog(sprintf('inserted success: %s', nrow(df)))
  }, error = function(e) {
    printlog('error: %s', e$message)
  })
}
