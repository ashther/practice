
library(httr)
library(purrr)
library(RMySQL)

pool <- pool::dbPool(RMySQL::MySQL(),
                     host = '192.168.15.128', 
                     port = 3306, 
                     username = 'api_card', 
                     password = '123456', 
                     dbname = 'utqe')
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)

ori_rows <- map_dbl(dbListTables(pool), function(x) {
  dbGetQuery(pool, sprintf('select count(*) from %s', x))[1, 1]
})

api_rows <- map_dbl(dbListTables(pool), function(x) {
  temp <- GET(url = 'http://localhost:8001/utqe_table_read', 
              query = list(table = x, pageIndex = 1, pageSize = 10))
  stop_for_status(temp)
  temp <- content(temp)
  stopifnot(temp$rows_total == nrow(temp$data))
  temp$rows_total
})

identical(ori_rows, api_rows)

dbDisconnect(con)
pool::poolClose(pool)
closeAllConnections()
