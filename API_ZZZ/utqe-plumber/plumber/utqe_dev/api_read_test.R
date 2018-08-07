
library(httr)
library(dplyr)
library(RMySQL)

url_port <- 'http://localhost:8002'

pool <- pool::dbPool(RMySQL::MySQL(),
                     host = '192.168.15.128', # change this after ip changed
                     port = 3306, 
                     username = 'api_card', 
                     password = '123456', 
                     dbname = 'utqe_test')
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)

base_url <- paste0(url_port, '/faculty/institution')
temp <- GET(paste0(url_port, '/test/test'), query = list(pageIndex = 1))
stopifnot(status_code(temp) == 404L)
stopifnot(content(temp)$errorCode == 40000)

temp <- GET(base_url, query = list(pageIndex = 1))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40001)

temp <- GET(base_url, query = list(pageSize = 10))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40001)

temp <- GET(base_url, query = list(pageIndex = 1, pageSize = 10, PXZS = 'test'))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)

temp <- GET(base_url, query = list(pageIndex = -1, pageSize = 10))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40003)

temp <- GET(base_url, query = list(pageIndex = 1, pageSize = -10))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40003)

ori_rows <- purrr::map_dbl(dbListTables(pool), function(x) {
  dbGetQuery(pool, sprintf('select count(*) from %s', x))[1, 1]
})

api_rows <- dbListTables(pool) %>% 
  stringr::str_replace_all('_', '/') %>% 
  sprintf(paste0(url_port, '/%s'), .) %>% # change this to call API
  purrr::map_dbl(function(x) {
    tryCatch({
      temp <- GET(x, query = list(pageIndex = 1, pageSize = 10))
      stop_for_status(temp)
      temp <- content(temp)
      # stopifnot(temp$rows_total == length(temp$data))
      temp$rowsTotal
    }, error = function(e) {
      stop(sprintf('%s: %s', x, e$message))
    })
  })

identical(ori_rows, api_rows)

dbDisconnect(con)
pool::poolClose(pool)
closeAllConnections()
