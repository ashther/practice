
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

base_url <- paste0(url_port, '/normal/condition')
temp <- GET(paste0(url_port, '/test/test'), query = list(pageIndex = 1))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == 'Resource not found.')

temp <- GET(base_url, query = list(pageIndex = 1))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == '缺少参数\"pageSize\",也没有缺省值')

temp <- GET(base_url, query = list(pageSize = 10))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "缺少参数\"pageIndex\",也没有缺省值")

temp <- GET(base_url, query = list(year = 2017))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "缺少参数\"pageIndex\",也没有缺省值")

temp <- GET(base_url, query = list(pageIndex = 1, pageSize = 10, year = 2017, JXALKALS = 'test'))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "Parameters type is invalid.")

temp <- GET(base_url, query = list(pageIndex = -1, pageSize = 10, year = 2017))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "pageIndex must be positive integer.")

temp <- GET(base_url, query = list(pageIndex = 1, pageSize = -10, year = 2017))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "pageSize must be positive integer.")

temp <- GET(base_url, query = list(pageIndex = 1, pageSize = 10, year = -2017))
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "year must be positive integer.")

ori_rows <- purrr::map_dbl(setdiff(dbListTables(pool), 'sys_dict'), function(x) {
  dbGetQuery(pool, sprintf('select count(*) from %s', x))[1, 1]
})

api_rows <- setdiff(dbListTables(pool), 'sys_dict') %>% 
  stringr::str_replace_all('_', '/') %>% 
  sprintf(paste0(url_port, '/%s'), .) %>% # change this to call API
  purrr::map_dbl(function(x) {
    tryCatch({
      temp <- GET(x, query = list(pageIndex = 1, pageSize = 10, year = 2017))
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
