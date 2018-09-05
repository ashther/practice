
library(httr)
library(dplyr)
library(RMySQL)

url_port <- 'http://localhost:8002'

pool <- pool::dbPool(RMySQL::MySQL(),
                     host = '192.168.126.128', # change this after ip changed
                     port = 3306, 
                     username = 'api_card', 
                     password = '123456', 
                     dbname = 'utqe_test')
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)

base_url <- paste0(url_port, '/normal/condition/delete') # add table name
temp <- POST(paste0(url_port, '/test/test/delete'), body = list(pageIndex = 1), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "Resource not found.")

temp <- POST(base_url, body = list(test = 1), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "缺少参数\"id\",也没有缺省值")

temp <- POST(base_url, body = list(id = '1,,2'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "!is.na(row_id) are not all TRUE")

temp <- POST(base_url, body = list(id = '-1'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "all(row_id > 0) is not TRUE")

temp <- POST(base_url, body = list(id = '0'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "all(row_id > 0) is not TRUE")

temp <- POST(base_url, body = list(id = '-2,1'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "all(row_id > 0) is not TRUE")

temp <- POST(base_url, body = list(id = 'test'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "!is.na(row_id) is not TRUE")

temp <- POST(base_url, body = list(id = '99999'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "value of parameter id is not in this table.")

api_rows <- purrr::map_dbl(setdiff(dbListTables(pool), 'sys_dict'), function(x) {
  tryCatch({
    max_id <- dbGetQuery(pool, paste0('select max(id) from ', x))[1, 1]
    url <- stringr::str_replace_all(x, '_', '/') %>%
      sprintf(paste0(url_port, '/%s/delete'), .)
    
    temp <- POST(url, body = list(id = max_id), encode = 'form')
    stop_for_status(temp)
    
    max_id
  }, error = function(e) {
    stop(sprintf('%s: %s', x, e$message))
  })
})

ori_rows <- purrr::map_dbl(setdiff(dbListTables(pool), 'sys_dict'), function(x) {
  tidyr::replace_na(dbGetQuery(pool, paste0('select max(id) from ', x))[1, 1], 0)
})

all(api_rows > ori_rows)

dbDisconnect(con)
pool::poolClose(pool)
closeAllConnections()
