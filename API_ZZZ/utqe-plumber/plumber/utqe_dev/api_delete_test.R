
library(httr)
library(dplyr)
library(RMySQL)

url_port <- 'http://localhost:8000'

pool <- pool::dbPool(RMySQL::MySQL(),
                     host = '192.168.126.128', # change this after ip changed
                     port = 3306, 
                     username = 'api_card', 
                     password = '123456', 
                     dbname = 'utqe_test')
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)

base_url <- paste0(url_port, '/universityInfo/info/delete') # add table name
temp <- GET(paste0(url_port, '/test/test/delete'), query = list(pageIndex = 1))
stopifnot(status_code(temp) == 404L)
stopifnot(content(temp)$errorCode == 40000)

temp <- POST(base_url, body = list(test = 1), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40001)
stopifnot(content(temp)$errorMsg == 'Required parameters must be supplied.')

temp <- POST(base_url, body = list(id = '1,,2'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameter id must be non-negative integer.')

temp <- POST(base_url, body = list(id = '-1'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameter id must be non-negative integer.')

temp <- POST(base_url, body = list(id = '0'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameter id must be non-negative integer.')

temp <- POST(base_url, body = list(id = '-2,1'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameter id must be non-negative integer.')

temp <- POST(base_url, body = list(id = 'test'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameter id must be non-negative integer.')

temp <- POST(base_url, body = list(id = '99999'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40003)
stopifnot(content(temp)$errorMsg == 'value of parameter id is not in this table.')

api_rows <- purrr::map_dbl(dbListTables(pool), function(x) {
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

ori_rows <- purrr::map_dbl(dbListTables(pool), function(x) {
  dbGetQuery(pool, paste0('select max(id) from ', x))[1, 1]
})

all(api_rows > ori_rows)

dbDisconnect(con)
pool::poolClose(pool)
closeAllConnections()
