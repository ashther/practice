
library(httr)
library(dplyr)
library(RMySQL)

pool <- pool::dbPool(RMySQL::MySQL(),
                     host = '192.168.126.128', # change this after ip changed
                     port = 3306, 
                     username = 'api_card', 
                     password = '123456', 
                     dbname = 'utqe_test')
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)

base_url <- 'http://localhost:8002/universityInfo/info/delete' # add table name
temp <- POST(base_url, body = list(test = 1), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40001)
stopifnot(content(temp)$errorMsg == 'Required parameters must be supplied.')

temp <- POST(base_url, body = list(id = '1,,2'), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameter id must be non-negative integer.')

temp <- POST(base_url, body = list(id = '-1'), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameter id must be non-negative integer.')

temp <- POST(base_url, body = list(id = '0'), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameter id must be non-negative integer.')

temp <- POST(base_url, body = list(id = '-2,1'), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameter id must be non-negative integer.')

temp <- POST(base_url, body = list(id = 'test'), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameter id must be non-negative integer.')

temp <- POST(base_url, body = list(id = '99999'), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40003)
stopifnot(content(temp)$errorMsg == 'value of parameter id is not in this table.')

api_rows <- purrr::map_dbl(dbListTables(pool), function(x) {
  tryCatch({
    max_id <- dbGetQuery(pool, paste0('select max(id) from ', x))[1, 1]
    url <- stringr::str_replace_all(x, '_', '/') %>%
      sprintf('http://localhost:8002/%s/delete', .)
    
    temp <- POST(url, body = list(id = max_id), encode = 'json')
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
