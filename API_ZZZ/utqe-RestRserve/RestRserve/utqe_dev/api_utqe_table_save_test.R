
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

base_url <- 'http://localhost:8002/faculty/institution/save' # add table name
temp <- POST(base_url, body = list(test = 1), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40001)
stopifnot(content(temp)$errorMsg == 'Parameter id must be supplied.')

temp <- POST(base_url, body = list(id = 'test'), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Type of parameters id is invalid, it must be integer.')

temp <- POST(base_url, body = list(id = -1), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40003)
stopifnot(content(temp)$errorMsg == 'Parameters id must be non-negative integer.')

temp <- POST(base_url, body = list(id = 0), encode = 'json')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40001)
stopifnot(content(temp)$errorMsg == 'Except id, there must be 1 parameter applied at least.')

temp <- POST(base_url, body = list(id = 0, PXZS = 'test'), encode = 'json') # add parameter name
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40002)
stopifnot(content(temp)$errorMsg == 'Parameters type is invalid.')

temp <- POST(base_url, body = list(id = 99999, DWMC = 'test'), encode = 'json') # add parameter name and value
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp)$errorCode == 40003)
stopifnot(content(temp)$errorMsg == 'value of parameter id is not in this table.')

api_rows <- purrr::map(dbListTables(pool), function(x) {
  tryCatch({
    field <- dbGetQuery(pool, paste0('show fields from ', x))[1, 1]
    url <- stringr::str_replace_all(x, '_', '/') %>%
      sprintf('http://localhost:8002/%s/save', .)
    
    params <- list(id = 0, 1)
    names(params)[2] <- field
    temp <- POST(url, body = params, encode = 'json')
    stop_for_status(temp)
    temp <- content(temp)
    jsonlite::fromJSON(jsonlite::toJSON(
      temp$data, auto_unbox = TRUE, na = 'null', null = 'null'
    ))
  }, error = function(e) {
    stop(sprintf('%s: %s', x, e$message))
  })
})

ori_rows <- purrr::map(dbListTables(pool), function(x) {
  dbGetQuery(pool, sprintf('select * from `%s` where id = (select max(id) from `%s`)', x, x))
})
is_not_na <- function(x)!is.na(x)
api_rows <- purrr::map(api_rows, ~ select_if(.x, is_not_na))
ori_rows <- purrr::map(ori_rows, ~ select_if(.x, is_not_na))

identical(ori_rows, api_rows)

englisth_table_names <- purrr::keep(dbListTables(pool), ~ stringr::str_detect(
  dbGetQuery(pool, paste0('show fields from ', .x))[1, 1], '[a-zA-Z]'
))

purrr::walk(englisth_table_names, function(x) {
  max_id <- dbGetQuery(pool, paste0('select max(id) from ', x))[1, 1]
  field <- dbGetQuery(pool, paste0('show fields from ', x))[1, 1]
  url <- stringr::str_replace_all(x, '_', '/') %>%
        sprintf('http://localhost:8002/%s/save', .)
  params <- list(id = max_id, 2)
  names(params)[2] <- field
  temp <- POST(url, body = params, encode = 'json')
  stop_for_status(temp)
})

updated_rows <- purrr::map_dbl(englisth_table_names, function(x) {
  as.integer(dbGetQuery(
    pool, sprintf('select * from `%s` where id = (select max(id) from `%s`)', x, x)
  )[1, 1])
})
all(updated_rows == 2)

dbDisconnect(con)
pool::poolClose(pool)
closeAllConnections()
