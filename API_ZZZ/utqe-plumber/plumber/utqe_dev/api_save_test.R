
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

base_url <- paste0(url_port, '/normal/condition/save') # add table name
temp <- POST(paste0(url_port, '/test/test/save'), body = list(pageIndex = 1), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "Resource not found.")

temp <- POST(base_url, body = list(test = 1), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "缺少参数\"id\",也没有缺省值")

temp <- POST(base_url, body = list(id = 'test'), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "!is.na(id) is not TRUE")

temp <- POST(base_url, body = list(id = -1), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "id >= 0 is not TRUE")

temp <- POST(base_url, body = list(id = 0), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "缺少参数\"year\",也没有缺省值")

temp <- POST(base_url, body = list(id = 0, year = 2017), encode = 'form')
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "No parameters to be updated or inserted.")

temp <- POST(base_url, body = list(id = 0, JXALKALS = 'test'), encode = 'form') # add parameter name
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "缺少参数\"year\",也没有缺省值")

temp <- POST(base_url, body = list(id = 99999, XNZYM = 'test'), encode = 'form') # add parameter name and value
stopifnot(status_code(temp) == 400L)
stopifnot(content(temp) == "value of parameter id is not in this table.")

api_rows <- purrr::map(setdiff(dbListTables(pool), 'sys_dict'), function(x) {
  tryCatch({
    field <- dbGetQuery(pool, paste0('show fields from ', x))[2, 1]
    url <- stringr::str_replace_all(x, '_', '/') %>%
      sprintf(paste0(url_port, '/%s/save'), .)
    
    params <- list(id = 0, 1, year = 2018)
    names(params)[2] <- field
    temp <- POST(url, body = params, encode = 'form')
    stop_for_status(temp)
    temp <- content(temp)
    jsonlite::fromJSON(jsonlite::toJSON(
      temp$data, auto_unbox = TRUE, na = 'null', null = 'null'
    ))
  }, error = function(e) {
    stop(sprintf('%s: %s', x, e$message))
  })
})

ori_rows <- purrr::map(setdiff(dbListTables(pool), 'sys_dict'), function(x) {
  dbGetQuery(pool, sprintf('select * from `%s` where id = (select max(id) from `%s`)', x, x))
})
is_not_na <- function(x)!is.na(x)
api_rows <- purrr::map(api_rows, ~ select_if(.x, is_not_na))
ori_rows <- purrr::map(ori_rows, ~ select_if(.x, is_not_na))

all_equal(ori_rows, api_rows)

# englisth_table_names <- purrr::keep(dbListTables(pool), ~ stringr::str_detect(
#   dbGetQuery(pool, paste0('show fields from ', .x))[1, 1], '[a-zA-Z]'
# ))

purrr::walk(setdiff(dbListTables(pool), 'sys_dict'), function(x) {
  max_id <- dbGetQuery(pool, paste0('select max(id) from ', x))[1, 1]
  field <- dbGetQuery(pool, paste0('show fields from ', x))[2, 1]
  url <- stringr::str_replace_all(x, '_', '/') %>%
        sprintf(paste0(url_port, '/%s/save'), .)
  params <- list(id = max_id, 2)
  names(params)[2] <- field
  temp <- POST(url, body = params, encode = 'form')
  stop_for_status(temp)
})

updated_rows <- purrr::map(setdiff(dbListTables(pool), 'sys_dict'), function(x) {
  dbGetQuery(
    pool, sprintf('select * from `%s` where id = (select max(id) from `%s`)', x, x)
  )[1, 2]
})
all(unlist(updated_rows) == '2')

dbDisconnect(con)
pool::poolClose(pool)
closeAllConnections()
