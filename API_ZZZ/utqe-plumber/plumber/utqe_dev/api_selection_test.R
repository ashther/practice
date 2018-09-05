
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

base_url <- paste0(url_port, '/selection/')
sys_dict <- dbReadTable(pool, 'sys_dict')
getItemName <- purrr::possibly(function(x) {
  temp <- GET(paste0(base_url, x))
  stop_for_status(temp)
  temp <- content(temp, 'text', encoding = 'utf-8')
  jsonlite::fromJSON(temp)$itemName
}, otherwise = NA_character_, quiet = FALSE)

temp <- purrr::map(unique(sys_dict$type_code), getItemName)
temp <- setNames(temp, unique(sys_dict$type_code))
item_name <- split(sys_dict$item_name, sys_dict$type_code)

purrr::walk(names(item_name), ~ tryCatch(
  stopifnot(identical(temp[[.x]], item_name[[.x]])), 
  error = function(e)print(sprintf('%s, %s', .x, e$message))
))

dbDisconnect(con)
pool::poolClose(pool)
closeAllConnections()
