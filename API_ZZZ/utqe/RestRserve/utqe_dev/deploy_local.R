
# prepare connection and constant ----------------------------------------

pool <- pool::dbPool(
  RMySQL::MySQL(), 
  host     = '192.168.126.128', 
  port     = 3306, 
  username = 'api_card', 
  password = '123456', 
  dbname   = 'utqe_test'
)
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)
# pool::poolClose(pool)

source('~/utqe/endpoint_table_params.R') # create with util.R
source('~/utqe/api_utqe_table_read.R')
source('~/utqe/api_utqe_table_save.R')
source('~/utqe/api_utqe_table_delete.R')

list_to_string <- function(params, .collapse = ' and ') {
  # sqlInterpolate will reganize chinese character as position variable!!
  # if there is chinese colnames, we will need data structure like this:
  # params <- list(names = list(code = '代码'), values = list(code = 001))
  paste0('`', names(params), '`', 
         ' = ?', 
         names(params), 
         collapse = .collapse)
}

logger_level <- RestRserve::INFO

# new app object and regist endpoint -------------------------------------

RestRserveApp <- RestRserve::RestRserveApplication$new(
  logger = RestRserve::Logger$new(logger_level, file = '~/utqe/logger')
)

purrr::walk(
  names(endpoint_table_params), ~ eval(parse(text = sprintf(
    "RestRserveApp$add_get(path = '%s', FUN = utqe_table_read_filter)", .x
  )))
)

purrr::walk(
  names(endpoint_table_params), ~ eval(parse(text = sprintf(
    "RestRserveApp$add_post(path = '%s/save', FUN = utqe_table_save_filter)", .x
  )))
)

purrr::walk(
  names(endpoint_table_params), ~ eval(parse(text = sprintf(
    "RestRserveApp$add_post(path = '%s/delete', FUN = utqe_table_delete_filter)", .x
  )))
)