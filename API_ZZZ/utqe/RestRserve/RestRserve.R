

# preparation ------------------------------------------------------------

HOME_PATH <- '/home/rstudio'
logger_level <- RestRserve::INFO
LOG_PATH <- file.path(HOME_PATH, 'logger')

source(file.path(HOME_PATH, 'api_utqe_table_read.R'))
source(file.path(HOME_PATH, 'api_utqe_table_save.R'))
source(file.path(HOME_PATH, 'api_utqe_table_delete.R'))
source(file.path(HOME_PATH, 'endpoint_table_params.R'))

# database connection pool -----------------------------------------------

pool <- pool::dbPool(
  RMySQL::MySQL(), 
  host     = '192.168.15.128', 
  port     = 3306, 
  username = 'api_card', 
  password = '123456', 
  dbname   = 'utqe_test'
)
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)
# pool::poolClose(pool)

# create application and register endpoints ------------------------------

RestRserveApp <- RestRserve::RestRserveApplication$new(
  logger = RestRserve::Logger$new(logger_level, file = LOG_PATH)
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

# RestRserveApp$add_openapi(path = '/openapi.yaml', file_path = 'openapi.yaml')
# RestRserveApp$add_swagger_ui(path = '/swagger', 
#                              path_openapi = '/openapi.yaml', 
#                              path_swagger_assets = '/__swagger__')

RestRserveApp$run(http_port = "8002", 
                  encoding = 'utf8', 
                  port = '6311', 
                  remote = 'enable')
