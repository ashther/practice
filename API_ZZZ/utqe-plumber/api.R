
# preparation ------------------------------------------------------------

HOME_PATH <- '/home/ashther/udas'
LOG_PATH <- file.path(HOME_PATH, 'logger')

source(file.path(HOME_PATH, 'endpoint_table_params.R'))
source(file.path(HOME_PATH, 'function.R'))

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


# endpoint and filter ----------------------------------------------------

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @param pageIndex int required
#* @param pageSize int required
#* @get /<db_name>/<table_name>
#* @serializer unboxedJSON
function(pageIndex, pageSize, req, res) {
  
  tryCatch({

# get info from req ------------------------------------------------------

    endpoint <- req$PATH_INFO
    query <- webutils::parse_query(
      gsub('^\\?', '', req$QUERY_STRING)
    )
    if (!endpoint %in% names(endpoint_table_params)) {
      res$status <- 404L
      return(list(
        errorMsg = 'Not correct endpoint name.', 
        errorCode = 40000
      ))
    }
    
    table_name <- endpoint_table_params[[endpoint]]$table_name
    req_params <- endpoint_table_params[[endpoint]]$req_params
    opt_params <- endpoint_table_params[[endpoint]]$opt_params
    all_params <- c(req_params, opt_params)


# check required paramters existance -------------------------------------

   if (!is.null(req_params)) {
      if (!all(names(req_params) %in% names(query))) {
        res$status <- 400L
        return(list(
          errorMsg = 'Required parameters must be supplied.', 
          errorCode = 40001
        ))
      }
    }

# check required and optional paramters type -----------------------------

    inter_params_names <- intersect(names(all_params), names(query))
    inter_params <- purrr::map(inter_params_names, ~ {
      eval(parse(text = sprintf('%s(query[["%s"]])', all_params[.x], .x)))
    })
    inter_params <- setNames(inter_params, inter_params_names)
    
    if (any(is.na(inter_params)) | any(is.null(inter_params))) {
      res$status <- 400L
      return(list(
        errorMsg = 'Parameters type is invalid.', 
        errorCode = 40002
      ))
    }

# call real function -----------------------------------------------------

    params <- query[setdiff(names(query), c('pageIndex', 'pageSize'))]
    result <- utqe_table_read(pool, table_name, params, pageIndex, pageSize)
    
    res$status <- ifelse('errorCode' %in% names(result), 400L, 200L)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    list(
      errorMsg = e$message, 
      errorCode = 40099
    )
  })
}
