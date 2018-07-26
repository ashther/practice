
pool <- pool::dbPool(
  RMySQL::MySQL(), 
  host = '192.168.15.128', 
  port = 3306, 
  username = 'api_card', 
  password = '123456', 
  dbname = 'utqe'
)
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)
# pool::poolClose(pool)

# utqe_table_read_params <- list(
#   'universityInfo_undergraduate' = list(
#     'req_params' = c('pageSize' = 'as.integer', 'pageIndex' = 'as.integer'), 
#     'opt_params' = c('XH' = 'as.character', 'XM' = 'as.character')
#   )
# )
source('~/utqe/utqe_table_read_params.R')

list_to_string <- function(params) {
  # sqlInterpolate will reganize chinese character as position variable!!
  # if there is chinese colnames, we will need data structure like this:
  # params <- list(names = list(code = '代码'), values = list(code = 001))
  paste0('`', names(params), '`', 
         ' = ?', 
         names(params), 
         collapse = ' and ')
}

# params <- list(XH = '320130900011', XM = 'some body')
utqe_table_read <- function(table_name, params = NULL, pageIndex, pageSize) {
  library(RMySQL)
  
  tryCatch({
    # get rows and pages number ----------------------------------------------
    pageIndex <- as.integer(pageIndex)
    pageSize <- as.integer(pageSize)
    if (pageIndex <= 0) {
      return(list(
        error_msg = 'pageIndex must be positive integer.', 
        error_code = 40003
      ))
    }
    if (pageSize <= 0) {
      return(list(
        error_msg = 'pageSize must be positive integer.', 
        error_code = 40003
      ))
    }
    
    sql <- paste0('select count(*) from ', table_name)
    if (length(params) > 0) {
      sql <- paste0(sql, ' where ', list_to_string(params))
      sql <- sqlInterpolate(pool, sql, .dots = params)
    }
    rows_total <- dbGetQuery(pool, sql)[1, 1]
    pages_total <- ceiling(rows_total / pageSize)
    
    # get table data ---------------------------------------------------------
    
    args <- list(offset = (pageIndex - 1) * pageSize, 
                 limit = pageSize)
    
    if (length(params) > 0) {
      sql <- paste0('select * from ', 
                    table_name, 
                    ' where ', 
                    list_to_string(params), 
                    ' limit ?limit offset ?offset;')
      args <- c(args, params)
    } else {
      sql <- paste0('select * from ', 
                    table_name, 
                    ' limit ?limit offset ?offset;')
    }
    sql <- sqlInterpolate(pool, sql, .dots = args)
    
    result <- dbGetQuery(pool, sql)
    
    list(rows_total = rows_total, 
         pages_total = pages_total, 
         data = result)
  }, error = function(e) {
    list(error_msg = e$message, 
         error_code = 40099)
  })
}

utqe_table_read_filter <- function(request, response) {
  library(RestRserve)
  library(jsonlite)
  library(purrr)

# standard error response ------------------------------------------------

  response_40001 <- RestRserveResponse$new(
    body = toJSON(list(error_msg = 'Required parameters must be supplied.', 
                       error_code = 40001), 
                  auto_unbox = TRUE), 
    content_type = 'application/json', 
    status_code = 400L
  )
  
  response_40002 <- RestRserveResponse$new(
    body = toJSON(list(error_msg = 'Parameters type is invalid.', 
                       error_code = 40002), 
                  auto_unbox = TRUE), 
    content_type = 'application/json', 
    status_code = 400L
  )

# check required parameters ----------------------------------------------

  query <- request$query
  if (!'table' %in% names(query)) {
    return(response_40001)
  }
  
  if (!query[['table']] %in% names(utqe_table_read_params)) {
    return(RestRserveResponse$new(
      body = toJSON(list(error_msg = 'Table does\'t exist, check the table name.', 
                         error_code = 40003), 
                    auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L
    ))
  }
  
  table_name <- query[['table']]
  req_params <- utqe_table_read_params[[table_name]]$req_params
  opt_params <- utqe_table_read_params[[table_name]]$opt_params
  all_params <- c(req_params, opt_params)
  
  if (!is.null(req_params)) {
    if (!all(names(req_params) %in% names(query))) {
      return(response_40001)
    }
  }

# check required and optional parameters type ----------------------------

  inter_params_names <- intersect(names(all_params), names(query))
  inter_params <- map(inter_params_names, ~ {
    eval(parse(text = sprintf('%s(query[["%s"]])', all_params[.x], .x)))
  })
  inter_params <- setNames(inter_params, inter_params_names)
  
  if (any(is.na(inter_params)) | any(is.null(inter_params))) {
    return(response_40002)
  }

# call real function -----------------------------------------------------

  pageIndex <- query[['pageIndex']]
  pageSize <- query[['pageSize']]
  params <- as.list(query)
  params <- params[setdiff(names(params), c('pageIndex', 'pageSize', 'table'))]
  
  result <- utqe_table_read(table_name, params, pageIndex, pageSize)
  if ('error_code' %in% names(result)) {
    return(RestRserveResponse$new(
      body = toJSON(result, auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L
    ))
  } else {
    return(RestRserveResponse$new(
      body = toJSON(result, auto_unbox = TRUE, na = 'null', null = 'null'), 
      content_type = 'application/json', 
      status_code = 200L
    ))
  }
    
}

RestRserveApp <- RestRserve::RestRserveApplication$new()
RestRserveApp$add_get(path = '/utqe_table_read',
                      FUN = utqe_table_read_filter)
# purrr::walk(
#   names(endpoint_table_params), ~ eval(parse(text = sprintf(
#     "RestRserveApp$add_get(path = '%s', FUN = utqe_table_get_filter)", .x
#   )))
# )

# RestRserveApp$run(http_port = '8001')