
list_to_string <- function(params, .collapse = ' and ') {
  # sqlInterpolate will reganize chinese character as position variable!!
  # if there is chinese colnames, we will need data structure like this:
  # params <- list(names = list(code = '代码'), values = list(code = 001))
  paste0('`', names(params), '`', 
         ' = ?', 
         names(params), 
         collapse = .collapse)
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
        errorMsg = 'pageIndex must be positive integer.', 
        errorCode = 40003
      ))
    }
    if (pageSize <= 0) {
      return(list(
        errorMsg = 'pageSize must be positive integer.', 
        errorCode = 40003
      ))
    }
    
    # sql <- paste0('select count(*) from ', table_name)
    sql <- sprintf('select count(*) from `%s`', table_name)
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
      sql <- paste0('select * from `', 
                    table_name, 
                    '` where ', 
                    list_to_string(params), 
                    ' limit ?limit offset ?offset;')
      args <- c(args, params)
    } else {
      sql <- paste0('select * from `', 
                    table_name, 
                    '` limit ?limit offset ?offset;')
    }
    sql <- sqlInterpolate(pool, sql, .dots = args)
    
    result <- dbGetQuery(pool, sql)
    
    list(rowsTotal  = rows_total, 
         pagesTotal = pages_total, 
         data       = result)
  }, error = function(e) {
    list(errorMsg  = e$message, 
         errorCode = 40099)
  })
}

utqe_table_read_filter <- function(request, response) {
  library(RestRserve)
  library(jsonlite)

# check required parameters ----------------------------------------------

  endpoint <- request$path
  query <- request$query
  
  req_params <- endpoint_table_params[[endpoint]]$req_params
  opt_params <- endpoint_table_params[[endpoint]]$opt_params
  all_params <- c(req_params, opt_params)
  table_name <- endpoint_table_params[[endpoint]]$table_name
  
  if (!is.null(req_params)) {
    if (!all(names(req_params) %in% names(query))) {
      return(RestRserveResponse$new(
        body = toJSON(list(errorMsg = 'Required parameters must be supplied.', 
                           errorCode = 40001), 
                      auto_unbox = TRUE), 
        content_type = 'application/json', 
        status_code = 400L, 
        headers = 'Access-Control-Allow-Origin:*'
      ))
    }
  }

# check required and optional parameters type ----------------------------

  inter_params_names <- intersect(names(all_params), names(query))
  inter_params <- purrr::map(inter_params_names, ~ {
    eval(parse(text = sprintf('%s(query[["%s"]])', all_params[.x], .x)))
  })
  inter_params <- setNames(inter_params, inter_params_names)
  
  if (any(is.na(inter_params)) | any(is.null(inter_params))) {
    return(RestRserveResponse$new(
      body = toJSON(list(errorMsg = 'Parameters type is invalid.', 
                         errorCode = 40002), 
                    auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L, 
      headers = 'Access-Control-Allow-Origin:*'
    ))
  }

# call real function -----------------------------------------------------

  pageIndex <- query[['pageIndex']]
  pageSize <- query[['pageSize']]
  params <- as.list(query)
  params <- params[setdiff(names(params), c('pageIndex', 'pageSize'))]
  
  result <- utqe_table_read(table_name, params, pageIndex, pageSize)
  if ('errorCode' %in% names(result)) {
    return(RestRserveResponse$new(
      body         = toJSON(result, auto_unbox = TRUE, na = 'null', null = 'null'), 
      content_type = 'application/json', 
      status_code  = 400L, 
      headers = 'Access-Control-Allow-Origin:*'
    ))
  } else {
    return(RestRserveResponse$new(
      body         = toJSON(result, auto_unbox = TRUE, na = 'null', null = 'null'), 
      content_type = 'application/json', 
      status_code  = 200L, 
      headers = 'Access-Control-Allow-Origin:*'
    ))
  }
    
}
