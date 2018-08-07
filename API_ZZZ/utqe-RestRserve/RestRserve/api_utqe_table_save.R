
# params <- list(id = 1, XH = '320130900011', XM = 'some body')
utqe_table_update <- function(table_name, params = NULL, row_id) {
  library(RMySQL)
  
  tryCatch({
    
    # sql <- sprintf('select count(*) from `%s` where id = ?row_id', table_name)
    # sql <- sqlInterpolate(pool, sql, row_id = row_id)
    # is_in <- dbGetQuery(pool, sql)[1, 1]
    # if (is_in == 0) {
    #   return(list(
    #     errorMsg = 'value of parameter id is not in this table.',
    #     errorCode = 40003
    #   ))
    # }
    
    # send query -------------------------------------------------------------
    
    sql <- sprintf('update `%s` set %s where id = ?row_id', 
                   table_name, 
                   list_to_string(params, .collapse = ','))
    sql <- sqlInterpolate(pool, sql, .dots = c(params, row_id = row_id))
    con <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(con), add = TRUE)
    res <- dbSendQuery(con, sql)
    if (dbGetRowsAffected(res) == 0) {
      return(list(
        errorMsg = 'value of parameter id is not in this table.',
        errorCode = 40003
      ))
    }
    
    # get data back ----------------------------------------------------------
    
    sql <- sprintf('select * from `%s` where id = ?row_id', table_name)
    sql <- sqlInterpolate(pool, sql, row_id = row_id)
    result <- dbGetQuery(pool, sql)
    
    list(rowsTotal  = nrow(result), 
         data       = result)
  }, error = function(e) {
    list(errorMsg  = e$message, 
         errorCode = 40099)
  })
}

# params <- list(id = 1, XH = '320130900011', XM = 'some body')
utqe_table_insert <- function(table_name, params = NULL) {
  library(RMySQL)
  library(dplyr)
  
  tryCatch({
    
    # send query -------------------------------------------------------------
    
    # fields <- dbListFields(pool, table_name)
    # temp <- as_tibble(matrix(
    #   data = NA, 
    #   nrow = 1, 
    #   ncol = length(fields), 
    #   dimnames = list(NULL, fields)
    # ))
    # temp[names(params)] <- params
    dbWriteTable(pool, table_name, as_tibble(params), append = TRUE, row.names = FALSE)
    
    # get data back ----------------------------------------------------------
    
    result <- dbGetQuery(
      pool, 
      sprintf('select * from `%s` where id = (select max(id) from `%s`)', 
              table_name, table_name)
    )
    
    list(rowsTotal  = nrow(result), 
         data       = result)
  }, error = function(e) {
    list(errorMsg  = e$message, 
         errorCode = 40099)
  })
}

utqe_table_save_filter <- function(request, response) {
  library(RestRserve)
  library(jsonlite)
  
  endpoint <- gsub('/save$', '', request$path)
  body <- fromJSON(gsub('^data=', '', URLdecode(rawToChar(request$body))))
  opt_params <- endpoint_table_params[[endpoint]]$opt_params
  table_name <- endpoint_table_params[[endpoint]]$table_name

# check id existance and type --------------------------------------------

  if (!'id' %in% names(body)) {
    return(RestRserveResponse$new(
      body = toJSON(list(errorMsg = 'Parameter id must be supplied.', 
                         errorCode = 40001), 
                    auto_unbox = TRUE), 
      content_type = 'application/json',
      status_code = 400L, 
      headers = 'Access-Control-Allow-Origin:*'
    ))
  }
  
  row_id <- as.integer(body[['id']])
  if (is.na(row_id) || is.null(row_id)) {
    return(RestRserveResponse$new(
      body = toJSON(list(errorMsg = 'Type of parameters id is invalid, it must be integer.', 
                         errorCode = 40002), 
                    auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L, 
      headers = 'Access-Control-Allow-Origin:*'
    ))
  }
  
  if (row_id < 0) {
    return(RestRserveResponse$new(
      body = toJSON(list(errorMsg = 'Parameters id must be non-negative integer.', 
                         errorCode = 40003), 
                    auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L, 
      headers = 'Access-Control-Allow-Origin:*'
    ))
  }

# check other paramters existance and type -------------------------------

  params <- as.list(body)
  params <- params[setdiff(names(params), 'id')]
  
  if (length(params) == 0 || is.na(params) || is.null(params)) {
    return(RestRserveResponse$new(
      body = toJSON(list(errorMsg = 'Except id, there must be 1 parameter applied at least.', 
                         errorCode = 40001), 
                    auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L, 
      headers = 'Access-Control-Allow-Origin:*'
    ))
  }
  
  inter_params_names <- intersect(names(opt_params), names(params))
  inter_params <- purrr::map(inter_params_names, ~ {
    eval(parse(text = sprintf('%s(body[["%s"]])', opt_params[.x], .x)))
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

# check update or insert -------------------------------------------------

  if (row_id > 0) { # it's update method
    result <- utqe_table_update(table_name, params, row_id)
  } else {
    result <- utqe_table_insert(table_name, params)
  }
  
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
