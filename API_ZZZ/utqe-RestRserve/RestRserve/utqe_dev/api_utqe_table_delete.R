
# row_id is integer vector
utqe_table_delete <- function(table_name, row_id) {
  library(RMySQL)
  library(dplyr)
  
  tryCatch({
    
    # send query -------------------------------------------------------------
    
    sql <- sprintf('delete from `%s` where id in (%s)', 
                   table_name, 
                   paste0(row_id, collapse = ','))
    
    con <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(con), add = TRUE)
    res <- dbSendQuery(con, sql)
    affected_rows <- dbGetRowsAffected(res)
    if (affected_rows == 0) {
      return(list(
        errorMsg = 'value of parameter id is not in this table.',
        errorCode = 40003
      ))
    }
    
    # get data back ----------------------------------------------------------
    
    list(rowsDelete = affected_rows)
  }, error = function(e) {
    list(errorMsg  = e$message, 
         errorCode = 40099)
  })
}

utqe_table_delete_filter <- function(request, response) {
  library(RestRserve)
  library(jsonlite)
  
  # check required parameters ----------------------------------------------
  
  endpoint <- gsub('/delete$', '', request$path)
  body <- fromJSON(rawToChar(request$body))
  table_name <- endpoint_table_params[[endpoint]]$table_name
  
  if (!'id' %in% names(body)) {
    return(RestRserveResponse$new(
      body = toJSON(list(errorMsg = 'Required parameters must be supplied.', 
                         errorCode = 40001), 
                    auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L, 
      headers = 'Access-Control-Allow-Origin:*'
    ))
  }
  
  # call real function -----------------------------------------------------
  
  row_id <- body[['id']]
  row_id <- unlist(strsplit(trimws(row_id), ','))
  row_id <- as.integer(row_id)
  if (any(row_id <= 0)|| any(is.na(row_id)) || any(is.null(row_id))) {
    return(RestRserveResponse$new(
      body = toJSON(list(errorMsg = 'Parameter id must be non-negative integer.', 
                         errorCode = 40002), 
                    auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L, 
      headers = 'Access-Control-Allow-Origin:*'
    ))
  }
  
  result <- utqe_table_delete(table_name, row_id)
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