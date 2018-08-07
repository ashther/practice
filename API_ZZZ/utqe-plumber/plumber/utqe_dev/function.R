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
    dbWriteTable(pool, 
                 table_name, 
                 dplyr::as_tibble(params), 
                 append = TRUE, 
                 row.names = FALSE)
    
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

# row_id is integer vector
utqe_table_delete <- function(table_name, row_id) {
  library(RMySQL)
  
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

