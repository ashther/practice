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
utqe_table_read <- function(.pool, table_name, params = NULL, pageIndex, pageSize) {
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
      sql <- sqlInterpolate(.pool, sql, .dots = params)
    }
    rows_total <- dbGetQuery(.pool, sql)[1, 1]
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
    sql <- sqlInterpolate(.pool, sql, .dots = args)
    
    result <- dbGetQuery(.pool, sql)
    
    list(rowsTotal  = rows_total, 
         pagesTotal = pages_total, 
         data       = result)
  }, error = function(e) {
    list(errorMsg  = e$message, 
         errorCode = 40099)
  })
}