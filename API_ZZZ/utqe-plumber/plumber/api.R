
# preparation ------------------------------------------------------------

HOME_PATH <- '/home/rstudio'
LOG_PATH <- file.path(HOME_PATH, 'logger')
LOG_LEVEL <- futile.logger::INFO
futile.logger::flog.threshold(LOG_LEVEL)
futile.logger::flog.appender(futile.logger::appender.file(LOG_PATH), 
                             name = 'api_utqe')
res_logger <- function(req, res, code = NULL, msg = NULL) {
  if (as.integer(res$status) == 200) {
    futile.logger::flog.info(
      '{"uuid":"%s","status":"%s","msg":"response successfully"}',
      req$cookies$uuid,
      res$status,
      name = 'api_utqe'
    )
    plumber::forward()
  } else {
    futile.logger::flog.error(
      '{"uuid":"%s","status":"%s","code":"%s","msg":"%s"}',
      req$cookies$uuid,
      res$status,
      code,
      msg, 
      name = 'api_utqe'
    )
    plumber::forward()
  }
}

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

source(file.path(HOME_PATH, 'endpoint_table_params.R'), local = TRUE)
source(file.path(HOME_PATH, 'function.R'), local = TRUE)


# endpoint and filter ----------------------------------------------------

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @filter req logger
function(req) {

  if (req$REQUEST_METHOD == 'GET') {
    params <- req$QUERY_STRING
  } else if (req$REQUEST_METHOD == 'POST') {
    params <- req$postBody
  } else {
    params <- NULL
  }

  req$cookies$uuid <- uuid::UUIDgenerate(TRUE)

  futile.logger::flog.info(
    '{"uuid":"%s","addr":"%s","server":"%s","port":"%s","path":"%s","method":"%s","params":"%s","headers":{%s}}',
    req$cookies$uuid,
    req$REMOTE_ADDR,
    req$SERVER_NAME,
    req$SERVER_PORT,
    req$PATH_INFO,
    req$REQUEST_METHOD,
    params,
    paste0(sprintf('"%s":"%s"', names(req$HEADERS), req$HEADERS), 
           collapse = ','), 
    name = 'api_utqe'
  )
  plumber::forward()
}

#* read table
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
      errorCode <- 40000
      errorMsg <- 'Resource not found.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg, 
        errorCode = errorCode
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
        errorCode <- 40001
        errorMsg <- 'Required parameters must be supplied.'
        res_logger(req, res, errorCode, errorMsg)
        return(list(
          errorMsg = errorMsg, 
          errorCode = errorCode
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
      errorCode <- 40002
      errorMsg <- 'Parameters type is invalid.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg, 
        errorCode = errorCode
      ))
    }

# call real function -----------------------------------------------------

    params <- query[setdiff(names(query), c('pageIndex', 'pageSize'))]
    result <- utqe_table_read(table_name, params, pageIndex, pageSize)
    
    if ('errorCode' %in% names(result)) {
      res$status <- 400L
      res_logger(req, res, result$errorCode, result$errorMsg)
    } else {
      res$status <- 200L
      res_logger(req, res)
    }
    
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    errorCode <- 40098
    errorMsg <- e$message
    res_logger(req, res, errorCode, errorMsg)
    list(
      errorMsg = errorMsg, 
      errorCode = errorCode
    )
  })
}

#* update and insert into table
#* @param id int required
#* @post /<db_name>/<table_name>/save
#* @serializer unboxedJSON
function(id, req, res) {
  
  tryCatch({

# get info from req ------------------------------------------------------

    endpoint <- gsub('/save$', '', req$PATH_INFO)
    body <- webutils::parse_query(req$postBody)
    
    if (!endpoint %in% names(endpoint_table_params)) {
      res$status <- 404L
      errorCode <- 40000
      errorMsg <- 'Resource not found.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg,
        errorCode = errorCode
      ))
    }
    
    opt_params <- endpoint_table_params[[endpoint]]$opt_params
    table_name <- endpoint_table_params[[endpoint]]$table_name    

# check id existance and type --------------------------------------------

    if (!'id' %in% names(body)) {
      res$status <- 400L
      errorCode <- 40001
      errorMsg <- 'Parameter id must be supplied.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg,
        errorCode = errorCode
      ))
    }
    
    row_id <- as.integer(body[['id']])
    if (is.na(row_id) || is.null(row_id)) {
      res$status <- 400L
      errorCode <- 40002
      errorMsg <- 'Type of parameters id is invalid, it must be integer.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg,
        errorCode = errorCode
      ))
    }
    
    if (row_id < 0) {
      res$status <- 400L
      errorCode <- 40003
      errorMsg <- 'Parameters id must be non-negative integer.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg,
        errorCode = errorCode
      ))
    }

# check other parameters existance and type ------------------------------
    
    params <- body[setdiff(names(body), 'id')]
    
    if (length(params) == 0 || is.na(params) || is.null(params)) {
      res$status <- 400L
      errorCode <- 40001
      errorMsg <- 'Except id, there must be 1 parameter applied at least.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg,
        errorCode = errorCode
      ))
    }
    
    inter_params_names <- intersect(names(opt_params), names(params))
    inter_params <- purrr::map(inter_params_names, ~ {
      eval(parse(text = sprintf('%s(body[["%s"]])', opt_params[.x], .x)))
    })
    inter_params <- setNames(inter_params, inter_params_names)
    
    if (any(is.na(inter_params)) | any(is.null(inter_params))) {
      res$status <- 400L
      errorCode <- 40002
      errorMsg <- 'Parameters type is invalid.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg,
        errorCode = errorCode
      ))
    } 

# check update or insert -------------------------------------------------

    if (row_id > 0) { # it's update method
      result <- utqe_table_update(table_name, params, row_id)
    } else {
      result <- utqe_table_insert(table_name, params)
    }
    
    if ('errorCode' %in% names(result)) {
      res$status <- 400L
      res_logger(req, res, result$errorCode, result$errorMsg)
    } else {
      res$status <- 200L
      res_logger(req, res)
    }
    
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    errorCode <- 40098
    errorMsg <- e$message
    res_logger(req, res, errorCode, errorMsg)
    list(errorMsg = errorMsg,
         errorCode = errorCode)
  })
}

#* delete from table
#* @param id int required
#* @post /<db_name>/<table_name>/delete
#* @serializer unboxedJSON
function(id, req, res) {
  
  tryCatch({
    

# get info from req ------------------------------------------------------

    endpoint <- gsub('/delete$', '', req$PATH_INFO)
    body <- webutils::parse_query(req$postBody) 
    
    if (!endpoint %in% names(endpoint_table_params)) {
      res$status <- 404L
      errorCode <- 40000
      errorMsg <- 'Resource not found.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg, 
        errorCode = errorCode
      ))
    }
    
    table_name <- endpoint_table_params[[endpoint]]$table_name 

# check required parameters ----------------------------------------------

    if (!'id' %in% names(body)) {
      res$status <- 400L
      errorCode <- 40001
      errorMsg <- 'Required parameters must be supplied.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg, 
        errorCode = errorCode
      ))
    }
    
    row_id <- body[['id']]
    row_id <- unlist(strsplit(trimws(row_id), ','))
    row_id <- as.integer(row_id)
    if (any(row_id <= 0)|| any(is.na(row_id)) || any(is.null(row_id))) {
      res$status <- 400L
      errorCode <- 40002
      errorMsg <- 'Parameter id must be non-negative integer.'
      res_logger(req, res, errorCode, errorMsg)
      return(list(
        errorMsg = errorMsg, 
        errorCode = errorCode
      ))
    }

# call real function -----------------------------------------------------

    result <- utqe_table_delete(table_name, row_id)
    
    if ('errorCode' %in% names(result)) {
      res$status <- 400L
      res_logger(req, res, result$errorCode, result$errorMsg)
    } else {
      res$status <- 200L
      res_logger(req, res)
    }
    
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    errorCode <- 40098
    errorMsg <- e$message
    res_logger(req, res, errorCode, errorMsg)
    list(errorMsg = errorMsg, 
         errorCode = errorCode)
  })
}

# test http method -------------------------------------------------------

#* @param a
#* @param b
#* @post /testpost
function(a, b, req) {
  test <- webutils::parse_query(req$postBody)
  list(
    raw = req$postBody, 
    test = test, 
    test_type = typeof(test)
  )
}

#* @param a
#* @param b
#* @get /testget
function(a, b, req, res) {
  test <- webutils::parse_query(
    gsub('^\\?', '', req$QUERY_STRING)
  )
  list(
    raw = req$QUERY_STRING, 
    test = test, 
    test_type = typeof(test), 
    status = res$status, 
    un = names(res)
  )
}
