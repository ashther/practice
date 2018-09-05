
# preparation ------------------------------------------------------------

HOME_PATH <- '/home/rstudio'
LOG_PATH <- file.path(HOME_PATH, 'logger')
LOG_LEVEL <- futile.logger::INFO
futile.logger::flog.threshold(LOG_LEVEL)
futile.logger::flog.appender(futile.logger::appender.file(LOG_PATH), 
                             name = 'api_utqe')
layout_custom <- futile.logger::layout.format(
  '~l [~t] ~m', datetime.fmt = '%Y-%m-%d %H:%M:%OS3'
)
futile.logger::flog.layout(layout_custom, name = 'api_utqe')

res_logger <- function(req, res, msg = NULL) {
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
      '{"uuid":"%s","status":"%s","msg":"%s"}',
      req$cookies$uuid,
      res$status,
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

#* selection year
#* @get /selection/year
function(req, res) {
  library(RMySQL)
  
  tryCatch({
    
    sql <- 'select distinct `year` from universityInfo_info;'
    result <- dbGetQuery(pool, sql)[, 1]
    result <- sort(result, decreasing = TRUE)
    
    res_logger(req, res)
    list(itemName = result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* read selection from table sys_dict
#* @get /selection/<type_code>
function(type_code, req, res) {
  library(RMySQL)
  
  tryCatch({
    
    sql <- 'select item_name from sys_dict where type_code = ?type_code;'
    sql <- sqlInterpolate(pool, sql, type_code = type_code)
    result <- dbGetQuery(pool, sql)[, 1]
    
    res_logger(req, res)
    list(itemName = result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* read table
#* @param pageIndex int required
#* @param pageSize int required
#* @param year int required
#* @get /<db_name>/<table_name>
#* @serializer unboxedJSON
function(pageIndex, pageSize, year, req, res) {
  
  tryCatch({

# get info from req ------------------------------------------------------

    endpoint <- req$PATH_INFO
    query <- webutils::parse_query(
      gsub('^\\?', '', req$QUERY_STRING)
    )
    if (!endpoint %in% names(endpoint_table_params)) {
      stop('Resource not found.')
    }
    
    table_name <- endpoint_table_params[[endpoint]]$table_name
    req_params <- endpoint_table_params[[endpoint]]$req_params
    opt_params <- endpoint_table_params[[endpoint]]$opt_params
    all_params <- c(req_params, opt_params)

# check required and optional paramters type -----------------------------

    inter_params_names <- intersect(names(all_params), names(query))
    inter_params <- purrr::map(inter_params_names, ~ {
      eval(parse(text = sprintf('%s(query[["%s"]])', all_params[.x], .x)))
    })
    inter_params <- setNames(inter_params, inter_params_names)
    
    if (any(is.na(inter_params)) | any(is.null(inter_params))) {
      stop('Parameters type is invalid.')
    }

# call real function -----------------------------------------------------

    params <- query[setdiff(names(query), c('pageIndex', 'pageSize', 'year'))]
    result <- utqe_table_read(table_name, params, pageIndex, pageSize, year)
    res_logger(req, res)
    
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* update and insert into table
#* @param id int required
#* @param year int required
#* @post /<db_name>/<table_name>/save
#* @serializer unboxedJSON
function(id, year, req, res) {
  
  tryCatch({

# get info from req ------------------------------------------------------

    endpoint <- gsub('/save$', '', req$PATH_INFO)
    body <- webutils::parse_query(req$postBody)
    
    if (!endpoint %in% names(endpoint_table_params)) {
      stop('Resource not found.')
    }
    
    opt_params <- endpoint_table_params[[endpoint]]$opt_params
    table_name <- endpoint_table_params[[endpoint]]$table_name    

# check id existance and type --------------------------------------------
    
    id <- as.integer(id)
    stopifnot(!is.na(id), id >= 0)
    row_id <- id

# check other parameters existance and type ------------------------------
    
    if (row_id > 0) {
      params <- body[setdiff(names(body), 'id')] # may be only year to be updated
    } else {
      year <- as.integer(year)
      stopifnot(!is.na(year), year > 0)
      params <- body[setdiff(names(body), c('id', 'year'))]
    }
    
    if (length(params) == 0 || is.na(params) || is.null(params)) {
      stop('No parameters to be updated or inserted.')
    }
    
    inter_params_names <- intersect(names(opt_params), names(params))
    inter_params <- purrr::map(inter_params_names, ~ {
      eval(parse(text = sprintf('%s(body[["%s"]])', opt_params[.x], .x)))
    })
    inter_params <- setNames(inter_params, inter_params_names)
    
    if (any(is.na(inter_params)) | any(is.null(inter_params))) {
      stop('Parameters type is invalid.')
    }
     

# check update or insert -------------------------------------------------

    if (row_id > 0) { # it's update method
      result <- utqe_table_update(table_name, params, row_id)
    } else {
      result <- utqe_table_insert(table_name, params, year)
    }
    res_logger(req, res)
    
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
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
    # body <- webutils::parse_query(req$postBody) 
    
    if (!endpoint %in% names(endpoint_table_params)) {
      stop('Resource not found.')
    }
    
    table_name <- endpoint_table_params[[endpoint]]$table_name 

# check required parameters ----------------------------------------------
    
    row_id <- id
    row_id <- unlist(strsplit(trimws(row_id), ','))
    row_id <- as.integer(row_id)
    stopifnot(!is.na(row_id), all(row_id > 0))

# call real function -----------------------------------------------------

    result <- utqe_table_delete(table_name, row_id)
    res_logger(req, res)
    
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
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
