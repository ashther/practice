
# find where script is and get config ------------------------------------

isInDocker <- function() {
  group_info <- system('cat /proc/1/cgroup', intern = TRUE)
  any(grepl('docker', group_info)) | file.exists('/.dockerenv')
}

if (isTRUE(isInDocker())) {
  HOME_PATH <- '/home/rstudio'
} else {
  HOME_PATH <- '/home/ashther/udas'
}
config <- jsonlite::fromJSON(file.path(HOME_PATH, 'config.json'))

# preparation ------------------------------------------------------------

pool <- pool::dbPool(
  RMySQL::MySQL(),
  host     = config$db_host,
  port     = config$db_port,
  username = config$db_username,
  password = config$db_password,
  dbname   = config$dbname
)
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)
# pool::poolClose(pool)

saveRDS(pool, file.path(HOME_PATH, 'pool.rds'))
source(file.path(HOME_PATH, 'function.R'), local = TRUE)

# filters ----------------------------------------------------------------

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
    name = 'api_udas'
  )
  plumber::forward()
}

# endpoint selection -----------------------------------------------------


#* @get /area
function(req, res) {
  tryCatch({
    
    result <- dbGetQuery(
      pool, 
      "SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
              encode_item_name AS `area`
       FROM ds_encode_item
       WHERE encode_type_code = 'B_ZHRMGHGXZQH'
         AND SUBSTR(encode_item_code, 3, 6) = '0000';"
    )$area
    
    res_logger(req, res)
    list(area = result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /zczk
function(req, res) {
  tryCatch({
    
    zczk <- dbGetQuery(
      pool, 
      "SELECT encode_item_code AS zczkm,
              encode_item_name AS zczk
       FROM ds_encode_item
       WHERE encode_type_code = 'ZCZK';"
    )
    zczk_order <- dbGetQuery(
      pool, 
      "SELECT zczkm,
              COUNT(*) AS n
       FROM bks_zcb
       GROUP BY zczkm
       ORDER BY n DESC;"
    )
    result <- c('全部', full_join(zczk_order, zczk, by = 'zczkm')$zczk)
    
    res_logger(req, res)
    list(zczk = result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /lqnf
function(req, res) {
  tryCatch({
    year_range <- dbGetQuery(pool, 'select distinct lqnf from ks_lqb;')
    year_range <- as.integer(year_range$lqnf)
    # full_year <- seq(min(year_range), max(year_range), 1)
    
    res_logger(req, res)
    # list(lqnf = full_year)
    list(lqnf = year_range)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /xnZc
function(req, res) {
  tryCatch({
    year_range <- dbGetQuery(pool, 'select distinct xn from bks_zcb;')
    year_range <- as.integer(year_range$xn)
    # full_year <- seq(min(year_range), max(year_range), 1)
    
    res_logger(req, res)
    # list(xn = full_year)
    list(xn = year_range)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /xnFs
function(req, res) {
  tryCatch({
    year_range <- dbGetQuery(pool, 'select distinct substr(xh, 2, 4) as xn from bks_zgfsb;')
    year_range <- as.integer(year_range$xn)
    # full_year <- seq(min(year_range), max(year_range), 1)
    
    res_logger(req, res)
    # list(xn = full_year)
    list(xn = year_range)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /ydrq
function(req, res) {
  tryCatch({
    year_range <- dbGetQuery(pool, 'SELECT DISTINCT SUBSTR(ydrq, 1, 4) as ydrq FROM bks_xjydb;')
    year_range <- as.integer(year_range$ydrq)
    # full_year <- seq(min(year_range), max(year_range), 1)
    
    res_logger(req, res)
    # list(ydrq = full_year)
    list(ydrq = year_range)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /byrq
function(req, res) {
  tryCatch({
    year_range <- dbGetQuery(pool, 'SELECT DISTINCT SUBSTR(byrq, 1, 4) as byrq FROM bks_jyqxb;')
    year_range <- as.integer(year_range$byrq)
    # full_year <- seq(min(year_range), max(year_range), 1)
    
    res_logger(req, res)
    # list(ydrq = full_year)
    list(byrq = year_range)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /kl
function(req, res) {
  tryCatch({
    # result <- dbGetQuery(
    #   pool, 'SELECT kl, COUNT(*) AS n FROM ks_lqb GROUP BY kl ORDER BY n DESC;'
    # )$kl
    # result <- c('全部', result)
    
    res_logger(req, res)
    list(kl = c('全部', '理工', '文史', '艺术', '体育', '其他'))
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /lqlx
function(req, res) {
  tryCatch({
    result <- dbGetQuery(
      pool, 'SELECT lqlx, COUNT(*) AS n FROM ks_lqb GROUP BY lqlx ORDER BY n DESC;'
    )$lqlx
    result <- c('全部', result)
    
    res_logger(req, res)
    list(lqlx = result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /zymc
function(req, res) {
  tryCatch({
    result <- dbGetQuery(
      pool, 'SELECT zymc, COUNT(*) AS n FROM ks_lqb GROUP BY zymc ORDER BY n DESC;'
    )$zymc
    result <- c('全部', result)
    
    res_logger(req, res)
    list(zymc = result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /dwmc
function(req, res) {
  tryCatch({
    result <- dbGetQuery(
      pool, 'SELECT dwmc, COUNT(*) AS n FROM jzg_jbsjb GROUP BY dwmc ORDER BY n DESC;'
    )$dwmc
    result <- c('全部', result)
    
    res_logger(req, res)
    list(dwmc = result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /zgfs
function(req, res) {
  tryCatch({
    result <- c('全部', zgfs_reason$name)
    
    res_logger(req, res)
    list(zgfs = result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @get /yxsmc
function(req, res) {
  tryCatch({
    result <- dbGetQuery(
      pool, 'SELECT yxsmc, COUNT(*) AS n FROM bks_xjb GROUP BY yxsmc ORDER BY n DESC;'
    )$yxsmc
    result <- c('全部', result)
    
    res_logger(req, res)
    list(yxsmc = result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}
