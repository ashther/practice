
# preparation ------------------------------------------------------------
pool <- pool::dbPool(
  RMySQL::MySQL(),
  host     = '192.168.15.128',
  port     = 3306,
  username = 'api_card',
  password = '123456',
  dbname   = 'udas'
)
con <- pool::poolCheckout(pool)
DBI::dbSendQuery(con, 'set names utf8')
pool::poolReturn(con)
# pool::poolClose(pool)

HOME_PATH <- '/home/rstudio'
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

#* 从字典表获取省级地区
#* @get /area
#* @serializer unboxedJSON
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

#* 从字典表获取注册状况
#* @get /zczk
#* @serializer unboxedJSON
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

#* 从录取表获取连续的录取年份
#* @get /lqnf
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    year_range <- dbGetQuery(pool, 'select distinct lqnf from ks_lqb;')
    year_range <- as.integer(year_range$lqnf)
    full_year <- seq(min(year_range), max(year_range), 1)
    
    res_logger(req, res)
    list(lqnf = full_year)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 从注册表获取连续的注册年份
#* @get /xn
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    year_range <- dbGetQuery(pool, 'select distinct xn from bks_zcb;')
    year_range <- as.integer(year_range$xn)
    full_year <- seq(min(year_range), max(year_range), 1)
    
    res_logger(req, res)
    list(xn = full_year)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 从学籍异动表获取连续的异动年份
#* @get /ydrq
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    year_range <- dbGetQuery(pool, 'SELECT DISTINCT SUBSTR(ydrq, 1, 4) as ydrq FROM bks_xjydb;')
    year_range <- as.integer(year_range$ydrq)
    full_year <- seq(min(year_range), max(year_range), 1)
    
    res_logger(req, res)
    list(ydrq = full_year)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 从录取表获取所有科类
#* @get /kl
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    result <- dbGetQuery(
      pool, 'SELECT kl, COUNT(*) AS n FROM ks_lqb GROUP BY kl ORDER BY n DESC;'
    )$kl
    result <- c('全部', result)
    
    res_logger(req, res)
    list(kl = result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 从录取表获取所有录取类型
#* @get /lqlx
#* @serializer unboxedJSON
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

#* 从录取表获取所有专业名称
#* @get /zymc
#* @serializer unboxedJSON
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

#* 从教职工表获取所有单位名称
#* @get /dwmc
#* @serializer unboxedJSON
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
