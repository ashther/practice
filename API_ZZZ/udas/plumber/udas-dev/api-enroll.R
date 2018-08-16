
HOME_PATH <- '/home/ashther/udas'
pool <- readRDS(file.path(HOME_PATH, 'pool.rds'))
source(file.path(HOME_PATH, 'function.R'), local = TRUE)

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

#* 新生入学 电子注册 概况 基本情况
#* @param xn
#* @get /regist/summary/info
#* @serializer unboxedJSON
function(req, res, xn) {
  tryCatch({
    year <- as.integer(xn)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT COUNT(*) AS total,
           SUM(CASE WHEN zczkm = 1 THEN 1 ELSE 0 END) AS pass
    FROM bks_zcb
    WHERE SUBSTR(xh, 2, 4) = ?year
      AND xn = ?year
      AND xqm = 1;"
    sql <- sqlInterpolate(pool, sql, year = year)
    temp <- dbGetQuery(pool, sql)
    
    result <- list(
      total = ifelse(is.null(temp$total), 0, temp$total), 
      pass = ifelse(is.null(temp$pass), 0, temp$pass), 
      prob = ifelse(is.null(temp$total) || temp$total == 0, 
                    NULL, round(temp$pass / temp$total, 4))
    )
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生入学 电子注册 概况 生源通过率
#* @param xn
#* @get /regist/summary/probProvince
#* @serializer unboxedJSON
function(req, res, xn) {
  tryCatch({
    year <- as.integer(xn)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT lqb.syssdm,
           COUNT(*) AS n,
           SUM(CASE WHEN zcb.zczkm = 1 THEN 1 ELSE 0 END) AS pass
    FROM
      (SELECT xh,
              zczkm
       FROM bks_zcb
       WHERE SUBSTR(xh, 2, 4) = ?year
         AND xn = ?year
         AND xqm = 1) AS zcb
    LEFT JOIN bks_xjb AS xjb ON zcb.xh = xjb.xh
    LEFT JOIN ks_lqb AS lqb ON xjb.ksh = lqb.ksh
    GROUP BY lqb.syssdm;"
    sql <- sqlInterpolate(pool, sql, year = year)
    temp <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS area
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 6) = '0000';"
    province <- dbGetQuery(pool, sql)
    
    result <- full_join(temp, province, by = 'syssdm') %>% 
      mutate(area = replace_na(area, '其他'), 
             n = replace_na(n, 0), 
             pass = replace_na(pass, 0), 
             prob = if_else(n == 0, 0, round(pass / n, 4))) %>% 
      select(area, prob)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生入学 电子注册 概况 省份历年通过率
#* @param area
#* @get /regist/summary/probYear
#* @serializer unboxedJSON
function(req, res, area) {
  tryCatch({
    
    sql <- "
    SELECT lqb.syssdm,
           zcb.xn,
           COUNT(*) AS n,
           SUM(CASE WHEN zcb.zczkm = 1 THEN 1 ELSE 0 END) AS pass
    FROM
      (SELECT xn,
              xh,
              zczkm
       FROM bks_zcb
       WHERE SUBSTR(xh, 2, 4) = xn
         AND xqm = 1) AS zcb
    LEFT JOIN bks_xjb AS xjb ON zcb.xh = xjb.xh
    LEFT JOIN ks_lqb AS lqb ON xjb.ksh = lqb.ksh
    GROUP BY lqb.syssdm,
             zcb.xn;"
    temp <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS area
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 6) = '0000';"
    province <- dbGetQuery(pool, sql)
    
    temp <- full_join(temp, province, by = 'syssdm') %>%
      filter(area == !!area) %>% 
      mutate(n = replace_na(n, 0), 
             pass = replace_na(pass, 0), 
             prob = if_else(n == 0, 0, round(pass / n, 4))) %>% 
      select(xn, prob)
    
    result <- yearFill(temp)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生入学 电子注册 注册详情
#* @param xn
#* @param area
#* @param zczk
#* @get /regist/detail
#* @serializer unboxedJSON
function(req, res, xn, area, zczk) {
  tryCatch({
    year <- as.integer(xn)
    stopifnot(!is.na(year))
    
    sql_1 <- "
    SELECT lqb.xm,
           (CASE WHEN LENGTH(lqb.kl) = 0 THEN '未知' ELSE lqb.kl END) AS kl,
           (CASE WHEN LENGTH(lqb.lqlx) = 0 THEN '未知' ELSE lqb.lqlx END) AS lqlx,
           (CASE WHEN LENGTH(lqb.zymc) = 0 THEN '未知' ELSE lqb.zymc END) AS zymc,
           lqb.zf,
           zcb.zcrq,
           zcb.encode_item_name AS zczk
    FROM
      (SELECT bks_zcb.xh,
              bks_zcb.zcrq,
              d.encode_item_name
       FROM bks_zcb
       LEFT JOIN ds_encode_item AS d ON bks_zcb.zczkm = d.encode_item_code
       WHERE SUBSTR(bks_zcb.xh, 2, 4) = ?year
         AND bks_zcb.xn = ?year
         AND bks_zcb.xqm = 1
         AND d.encode_type_code = 'ZCZK'"
    
    sql_2 <- "
    ) AS zcb
    LEFT JOIN bks_xjb AS xjb ON zcb.xh = xjb.xh
    LEFT JOIN ks_lqb AS lqb ON xjb.ksh = lqb.ksh
    LEFT JOIN
      (SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
              encode_item_name AS `area`
       FROM ds_encode_item
       WHERE encode_type_code = 'B_ZHRMGHGXZQH'
         AND SUBSTR(encode_item_code, 3, 6) = '0000') AS dict ON lqb.syssdm = dict.syssdm
    WHERE dict.area = ?area;"
    
    # zczk <- match.arg(zczk)
    if (zczk == '全部') {
      sql <- paste(sql_1, sql_2)
      sql <- sqlInterpolate(pool, sql, year = year, area = area)
    } else {
      sql <- paste(sql_1, 'AND d.encode_item_name = ?zczk', sql_2)
      sql <- sqlInterpolate(pool, sql, year = year, area = area, zczk = zczk)
    }
    result <- dbGetQuery(pool, sql)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}
