
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

#* 概览 报考人数（录取人数）
#* @get /peopleCount
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    
    sql <- "
    SELECT lqnf,
           count(*) AS n,
           round(AVG(zf), 2) AS scoreAvg
    FROM ks_lqb
    GROUP BY lqnf;"
    temp <- dbGetQuery(pool, sql)
    
    result <- yearFill(temp)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 概览 生源结构 性别 民族 区域 政治面貌
#* @param lqnf:int
#* @get /source
#* @serializer unboxedJSON
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT (CASE LENGTH(sfzh)
                WHEN 18 THEN IF((SUBSTR(sfzh, 17, 1)%2)=0, '女', '男')
                WHEN 15 THEN IF((SUBSTR(sfzh, 15, 1)%2)=0, '女', '男')
                ELSE '未知'
            END) AS sex,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
    GROUP BY sex
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    sex <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT mzdm AS nation,
       count(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
    GROUP BY mzdm
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    nation <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT dict.encode_item_name AS area,
       lqb.n
    FROM
      (SELECT syssdm,
              COUNT(*) AS n
       FROM ks_lqb
       WHERE lqnf = ?year
       GROUP BY syssdm) AS lqb
    LEFT JOIN ds_encode_item AS dict ON CONCAT(lqb.syssdm, '0000') = dict.encode_item_code
    WHERE dict.encode_type_code = 'B_ZHRMGHGXZQH';"
    sql <- sqlInterpolate(pool, sql, year = year)
    temp <- dbGetQuery(pool, sql)
    temp <- left_join(temp, area, by = c('area' = 'value'))
    temp <- summarise(group_by(temp, name), n = sum(n))
    area <- arrange(temp, desc(n))
    
    sql <- "
    SELECT (CASE
                WHEN LENGTH(zzmmdm) = 0 THEN '未知'
                ELSE zzmmdm
            END) AS zzmm,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
    GROUP BY zzmmdm
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    zzmm <- dbGetQuery(pool, sql)
    
    res_logger(req, res)
    list(sex = sex, 
         nation = nation, 
         area = area, 
         zzmm = zzmm)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 概览 学院与专业
#* @param lqnf
#* @get /peopleMajorCount
#* @serializer unboxedJSON
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT yxsmc,
       COUNT(*) AS n
    FROM bks_xjb
    WHERE SUBSTR(rxny, 1, 4) = ?year
    GROUP BY yxsmc
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    result <- dbGetQuery(pool, sql)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}