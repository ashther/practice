
HOME_PATH <- '/home/rstudio'
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

#* 新生录取 生源分布 生源结构
#* @param lqnf:int
#* @get /source/structure
#* @serializer unboxedJSON
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
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
    area <- dbGetQuery(pool, sql)
    
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
    SELECT (CASE
                WHEN length(kl) = 0 THEN '未知'
                ELSE kl
            END) AS kl,
           count(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
    GROUP BY kl
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    kl <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT (CASE
                WHEN syssdm = 62 THEN '省内'
                ELSE '省外'
            END) AS `area`,
           count(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
    GROUP BY `area`
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    is_in <- dbGetQuery(pool, sql)
    
    # no wish level pie chart
    
    res_logger(req, res)
    list(area = area, 
         sex = sex, 
         kl = kl, 
         isIn = is_in)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 生源分布 省份详情 全部地区
#* @param lqnf
#* @get /source/province
#* @serializer unboxedJSON
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT dict.area,
           lqb.n
    FROM
      (SELECT syssdm,
              COUNT(*) AS n
       FROM ks_lqb
       WHERE lqnf = ?year
       GROUP BY syssdm) AS lqb
    LEFT JOIN
      (SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
              encode_item_name AS `area`
       FROM ds_encode_item
       WHERE encode_type_code = 'B_ZHRMGHGXZQH'
         AND SUBSTR(encode_item_code, 3, 6) = '0000') AS dict ON lqb.syssdm = dict.syssdm
    ORDER BY lqb.n DESC;"
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

#* 新生录取 生源分布 省份详情 单个地区
#* @param lqnf
#* @param area
#* @get /source/province/detail
#* @serializer unboxedJSON
function(req, res, lqnf, area) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT kl,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
      AND syssdm =
        (SELECT SUBSTR(encode_item_code, 1, 2)
         FROM ds_encode_item
         WHERE encode_type_code = 'B_ZHRMGHGXZQH'
           AND encode_item_name = ?area)
    GROUP BY kl
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    kl <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT (CASE LENGTH(sfzh)
                WHEN 18 THEN IF((SUBSTR(sfzh, 17, 1)%2)=0, '女', '男')
                WHEN 15 THEN IF((SUBSTR(sfzh, 15, 1)%2)=0, '女', '男')
                ELSE '未知'
            END) AS sex,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
      AND syssdm =
        (SELECT SUBSTR(encode_item_code, 1, 2)
         FROM ds_encode_item
         WHERE encode_type_code = 'B_ZHRMGHGXZQH'
           AND encode_item_name = ?area)
    GROUP BY sex
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    sex <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT kl,
           MAX(zf) AS scoreMax,
           ROUND(AVG(zf), 2) AS scoreAvg,
           MIN(zf) AS scoreMin
    FROM ks_lqb
    WHERE lqnf = ?year
      AND syssdm =
        (SELECT SUBSTR(encode_item_code, 1, 2)
         FROM ds_encode_item
         WHERE encode_type_code = 'B_ZHRMGHGXZQH'
           AND encode_item_name = ?area)
    GROUP BY kl;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    score <- dbGetQuery(pool, sql)
    
    res_logger(req, res)
    list(kl = kl, sex = sex, score = score)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 生源分布 专业详情 全部专业
#* @param lqnf
#* @get /source/major
#* @serializer unboxedJSON
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT zymc,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
    GROUP BY zymc
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

#* 新生录取 生源分布 专业详情 单个专业
#* @param lqnf
#* @param zymc
#* @get /source/major/detail
#* @serializer unboxedJSON
function(req, res, lqnf, zymc) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT kl,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
      AND zymc = ?zymc
    GROUP BY kl
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year, zymc = zymc)
    kl <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT (CASE LENGTH(sfzh)
                WHEN 18 THEN IF((SUBSTR(sfzh, 17, 1)%2)=0, '女', '男')
                WHEN 15 THEN IF((SUBSTR(sfzh, 15, 1)%2)=0, '女', '男')
                ELSE '未知'
            END) AS sex,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
      AND zymc = ?zymc
    GROUP BY sex
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year, zymc = zymc)
    sex <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT kl,
           MAX(zf) AS scoreMax,
           ROUND(AVG(zf), 2) AS scoreAvg,
           MIN(zf) AS scoreMin
    FROM ks_lqb
    WHERE lqnf = ?year
      AND zymc = ?zymc
    GROUP BY kl;"
    sql <- sqlInterpolate(pool, sql, year = year, zymc = zymc)
    score <- dbGetQuery(pool, sql)
    
    res_logger(req, res)
    list(kl = kl, sex = sex, score = score)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 录取分数 概况 绝对值
#* @param lqnf
#* @param kl
#* @param lqlx
#* @param item
#* @get /score/summary/absolute
#* @serializer unboxedJSON
function(req, res, lqnf, kl, lqlx, item) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql_begin <- "
    SELECT dict.encode_item_name AS area,
       lqb.score_max,
       lqb.score_min,
       lqb.score_avg
    FROM
      (SELECT syssdm,
              MAX(zf) AS score_max,
              MIN(zf) AS score_min,
              ROUND(AVG(zf), 2) AS score_avg
       FROM ks_lqb
       WHERE lqnf = ?year"
    
    sql_kl <- "AND kl = ?kl"
    sql_lqlx <- "AND lqlx = ?lqlx"
    
    sql_end <- "
       GROUP BY syssdm) AS lqb
    LEFT JOIN ds_encode_item AS dict ON CONCAT(lqb.syssdm, '0000') = dict.encode_item_code
    WHERE dict.encode_type_code = 'B_ZHRMGHGXZQH';"
    
    params <- list(year = year)
    if (kl != '全部') {
      sql_begin <- paste(sql_begin, sql_kl, sep = ' ')
      params$kl <- kl
    }
    if (lqlx != '全部') {
      sql_begin <- paste(sql_begin, sql_lqlx, sep = ' ')
      params$lqlx <- lqlx
    }
    sql <- paste(sql_begin, sql_end)
    
    sql <- sqlInterpolate(pool, sql, .dots = params)
    temp <- dbGetQuery(pool, sql)
    if (item == '最高分') {
      result <- rename(select(temp, area, score_max), score = score_max)
    } else if (item == '最低分') {
      result <- rename(select(temp, area, score_min), score = score_min)
    } else if (item == '平均分') {
      result <- rename(select(temp, area, score_avg), score = score_avg)
    } else {
      stop('Not correct parameter item.')
    }
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 录取分数 省份详情
#* @param lqnf
#* @param area
#* @get /score/province
#* @serializer unboxedJSON
function(req, res, lqnf, area) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT dict.encode_item_name AS area,
           lqb.kl,
           lqb.lqlx,
           lqb.scoreMax, 
           lqb.scoreMin, 
           lqb.scoreAvg, 
           lqb.n
    FROM
      (SELECT syssdm,
              (CASE WHEN LENGTH(kl) = 0 THEN '未知' ELSE kl END) AS kl,
              (CASE WHEN LENGTH(lqlx) = 0 THEN '未知' ELSE lqlx END) AS lqlx,
              MAX(zf) AS scoreMax,
              MIN(zf) AS scoreMin,
              AVG(zf) AS scoreAvg,
              COUNT(*) AS n
       FROM ks_lqb
       WHERE lqnf = ?year
       GROUP BY kl,
                lqlx,
                syssdm) AS lqb
    LEFT JOIN ds_encode_item AS dict ON CONCAT(lqb.syssdm, '0000') = dict.encode_item_code
    WHERE dict.encode_type_code = 'B_ZHRMGHGXZQH'
      AND dict.encode_item_name = ?area;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    result <- dbGetQuery(pool, sql)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 录取分数 专业详情
#* @param lqnf
#* @param area
#* @get /score/major
#* @serializer unboxedJSON
function(req, res, lqnf, area) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT dict.encode_item_name AS area,
           lqb.kl,
           lqb.lqlx,
           lqb.zymc,
           lqb.scoreMax,
           lqb.scoreMin,
           lqb.scoreAvg,
           lqb.n
    FROM
      (SELECT syssdm,
              (CASE WHEN LENGTH(kl) = 0 THEN '未知' ELSE kl END) AS kl,
              (CASE WHEN LENGTH(lqlx) = 0 THEN '未知' ELSE lqlx END) AS lqlx,
              (CASE WHEN LENGTH(zymc) = 0 THEN '未知' ELSE zymc END) AS zymc,
              MAX(zf) AS scoreMax,
              MIN(zf) AS scoreMin,
              AVG(zf) AS scoreAvg,
              COUNT(*) AS n
       FROM ks_lqb
       WHERE lqnf = ?year
       GROUP BY kl,
                lqlx,
                zymc,
                syssdm) AS lqb
    LEFT JOIN ds_encode_item AS dict ON CONCAT(lqb.syssdm, '0000') = dict.encode_item_code
    WHERE dict.encode_type_code = 'B_ZHRMGHGXZQH'
      AND dict.encode_item_name = ?area;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    result <- dbGetQuery(pool, sql)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 录取人数 概况 省份变迁
#* @param area
#* @get /count/summary/trend
#* @serializer unboxedJSON
function(req, res, area) {
  tryCatch({
    
    sql <- "
    SELECT lqnf,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE syssdm =
        (SELECT SUBSTR(encode_item_code, 1, 2)
         FROM ds_encode_item
         WHERE encode_item_name = ?area) 
    GROUP BY lqnf;"
    sql <- sqlInterpolate(pool, sql, area = area)
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

#* 新生录取 录取人数 概况 top10
#* @param lqnf
#* @get /count/summary/top
#* @serializer unboxedJSON
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT a.area,
          temp.n
    FROM
     (SELECT syssdm,
             COUNT(*) AS n
      FROM ks_lqb
      WHERE lqnf = ?year
      GROUP BY syssdm) AS temp
    LEFT JOIN
     (SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
             encode_item_name AS area
      FROM ds_encode_item
      WHERE encode_type_code = 'B_ZHRMGHGXZQH'
        AND SUBSTR(encode_item_code, 3, 4) = '0000') AS a ON temp.syssdm = a.syssdm;"
    sql_this_year <- sqlInterpolate(pool, sql, year = year)
    sql_last_year <- sqlInterpolate(pool, sql, year = (as.integer(year) - 1))
    this_year <- dbGetQuery(pool, sql_this_year)
    last_year <- dbGetQuery(pool, sql_last_year)
    
    this_year_top10 <- head(arrange(this_year, desc(n)), 10)
    
    temp <- full_join(this_year, last_year, by = 'area') %>% 
      mutate(n.x = if_else(is.na(n.x), 0, n.x), 
             n.y = if_else(is.na(n.y), 0, n.y), 
             n = n.x - n.y) %>% 
      arrange(desc(n)) %>% 
      select(area, n)
    yoy_max_top10 <- head(filter(temp, n > 0), 10)
    yoy_min_top <- filter(temp, n < 0) %>% 
      tail(10) %>% 
      mutate(n = -1 * n) %>% 
      arrange(desc(n))
    
    area_sum <- left_join(this_year, area, by = c('area' = 'value')) %>% 
      group_by(name) %>% 
      summarise(n = sum(n)) %>% 
      rename(area = name) %>% 
      arrange(desc(n))
    
    res_logger(req, res)
    list(thisYearTop = this_year_top10, 
         yoyMaxTop = yoy_max_top10, 
         yoyMinTop = yoy_min_top, 
         areaSum = area_sum)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 录取人数 省份详情
#* @get /count/province
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    
    sql <- "
    SELECT syssdm,
           lqnf as year,
           COUNT(*) AS n
    FROM ks_lqb
    GROUP BY syssdm,
             lqnf;"
    area_year_n <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS area
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 4) = '0000';"
    province <- dbGetQuery(pool, sql)
    
    result <- left_join(area_year_n, province, by = 'syssdm') %>% 
      select(-syssdm) %>% 
      spread(year, n, fill = 0)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 录取人数 专业详情
#* @param lqnf
#* @param kl
#* @param lqlx
#* @get /count/major
#* @serializer unboxedJSON
function(req, res, lqnf, kl, lqlx) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql_begin <- "
    SELECT syssdm,
           (CASE WHEN LENGTH(zymc) = 0 THEN '未知' ELSE zymc END) AS zymc,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year"
    
    sql_kl <- "AND kl = ?kl"
    sql_lqlx <- "AND lqlx = ?lqlx"
    
    sql_end <- "
    GROUP BY syssdm,
             zymc;"
    
    params <- list(year = year)
    if (kl != '全部') {
      sql_begin <- paste(sql_begin, sql_kl, sep = ' ')
      params$kl <- kl
    }
    if (lqlx != '全部') {
      sql_begin <- paste(sql_begin, sql_lqlx, sep = ' ')
      params$lqlx <- lqlx
    }
    sql <- paste(sql_begin, sql_end)
    
    sql <- sqlInterpolate(pool, sql, .dots = params)
    area_major_n <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS area
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 4) = '0000';"
    province <- dbGetQuery(pool, sql)
    
    result <- left_join(area_major_n, province, by = 'syssdm') %>% 
      select(-syssdm) %>% 
      spread(area, n, fill = 0)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 录取专业 概况 科类和类别结构
#* @param lqnf
#* @get /major/summary/structure
#* @serializer unboxedJSON
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT (CASE WHEN LENGTH(kl) = 0 THEN '未知' ELSE kl END) AS kl,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = 2017
    GROUP BY kl
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    kl <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT (CASE WHEN LENGTH(lqlx) = 0 THEN '未知' ELSE lqlx END) AS lqlx,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = 2017
    GROUP BY lqlx
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    lqlx <- dbGetQuery(pool, sql)
    
    res_logger(req, res)
    list(kl = kl, lqlx = lqlx)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 录取专业 概况 top10
#* @param lqnf
#* @get /major/summary/top
#* @serializer unboxedJSON
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT (CASE WHEN LENGTH(zymc) = 0 THEN '未知' ELSE zymc END) AS zymc, 
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
    GROUP BY zymc;"
    sql_this_year <- sqlInterpolate(pool, sql, year = year)
    sql_last_year <- sqlInterpolate(pool, sql, year = (as.integer(year) - 1))
    this_year <- dbGetQuery(pool, sql_this_year)
    last_year <- dbGetQuery(pool, sql_last_year)
    
    this_year_top10 <- head(arrange(this_year, desc(n)), 10)
    
    temp <- full_join(this_year, last_year, by = 'zymc') %>% 
      mutate(n.x = if_else(is.na(n.x), 0, n.x), 
             n.y = if_else(is.na(n.y), 0, n.y), 
             n = n.x - n.y) %>% 
      arrange(desc(n)) %>% 
      select(zymc, n)
    yoy_max_top10 <- head(filter(temp, n > 0), 10)
    yoy_min_top <- filter(temp, n < 0) %>% 
      tail(10) %>% 
      mutate(n = -1 * n) %>% 
      arrange(desc(n))
    
    res_logger(req, res)
    list(thisYearTop = this_year_top10, 
         yoyMaxTop = yoy_max_top10, 
         yoyMinTop = yoy_min_top)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 录取专业 概况 专业人数变迁
#* @param area
#* @param kl
#* @param lqlx
#* @param zymc
#* @get /major/summary/trend
#* @serializer unboxedJSON
function(req, res, area, kl, lqlx, zymc) {
  tryCatch({
    sql_begin <- "
    SELECT lqb.lqnf, 
           lqb.n
    FROM
      (SELECT syssdm, 
              lqnf,
              COUNT(*) AS n
       FROM ks_lqb"
    
    sql_kl <- 'where kl = ?kl'
    sql_lqlx <- 'and lqlx = ?lqlx'
    sql_zymc <- 'and zymc = ?zymc'
    
    sql_end <- "
       GROUP BY syssdm, 
                lqnf) AS lqb
    LEFT JOIN
      (SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
              encode_item_name AS `area`
       FROM ds_encode_item
       WHERE encode_type_code = 'B_ZHRMGHGXZQH'
         AND SUBSTR(encode_item_code, 3, 4) = '0000') AS dict ON lqb.syssdm = dict.syssdm
    WHERE dict.area = ?area
    ORDER BY lqnf;"
    
    params <- list(area = area)
    if (kl != '全部') {
      sql_begin <- paste(sql_begin, sql_kl, sep = ' ')
      params$kl <- kl
    }
    if (lqlx != '全部') {
      sql_begin <- paste(sql_begin, sql_lqlx, sep = ' ')
      params$lqlx <- lqlx
    }
    if (zymc != '全部') {
      sql_begin <- paste(sql_begin, sql_zymc, sep = ' ')
      params$zymc <- zymc
    }
    sql_begin <- sqlFill(sql_begin)
    sql <- paste(sql_begin, sql_end)
    
    sql <- sqlInterpolate(pool, sql, .dots = params)
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

#* 新生录取 录取专业 录取分数
#* @param lqnf
#* @param area
#* @get /major/score
#* @serializer unboxedJSON
function(req, res, lqnf, area) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT lqb.kl,
           lqb.zymc,
           lqb.scoreMax,
           lqb.scoreMin,
           lqb.scoreAvg,
           lqb.lqlx,
           lqb.n
    FROM
      (SELECT syssdm,
              (CASE WHEN LENGTH(kl) = 0 THEN '未知' ELSE kl END) AS kl,
              (CASE WHEN LENGTH(zymc) = 0 THEN '未知' ELSE zymc END) AS zymc,
              MAX(zf) AS scoreMax,
              MIN(zf) AS scoreMin,
              ROUND(AVG(zf), 2) AS scoreAvg,
              (CASE WHEN LENGTH(lqlx) = 0 THEN '未知' ELSE lqlx END) AS lqlx,
              COUNT(*) AS n
       FROM ks_lqb
       WHERE lqnf = ?year
       GROUP BY kl,
                zymc,
                lqlx) AS lqb
    LEFT JOIN
      (SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
              encode_item_name AS `area`
       FROM ds_encode_item
       WHERE encode_type_code = 'B_ZHRMGHGXZQH'
         AND SUBSTR(encode_item_code, 3, 4) = '0000') AS dict ON lqb.syssdm = dict.syssdm
    WHERE dict.area = ?area;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    result <- dbGetQuery(pool, sql)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生录取 录取专业 录取人数
#* @param lqnf
#* @param kl
#* @param lqlx
#* @get /major/count
#* @serializer unboxedJSON
function(req, res, lqnf, kl, lqlx) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))
    
    sql_begin <- "
    SELECT syssdm,
           (CASE WHEN LENGTH(zymc) = 0 THEN '未知' ELSE zymc END) AS zymc,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year"
    
    sql_kl <- "AND kl = ?kl"
    sql_lqlx <- "AND lqlx = ?lqlx"
    
    sql_end <- "
    GROUP BY syssdm,
             zymc;"
    
    params <- list(year = year)
    if (kl != '全部') {
      sql_begin <- paste(sql_begin, sql_kl)
      params$kl <- kl
    }
    if (lqlx != '全部') {
      sql_begin <- paste(sql_begin, sql_lqlx)
      params$lqlx <- lqlx
    }
    sql <- paste(sql_begin, sql_end)
    
    sql <- sqlInterpolate(pool, sql, .dots = params)
    temp <- dbGetQuery(pool, sql)
    
    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS area
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 4) = '0000';"
    province <- dbGetQuery(pool, sql)
    
    result <- left_join(temp, province, by = 'syssdm') %>% 
      select(-syssdm) %>% 
      spread(area, n, fill = 0)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}