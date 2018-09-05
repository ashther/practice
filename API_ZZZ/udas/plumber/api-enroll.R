
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

#* 新生入学 新生复查 概况 基本情况 # 复查通过人数在复查结论数据补全后再调整
#* @param xn
#* @get /review/summary/info
#* @serializer unboxedJSON
function(req, res, xn) {
  tryCatch({
    year <- as.integer(xn)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT COUNT(*) AS total,
           SUM(CASE WHEN dasysfsfyz = 1
               AND dabyzxmcsfyz = 1
               AND sfzhsfyz = 1
               AND zpxmsfyz = 1
               AND daywtg = 0
               AND xmywbg = 0
               AND ywyc = 0
               AND ywwgjf = 0 THEN 1 ELSE 0 END) AS pass,
           SUM(CASE WHEN dasysfsfyz <> 1 THEN 1 ELSE 0 END) AS reason_dasysfsfyz,
           SUM(CASE WHEN dabyzxmcsfyz <> 1 THEN 1 ELSE 0 END) AS reason_dabyzxmcsfyz,
           SUM(CASE WHEN sfzhsfyz <> 1 THEN 1 ELSE 0 END) AS reason_sfzhsfyz,
           SUM(CASE WHEN zpxmsfyz <> 1 THEN 1 ELSE 0 END) AS reason_zpxmsfyz,
           SUM(CASE WHEN daywtg = 1 THEN 1 ELSE 0 END) AS reason_daywtg,
           SUM(CASE WHEN xmywbg = 1 THEN 1 ELSE 0 END) AS reason_xmywbg,
           SUM(CASE WHEN ywyc <> 0 THEN 1 ELSE 0 END) AS reason_ywyc,
           SUM(CASE WHEN ywwgjf <> 0 THEN 1 ELSE 0 END) AS reason_ywwgjf
    FROM bks_zgfsb
    WHERE SUBSTR(xh, 2, 4) = ?year;"
    sql <- sqlInterpolate(pool, sql, year = year)
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    
    result <- list(
      total = ifelse(is.null(temp$total), 0, temp$total), 
      pass = ifelse(is.null(temp$pass), 0, temp$pass), 
      noPass = temp$total - temp$pass, 
      nReason = sum(select(temp, starts_with('reason')), na.rm = TRUE), 
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

#* 新生入学 新生复查 概况 复查未通过原因
#* @param zgfs
#* @get /review/summary/reason
function(req, res, zgfs) {
  tryCatch({
    if (!zgfs %in% zgfs_reason$name & zgfs != '全部') {
      stop('Not correct selection.')
    }
    
    sql <- "
    SELECT SUBSTR(xh, 2, 4) AS `year`,
           SUM(CASE WHEN dasysfsfyz <> 1 THEN 1 ELSE 0 END) AS dasysfsfyz,
           SUM(CASE WHEN dabyzxmcsfyz <> 1 THEN 1 ELSE 0 END) AS dabyzxmcsfyz,
           SUM(CASE WHEN sfzhsfyz <> 1 THEN 1 ELSE 0 END) AS sfzhsfyz,
           SUM(CASE WHEN zpxmsfyz <> 1 THEN 1 ELSE 0 END) AS zpxmsfyz,
           SUM(CASE WHEN daywtg = 1 THEN 1 ELSE 0 END) AS daywtg,
           SUM(CASE WHEN xmywbg = 1 THEN 1 ELSE 0 END) AS xmywbg,
           SUM(CASE WHEN ywyc <> 0 THEN 1 ELSE 0 END) AS ywyc,
           SUM(CASE WHEN ywwgjf <> 0 THEN 1 ELSE 0 END) AS ywwgjf
    FROM bks_zgfsb
    GROUP BY `year`;"
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    
    if (zgfs == '全部') {
      result <- tibble(
        year = temp$year, 
        n = apply(select(temp, -year), 1, sum)
      )
    } else {
      zgfs_code <- zgfs_reason$code[zgfs_reason$name == zgfs]
      result <- select(temp, year, !!zgfs_code)
      result <- setNames(result, c('year', 'n'))
    }
    result <- yearFill(result)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生入学 新生复查 概况 年份生源复审
#* @param xn
#* @param item
#* @get /review/summary/province
function(req, res, xn, item) {
  tryCatch({
    year <- as.integer(xn)
    stopifnot(!is.na(year))
    stopifnot(item %in% c('通过率', '总人数'))
    
    sql <- "
    SELECT syssdm,
           COUNT(*) AS total,
           SUM(is_pass) AS pass
    FROM
      (SELECT zgfsb.*,
              lqb.syssdm
       FROM
         (SELECT xh,
                 (CASE WHEN dasysfsfyz = 1
                  AND dabyzxmcsfyz = 1
                  AND sfzhsfyz = 1
                  AND zpxmsfyz = 1
                  AND daywtg = 0
                  AND xmywbg = 0
                  AND ywyc = 0
                  AND ywwgjf = 0 THEN 1 ELSE 0 END) AS is_pass
          FROM bks_zgfsb
          WHERE SUBSTR(xh, 2, 4) = ?year) AS zgfsb
       LEFT JOIN bks_xjb ON zgfsb.xh = bks_xjb.xh
       LEFT JOIN ks_lqb AS lqb ON bks_xjb.ksh = lqb.ksh) AS t
    GROUP BY syssdm
    ORDER BY total DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    
    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS `area`
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 6) = '0000';"
    province <- dbGetQuery(pool, sql)
    
    temp <- left_join(temp, province, by = 'syssdm') %>% 
      select(area, total, pass) %>% 
      mutate(area = replace_na(area, '其他'), 
             prob = round(pass/total, 4))
    if (item == '通过率') {
      result <- select(temp, area, prob)
    }
    if (item == '总人数') {
      result <- select(temp, area, total)
    }
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生入学 新生复查 概况 省份历年复审
#* @param area
#* @param item
#* @get /review/summary/trend
function(req, res, area, item) {
  tryCatch({
    stopifnot(item %in% c('通过率', '总人数'))
    
    sql <- "
    SELECT SUBSTR(zgfsb.xh, 2, 4) AS `year`,
           COUNT(*) AS total,
           SUM(is_pass) AS pass
    FROM
      (SELECT xh,
              (CASE WHEN dasysfsfyz = 1
               AND dabyzxmcsfyz = 1
               AND sfzhsfyz = 1
               AND zpxmsfyz = 1
               AND daywtg = 0
               AND xmywbg = 0
               AND ywyc = 0
               AND ywwgjf = 0 THEN 1 ELSE 0 END) AS is_pass
       FROM bks_zgfsb) AS zgfsb
    LEFT JOIN bks_xjb ON zgfsb.xh = bks_xjb.xh
    LEFT JOIN ks_lqb AS lqb ON bks_xjb.ksh = lqb.ksh
    WHERE lqb.syssdm =
        ( SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm
         FROM ds_encode_item
         WHERE encode_type_code = 'B_ZHRMGHGXZQH'
           AND encode_item_name = ?area)
    GROUP BY `year`;"
    sql <- sqlInterpolate(pool, sql, area = area)
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    
    temp <- mutate(temp, prob = round(pass/total, 4))
    if (item == '通过率') {
      result <- yearFill(select(temp, year, prob))
    }
    if (item == '总人数') {
      result <- yearFill(select(temp, year, total))
    }
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生入学 新生复查 复查详情
#* @param xn
#* @param area
#* @param item
#* @get /review/detail
function(req, res, xn, area, item) {
  tryCatch({
    year <- as.integer(xn)
    stopifnot(!is.na(year))
    stopifnot(item %in% c('全部', '通过', '未通过'))
    
    sql <- "
    SELECT lqb.xm,
           lqb.kl,
           lqb.lqlx,
           lqb.zymc,
           lqb.zf,
           zgfsb.*
    FROM bks_zgfsb AS zgfsb
    LEFT JOIN bks_xjb ON zgfsb.xh = bks_xjb.xh
    LEFT JOIN ks_lqb AS lqb ON bks_xjb.ksh = lqb.ksh
    WHERE lqb.syssdm =
        (SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm
         FROM ds_encode_item
         WHERE encode_type_code = 'B_ZHRMGHGXZQH'
           AND encode_item_name = ?area)
      AND SUBSTR(zgfsb.xh, 2, 4) = ?year;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    
    temp <- mutate(temp, is_pass = case_when(
      dasysfsfyz == '1' &
        dabyzxmcsfyz == '1' &
        sfzhsfyz == '1' &
        zpxmsfyz == '1' &
        daywtg == '0' &
        xmywbg == '0' &
        ywyc == '0' &
        ywwgjf == '0' ~ '通过', 
      dasysfsfyz != '1' |
        dabyzxmcsfyz != '1' | 
        sfzhsfyz != '1' |
        zpxmsfyz != '1' |
        daywtg == '1' |
        xmywbg == '1' |
        ywyc != '0' |
        ywwgjf != '0' ~ '未通过', 
      TRUE ~ '其他'
    ))
    
    if (item != '全部') {
      temp <- filter(temp, is_pass == !!item)
    }
    temp$remarks <- purrr::pmap_chr(
      select(temp, dasysfsfyz:ywwgjf), ~ {
        res <- c()
        if (..1 != '1') res <- c(res, zgfs_reason$name[1])
        if (..2 != '1') res <- c(res, zgfs_reason$name[2])
        if (..3 != '1') res <- c(res, zgfs_reason$name[3])
        if (..4 != '1') res <- c(res, zgfs_reason$name[4])
        if (..5 == '1') res <- c(res, zgfs_reason$name[5])
        if (..6 == '1') res <- c(res, zgfs_reason$name[6])
        if (..7 != '0') res <- c(res, zgfs_reason$name[7])
        if (..8 != '0') res <- c(res, zgfs_reason$name[8])
        paste0(res, collapse = '，')
      }  
    )
    
    result <- select(temp, xm, kl, lqlx, zymc, zf, is_pass, remarks) %>% 
      rename(isPass = is_pass)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
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
           SUM(CASE WHEN zczkm = 1 THEN 1 ELSE 0 END) AS pass,
           SUM(CASE WHEN zczkm = 4 THEN 1 ELSE 0 END) AS noRegist,
           SUM(CASE WHEN zczkm = 3 THEN 1 ELSE 0 END) AS noCheckin
    FROM bks_zcb
    WHERE SUBSTR(xh, 2, 4) = ?year
      AND xn = ?year
      AND xqm = 1;"
    sql <- sqlInterpolate(pool, sql, year = year)
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    
    result <- list(
      total = ifelse(is.null(temp$total), 0, temp$total), 
      pass = ifelse(is.null(temp$pass), 0, temp$pass), 
      prob = ifelse(is.null(temp$total) || temp$total == 0, 
                    NULL, round(temp$pass / temp$total, 4)), 
      noRegist = ifelse(is.null(temp$noRegist), 0, temp$noRegist), 
      noCheckin = ifelse(is.null(temp$noCheckin), 0, temp$noCheckin)
    )
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生入学 电子注册 概况 注册状态
#* @param zczk
#* @get /regist/summary/status
function(req, res, zczk) {
  tryCatch({
    
    sql_begin <- "
    SELECT xn,
           COUNT(*) AS n
    FROM bks_zcb
    WHERE SUBSTR(xh, 2, 4) = xn
      AND xqm = 1 "
    
    sql_zczk <- "
     AND zczkm =
        (SELECT encode_item_code
         FROM ds_encode_item
         WHERE encode_type_code = 'ZCZK'
           AND encode_item_name = ?zczk) "
    
    sql_end <- "
     GROUP BY xn;"
    
    if (zczk != '全部') {
      sql <- paste0(sql_begin, sql_zczk, sql_end)
      sql <- sqlInterpolate(pool, sql, zczk = zczk)
    } else {
      sql <- paste0(sql_begin, sql_end)
    }
    
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    result <- yearFill(temp)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生入学 电子注册 概况 年份生源注册
#* @param xn
#* @param item
#* @get /regist/summary/province
function(req, res, xn, item) {
  tryCatch({
    year <- as.integer(xn)
    stopifnot(!is.na(year))
    stopifnot(item %in% c('通过率', '总人数'))
    
    sql <- "
    SELECT lqb.syssdm,
           COUNT(*) AS total,
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
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    
    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS area
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 6) = '0000';"
    province <- dbGetQuery(pool, sql)
    
    temp <- full_join(temp, province, by = 'syssdm') %>% 
      mutate(area = replace_na(area, '其他'), 
             total = replace_na(total, 0), 
             pass = replace_na(pass, 0), 
             prob = if_else(total == 0, 0, round(pass / total, 4)))
    
    if (item == '通过率') {
      result <- select(temp, area, prob)
    } else {
      result <- select(temp, area, total)
    }
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}

#* 新生入学 电子注册 概况 省份历年注册
#* @param area
#* @param item
#* @get /regist/summary/trend
function(req, res, area, item) {
  tryCatch({
    
    sql <- "
    SELECT lqb.syssdm,
           zcb.xn,
           COUNT(*) AS total,
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
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    
    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS area
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 6) = '0000';"
    province <- dbGetQuery(pool, sql)
    
    temp <- full_join(temp, province, by = 'syssdm') %>%
      filter(area == !!area) %>% 
      mutate(total = replace_na(total, 0), 
             pass = replace_na(pass, 0), 
             prob = if_else(total == 0, 0, round(pass / total, 4)))
    
    if (item == '通过率') {
      temp <- select(temp, xn, prob)
    } else {
      temp <- select(temp, xn, total)
    }
    
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
#* @param item
#* @get /regist/detail
function(req, res, xn, area, item) {
  tryCatch({
    year <- as.integer(xn)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT lqb.xm,
           (CASE
                WHEN LENGTH(lqb.kl) = 0 THEN '未知'
                ELSE lqb.kl
            END) AS kl,
           (CASE
                WHEN LENGTH(lqb.lqlx) = 0 THEN '未知'
                ELSE lqb.lqlx
            END) AS lqlx,
           (CASE
                WHEN LENGTH(lqb.zymc) = 0 THEN '未知'
                ELSE lqb.zymc
            END) AS zymc,
           lqb.zf,
           zcb.zcrq,
           zcb.zczkm as zczk
    FROM
      (SELECT xh,
              zczkm,
              zcrq
       FROM bks_zcb
       WHERE SUBSTR(xh, 2, 4) = ?year
         AND xn = ?year
         AND xqm = 1) AS zcb
    LEFT JOIN bks_xjb AS xjb ON zcb.xh = xjb.xh
    LEFT JOIN ks_lqb AS lqb ON xjb.ksh = lqb.ksh
    LEFT JOIN
      (SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
              encode_item_name AS `area`
       FROM ds_encode_item
       WHERE encode_type_code = 'B_ZHRMGHGXZQH'
         AND SUBSTR(encode_item_code, 3, 6) = '0000') AS dict ON lqb.syssdm = dict.syssdm
    WHERE dict.area = ?area;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    temp <- mutate(
      temp, 
      zcrq = paste0(
        substr(zcrq, 1, 4), '/', substr(zcrq, 5, 6), '/', substr(zcrq, 7, 8)
      ), 
      zczk = case_when(zczk == '1' ~ '通过', TRUE ~ '未通过'), 
      remarks = ''
    )
    if (item != '全部') {
      temp <- filter(temp, zczk == !!item)
    }
    result <- temp
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}
