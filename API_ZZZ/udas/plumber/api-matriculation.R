
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


#* @param lqnf:int
#* @get /source/structure
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
    kl <- klTransfer(kl)

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

    sql <- "
    SELECT (CASE
                WHEN length(zzmmdm) = 0 THEN '未知'
                ELSE zzmmdm
            END) AS zzmm,
           count(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
    GROUP BY zzmmdm
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    zzmm <- dbGetQuery(pool, sql)

    res_logger(req, res)
    list(area = area,
         kl = kl,
         sex = sex,
         zzmm = zzmm,
         isIn = is_in)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param lqnf
#* @get /source/province
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))

    sql <- "
    SELECT syssdm,
           kl,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
    GROUP BY syssdm,
             kl;"
    sql <- sqlInterpolate(pool, sql, year = year)
    area_kl_n <- dbGetQuery(pool, sql)
    area_kl_n <- klTransfer(area_kl_n)
    area_kl_n <- add_count(area_kl_n, syssdm, wt = n)

    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS `area`
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 6) = '0000';"
    province <- dbGetQuery(pool, sql)

    result <- left_join(area_kl_n, province, by = 'syssdm') %>%
      select(-syssdm) %>%
      spread(kl, n, fill = 0) %>%
      arrange(desc(nn)) %>%
      rename('实际录取' = nn) %>%
      select(area, `实际录取`, `理工`, `文史`, `艺术`, `体育`, `其他`)

    res_logger(req, res)
    return(result)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param lqnf
#* @param area
#* @get /source/province/detail
function(req, res, lqnf, area) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))

    sql <- "
    SELECT kl,
           MAX(zf) AS scoreMax,
           MIN(zf) AS scoreMin,
           ROUND(AVG(zf), 2) AS scoreAvg,
           lqlx,
           count(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
      AND syssdm =
        (SELECT SUBSTR(encode_item_code, 1, 2)
         FROM ds_encode_item
         WHERE encode_type_code = 'B_ZHRMGHGXZQH'
           AND encode_item_name = ?area)
    GROUP BY kl,
             lqlx
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    temp <- suppressWarnings(dbGetQuery(pool, sql))

    sql <- "
    SELECT cxkl as kl,
           zdx
    FROM zdx
    WHERE sf = ?area
    AND nf = ?year;"
    sql <- sqlInterpolate(pool, sql, area = area, year = year)
    zdx <- suppressWarnings(dbGetQuery(pool, sql))

    # to be continued
    left_join(temp, zdx, by = 'kl') %>%
      mutate(zdx = as.numeric(zdx),
             diffMax = scoreMax - zdx,
             diffMin = scoreMin - zdx,
             diffAvg = scoreAvg - zdx)

    res_logger(req, res)
    return(result)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param lqnf
#* @param kl
#* @param lqlx
#* @get /source/major
function(req, res, lqnf, kl, lqlx) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))

    sql_begin <- "
    SELECT zymc,
           syssdm,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year"

    sql_kl <- klSplit(kl)
    sql_lqlx <- " AND lqlx = ?lqlx "

    sql_end <- "
    GROUP BY zymc,
             syssdm
    ORDER BY n DESC;"

    params <- list(year = year)
    sql_begin <- paste(sql_begin, sql_kl)

    if (lqlx != '全部') {
      sql_begin <- paste(sql_begin, sql_lqlx)
      params$lqlx <- lqlx
    }
    sql <- paste(sql_begin, sql_end)
    sql <- sqlInterpolate(pool, sql, .dots = params)
    temp <- dbGetQuery(pool, sql)

    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS `area`
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 6) = '0000';"
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


#* @param lqnf
#* @param zymc
#* @get /source/major/detail
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


#* @param lqnf
#* @param kl
#* @param lqlx
#* @param item
#* @get /score/summary/absolute
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

    sql_kl <- klSplit(kl)
    sql_lqlx <- "AND lqlx = ?lqlx"

    sql_end <- "
       GROUP BY syssdm) AS lqb
    LEFT JOIN ds_encode_item AS dict ON CONCAT(lqb.syssdm, '0000') = dict.encode_item_code
    WHERE dict.encode_type_code = 'B_ZHRMGHGXZQH';"

    params <- list(year = year)
    sql_begin <- paste0(sql_begin, sql_kl)

    if (lqlx != '全部') {
      sql_begin <- paste(sql_begin, sql_lqlx, sep = ' ')
      params$lqlx <- lqlx
    }
    sql <- paste(sql_begin, sql_end)

    sql <- sqlInterpolate(pool, sql, .dots = params)
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    if (item == '最高分') {
      result <- rename(select(temp, area, score_max), score = score_max)
    } else if (item == '最低分') {
      result <- rename(select(temp, area, score_min), score = score_min)
    } else if (item == '平均分') {
      result <- rename(select(temp, area, score_avg), score = score_avg)
    } else {
      stop('Not correct parameter item.')
    }

    result <- mutate(result, scoreAdmission = NA) # need to change after data got

    res_logger(req, res)
    return(result)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param lqnf
#* @param kl
#* @param lqlx
#* @param item
#* @get /score/summary/relative
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

    sql_kl <- klSplit(kl)
    sql_lqlx <- "AND lqlx = ?lqlx"

    sql_end <- "
    GROUP BY syssdm) AS lqb
    LEFT JOIN ds_encode_item AS dict ON CONCAT(lqb.syssdm, '0000') = dict.encode_item_code
    WHERE dict.encode_type_code = 'B_ZHRMGHGXZQH';"

    params <- list(year = year)
    sql_begin <- paste0(sql_begin, sql_kl)

    if (lqlx != '全部') {
      sql_begin <- paste(sql_begin, sql_lqlx, sep = ' ')
      params$lqlx <- lqlx
    }
    sql <- paste(sql_begin, sql_end)

    sql <- sqlInterpolate(pool, sql, .dots = params)
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    if (item == '高重差') {
      result <- rename(select(temp, area, score_max), score = score_max)
    } else if (item == '低重差') {
      result <- rename(select(temp, area, score_min), score = score_min)
    } else if (item == '均重差') {
      result <- rename(select(temp, area, score_avg), score = score_avg)
    } else {
      stop('Not correct parameter item.')
    }

    result <- mutate(result, score = score - 0) # need to change after data got

    res_logger(req, res)
    return(result)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param lqnf
#* @param area
#* @get /score/province
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
    result <- suppressWarnings(dbGetQuery(pool, sql))

    res_logger(req, res)
    return(result)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param lqnf
#* @param area
#* @get /score/major
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
    result <- suppressWarnings(dbGetQuery(pool, sql))

    res_logger(req, res)
    return(result)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param area
#* @get /count/summary/trend
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


#* @param lqnf
#* @get /count/summary/top
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


#* @param lqnf
#* @get /count/summary/province
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


#* @get /count/province
function(req, res) {
  tryCatch({

    sql <- "
    SELECT syssdm,
           lqnf AS year,
           COUNT(*) AS n
    FROM ks_lqb
    GROUP BY syssdm,
             lqnf;"
    area_year_n <- dbGetQuery(pool, sql) %>%
      mutate(year = as.integer(year))

    year_max <- max(area_year_n$year)
    year_full <- seq(year_max - 9, year_max, 1)
    area_year_n <- expand.grid(
      unique(area_year_n$syssdm),
      year_full,
      stringsAsFactors = FALSE
    ) %>%
      setNames(c('syssdm', 'year')) %>%
      left_join(area_year_n, by = c('syssdm', 'year')) %>%
      mutate(n = replace_na(n, 0))

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


#* @param lqnf
#* @param kl
#* @param lqlx
#* @get /count/major
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

    sql_kl <- klSplit(kl)
    sql_lqlx <- "AND lqlx = ?lqlx"

    sql_end <- "
    GROUP BY syssdm,
             zymc;"

    params <- list(year = year)
    sql_begin <- paste0(sql_begin, sql_kl)

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


#* @param lqnf
#* @get /major/summary/structure
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
    kl <- klTransfer(kl)

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


#* @param lqnf
#* @get /major/summary/top
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


#* @param area
#* @param kl
#* @param lqlx
#* @param zymc
#* @get /major/summary/trend
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

    sql_kl <- klSplit(kl)
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
    sql_begin <- paste0(sql_begin, sql_kl)

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


#* @param lqnf
#* @param area
#* @get /major/score
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


#* @param lqnf
#* @param kl
#* @param lqlx
#* @get /major/count
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

    sql_kl <- klSplit(kl)
    sql_lqlx <- "AND lqlx = ?lqlx"

    sql_end <- "
    GROUP BY syssdm,
             zymc;"

    params <- list(year = year)
    sql_begin <- paste0(sql_begin, sql_kl)

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


#* @get /school/summary/trend
function(req, res) {
  tryCatch({

    sql <- "
    SELECT lqnf,
           zxmc,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE zxmc is not NULL
    GROUP BY lqnf,
             zxmc;"
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    temp <- yearFill(temp)

    sql <- "
    SELECT kl,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf > ((SELECT MAX(lqnf)
                     FROM ks_lqb) - 6)
    GROUP BY kl;"
    kl <- suppressWarnings(dbGetQuery(pool, sql))
    kl <- klTransfer(kl)

    count <- purrr::map_dfr(1:6, zxStatGet, df = temp) %>%
      mutate(nYear = paste0('连续', row_number(), '年'),
             nYear = case_when(
               row_number() == 1 ~ gsub('连续', '', nYear),
               TRUE ~ nYear
             ),
             prob = NA) # need to change after data got

    res_logger(req, res)
    list(count = count, kl = kl)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param lqnf
#* @get /school/summary/info
#* @serializer unboxedJSON
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))

    sql <- "
    SELECT zxmc,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
      AND zxmc IS NOT NULL
      GROUP BY zxmc
      ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    this_year <- suppressWarnings(dbGetQuery(pool, sql))

    sql <- sqlInterpolate(pool, sql, year = (year - 1))
    last_year <- suppressWarnings(dbGetQuery(pool, sql))

    yoy <- left_join(this_year, last_year, by = 'zxmc') %>%
      mutate(n.y = tidyr::replace_na(n.y, 0),
             n = n.x - n.y)

    res_logger(req, res)
    safeJSON(list(
      n = nrow(this_year),
      peopleAvg = round(mean(this_year$n), 1),
      peopleMed = median(this_year$n),
      nPlus = sum(yoy$n > 0),
      nMinus = sum(yoy$n < 0)
    ))

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param lqnf
#* @get /school/summary/top
function(req, res, lqnf) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))

    sql <- "
    SELECT zxmc,
           COUNT(*) AS n
    FROM ks_lqb
    WHERE lqnf = ?year
      AND zxmc IS NOT NULL
      GROUP BY zxmc
      ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year)
    this_year <- suppressWarnings(dbGetQuery(pool, sql))

    sql <- sqlInterpolate(pool, sql, year = (year - 1))
    last_year <- suppressWarnings(dbGetQuery(pool, sql))

    yoy <- left_join(this_year, last_year, by = 'zxmc') %>%
      mutate(n.y = tidyr::replace_na(n.y, 0),
             n = n.x - n.y)

    yoyMaxTop <- filter(yoy, n > 0) %>%
      arrange(desc(n)) %>%
      select(zxmc, n)

    yoyMinTop <- filter(yoy, n < 0) %>%
      arrange(n) %>%
      select(zxmc, n) %>%
      mutate(n = n * -1)

    res_logger(req, res)
    list(thisYearTop = this_year,
         yoyMaxTop = yoyMaxTop,
         yoyMinTop = yoyMinTop)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param lqnf
#* @param area
#* @get /school/score
function(req, res, lqnf, area) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))

    sql <- "
    SELECT lqb.zxmc,
           lqb.zf
    FROM ks_lqb AS lqb
    LEFT JOIN
      (SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
              encode_item_name AS `area`
       FROM ds_encode_item
       WHERE encode_type_code = 'B_ZHRMGHGXZQH'
         AND SUBSTR(encode_item_code, 3, 6) = '0000') AS dict ON lqb.syssdm = dict.syssdm
    WHERE lqb.zxmc IS NOT NULL
      AND lqb.lqnf = ?year
      AND dict.area = ?area;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    temp <- suppressWarnings(dbGetQuery(pool, sql))

    result <- group_by(temp, zxmc) %>%
      summarise(
        n = n(),
        scoreMax = max(zf),
        scoreMin = min(zf),
        scoreAvg = mean(zf),
        scoreMed = median(zf)
      )

    res_logger(req, res)
    return(result)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param lqnf
#* @param area
#* @get /school/count
function(req, res, lqnf, area) {
  tryCatch({
    year <- as.integer(lqnf)
    stopifnot(!is.na(year))

    sql <- "
    SELECT lqb.lqnf,
           lqb.zxmc,
           COUNT(*) AS n
    FROM ks_lqb AS lqb
    LEFT JOIN
      (SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
              encode_item_name AS `area`
       FROM ds_encode_item
       WHERE encode_type_code = 'B_ZHRMGHGXZQH'
         AND SUBSTR(encode_item_code, 3, 6) = '0000') AS dict ON lqb.syssdm = dict.syssdm
    WHERE lqb.zxmc IS NOT NULL
      AND lqb.lqnf > (?year - 5)
      AND dict.area = ?area
    GROUP BY lqb.lqnf, lqb.zxmc
    ORDER BY n DESC;"
    sql <- sqlInterpolate(pool, sql, year = year, area = area)
    temp <- suppressWarnings(dbGetQuery(pool, sql))

    zxmc_all <- unique(temp$zxmc)

    temp <- yearFill(temp)
    result <- spread(temp, lqnf, n, fill = 0) %>%
      filter(zxmc %in% zxmc_all)

    res_logger(req, res)
    return(result)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}
