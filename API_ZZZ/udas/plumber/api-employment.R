
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


#* @param byrq
#* @param kl
#* @param lqlx
#* @param yxsmc
#* @get /info
#* @serializer unboxedJSON
function(req, res, byrq, kl, lqlx, yxsmc) {
  tryCatch({
    
    year <- as.integer(byrq)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT COUNT(*) AS n,
           SUM(CASE WHEN jyqx.byqxm NOT IN (30, 40, 50, 99) THEN 1 ELSE 0 END) AS n_emp,
           SUM(CASE WHEN jyqx.byqxm IN (30, 40) THEN 1 ELSE 0 END) AS n_grd_abroad
    FROM bks_jyqxb AS jyqx
    LEFT JOIN bks_xjb AS xjb ON jyqx.xh = xjb.xh
    LEFT JOIN ks_lqb AS lqb ON xjb.ksh = xjb.ksh
    WHERE SUBSTR(jyqx.byrq, 1, 4) = ?year"
    sql_kl <- klSplit(kl)
    sql_lqlx <- " and lqb.lqlx = ?lqlx "
    sql_yxsmc <- " and xjb.yxsmc = ?yxsmc "
    
    params <- list(year = year)
    sql <- paste(sql, sql_kl)
    
    if (lqlx != '全部') {
      sql <- paste(sql, sql_lqlx)
      params$lqlx <- lqlx
    }
    if (yxsmc != '全部') {
      sql <- paste(sql, sql_yxsmc)
      params$yxsmc <- yxsmc
    }
    
    sql <- sqlInterpolate(pool, sql, .dots = params)
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    
    res_logger(req, res)
    safeJSON(list(
      n          = temp$n, 
      nEmp       = temp$n_emp, 
      probEmp    = round(temp$n_emp/temp$n, 4), 
      nGrdAbroad = temp$n_grd_abroad
    ))
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param byrq
#* @get /enterprise
#* @serializer unboxedJSON
function(req, res, byrq) {
  tryCatch({
    
    year <- as.integer(byrq)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT jygzdw
    FROM bks_jyqxb
    WHERE SUBSTR(byrq, 1, 4) = ?year
      AND byqxm NOT IN (30, 40, 50, 99);"
    sql <- sqlInterpolate(pool, sql, year = year)
    temp <- dbGetQuery(pool, sql)
    
    world_temp <- filter(fortune500, year == year, level == 'world')
    world_500 <- left_join(temp, world_temp, by = c('jygzdw' = 'name')) %>% 
      count(is_in = is.na(rank))
    world_50 <- count(world_temp, !is.na(rank) & rank <= 50)
    
    china_temp <- filter(fortune500, year == year, level == 'China')
    china_500 <- count(china_temp, is.na(rank))
    china_50 <- count(china_temp, !is.na(rank) & rank <= 50)
    
    # to be continued
    
    res_logger(req, res)
    list(world50 = list(yes = NA, no = NA), 
         world500 = list(yes = NA, no = NA), 
         china50 = list(yes = NA, no = NA), 
         china500 = list(yes = NA, no = NA))
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param byrq
#* @get /college
function(req, res, byrq) {
  tryCatch({
    
    year <- as.integer(byrq)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT xjb.yxsmc,
           SUM(CASE WHEN jyqx.byqxm NOT IN (30, 40, 50, 99) THEN 1 ELSE 0 END) AS nEmp,
           SUM(CASE WHEN jyqx.byqxm IN (30, 40, 50, 99) THEN 1 ELSE 0 END) AS nOther
    FROM bks_jyqxb AS jyqx
    LEFT JOIN bks_xjb AS xjb ON jyqx.xh = xjb.xh
    WHERE SUBSTR(jyqx.byrq, 1, 4) = ?year
    GROUP BY xjb.yxsmc;"
    sql <- sqlInterpolate(pool, sql, year = year)
    college <- suppressWarnings(dbGetQuery(pool, sql))
    
    sql <- "
    SELECT xjb.xnzymc as zymc,
           COUNT(*) AS n,
           SUM(CASE WHEN jyqx.byqxm NOT IN (30, 40, 50, 99) THEN 1 ELSE 0 END) AS nEmp
    FROM bks_jyqxb AS jyqx
    LEFT JOIN bks_xjb AS xjb ON jyqx.xh = xjb.xh
    WHERE SUBSTR(jyqx.byrq, 1, 4) = ?year
    GROUP BY xjb.xnzymc;"
    sql <- sqlInterpolate(pool, sql, year = year)
    major <- suppressWarnings(dbGetQuery(pool, sql))
    
    major <- mutate(major, prob = if_else(n == 0 | is.na(n), 0, nEmp / n))
    major <- arrange(major, desc(prob))
    
    res_logger(req, res)
    list(college = college, 
         major = major)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param byrq
#* @get /province
function(req, res, byrq) {
  tryCatch({
    
    year <- as.integer(byrq)
    stopifnot(!is.na(year))
    
    sql <- "
    SELECT syssdm,
           COUNT(*) AS n,
           SUM(CASE WHEN jyqx.byqxm NOT IN (30, 40, 50, 99) THEN 1 ELSE 0 END) AS n_emp
    FROM bks_jyqxb AS jyqx
    LEFT JOIN bks_xjb AS xjb ON jyqx.xh = xjb.xh
    LEFT JOIN ks_lqb AS lqb ON xjb.ksh = xjb.ksh
    WHERE SUBSTR(jyqx.byrq, 1, 4) = ?year
    GROUP BY lqb.syssdm;"
    sql <- sqlInterpolate(pool, sql, year = year)
    temp <- suppressWarnings(dbGetQuery(pool, sql))
    
    sql <- "
    SELECT SUBSTR(encode_item_code, 1, 2) AS syssdm,
           encode_item_name AS `area`
    FROM ds_encode_item
    WHERE encode_type_code = 'B_ZHRMGHGXZQH'
      AND SUBSTR(encode_item_code, 3, 6) = '0000';"
    province <- dbGetQuery(pool, sql)
    
    result <- left_join(temp, province, by = 'syssdm') %>% 
      mutate(prob = if_else(n == 0 | is.na(n), 0, n_emp / n)) %>% 
      select(area, prob)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}
