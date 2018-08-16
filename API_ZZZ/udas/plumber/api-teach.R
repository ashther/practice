
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

#* 教学资源 资源投入
#* @param dwmc
#* @get /resource/college
#* @serializer unboxedJSON
function(req, res, dwmc) {
  tryCatch({
    
    sql_begin <- "
    SELECT SUBSTR(lxrq, 1, 4) AS lxrq,
          gwlb,
          COUNT(*) as n
    FROM jzg_jbsjb
    WHERE zg IN ('在岗',
                '编外在岗', 
                '聘用制', 
                '外聘高级专家', 
                '专职辅导员')
     AND gwlb IN ('教师岗位',
                  '教室岗位、管理岗位',
                  '其他专业技术岗位', 
                  '专职辅导员')"
    
    sql_dwmc <- "AND dwmc = ?dwmc"
    
    sql_end <- "
    GROUP BY SUBSTR(lxrq, 1, 4),
            gwlb;"
    
    if (dwmc != '全部') {
      sql <- paste(sql_begin, sql_dwmc, sql_end)
      sql <- sqlInterpolate(pool, sql, dwmc = dwmc)
    } else {
      sql <- paste(sql_begin, sql_end)
      sql <- sqlInterpolate(pool, sql)
    }
    
    temp <- dbGetQuery(pool, sql)
    temp <- filter(temp, grepl('[0-9]{4}', lxrq)) %>% 
      spread(gwlb, n, fill = 0)
    
    result <- yearFill(temp)
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}
