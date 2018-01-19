
library(httr)
library(dplyr)
library(jsonlite)
library(stringr)

weiboTimeConvert <- function(df) {
  library(stringr)
  library(dplyr)
  library(lubridate)
  
  mutate(df, created_at = case_when(
    str_detect(created_at, '刚刚') ~ as.character(now()), 
    str_detect(created_at, '分钟前') ~ 
      as.character(
        now() - minutes(as.numeric(str_extract(created_at, '[:digit:]*')))
      ), 
    str_detect(created_at, '小时前') ~ 
      as.character(
        now() - hours(as.numeric(str_extract(created_at, '[:digit:]*')))
      ), 
    str_detect(created_at, '昨天') ~ paste0(
      str_replace(created_at, '昨天', as.character(today() - 1)), 
      ':00'
    ), 
    nchar(created_at) == 5 ~ paste0(
      year(now()), '-', created_at, ' 00:00:00' 
    ), 
    TRUE ~ paste0(created_at, ' 00:00:00')
  ))
}

weiboMetaDataGet <- function(x) {
  library(stringr)
  library(dplyr)
  library(lubridate)
  library(luzlogr)
  
  tryCatch({
    data_frame(
      id_weibo = x$mblog$id, 
      text = str_replace_all(x$mblog$text, '<.*?>', ''), 
      source = x$mblog$source, 
      reposts_count = x$mblog$reposts_count, 
      comments_count = x$mblog$comments_count, 
      attitudes_count = x$mblog$attitudes_count, 
      created_at = x$mblog$created_at, 
      user_id = as.character(x$mblog$user$id), 
      screen_name = x$mblog$user$screen_name, 
      # profile = x$mblog$user$profile_url, 
      followers_count = x$mblog$user$followers_count, 
      follow_count = x$mblog$user$follow_count, 
      gender = x$mblog$user$gender, 
      time_stamp = as.character(Sys.time())
    )
  }, error = function(e) {
    printlog(sprintf('error: %s', e$message))
    print(e$message)
    print('=============== start ===================')
    print(str(x, max.level = 1))
    print(str(x$mblog, max.level = 1))
    print(str(x$mblog$id, max.level = 1))
    print('================ end ====================')
    return(data_frame())
  })
}

weiboPageGet <- function(queryVal, page, 
                         time_sleep = 3, time_out = 30, 
                         cookie = readLines('cookie'), 
                         u_agent = readLines('user-agent')) {
  library(httr)
  library(dplyr)
  library(luzlogr)
  library(R.utils)
  library(methods)
  
  # openlog('e:/test.log', append = TRUE, sink = TRUE)
  # on.exit(closelog(sessionInfo = FALSE))
  
  tryCatch({
    
    withTimeout(
      temp <- GET(
        url = 'https://m.weibo.cn/api/container/getIndex',
        query = list(
          type = 'all',
          queryVal = queryVal,
          featurecode = 20000320, 
          luicode = 10000011,
          lfid = '106003type=1', 
          title = queryVal,
          containerid = paste0('100103type=1&q=', queryVal), 
          page = page
        ),  
        config = config(
          cookie = cookie
        ),
        add_headers(
          'user-agent' = u_agent
        )
      ), 
      timeout = time_out
    )
    
    temp <- content(temp)$data$cards
    Sys.sleep(time_sleep)
    
    if (length(temp) == 0) {
      return(NULL)
    }
    
    bind_rows(
      lapply(temp[[length(temp)]][['card_group']], weiboMetaDataGet)
    )
    
  }, error = function(e) {
    print(e$message)
    printlog(sprintf('page: %s, error: %s', page, e$message))
    return(data_frame())
  })
  
}

pageChange <- function(idx_this, idx, page) {
  
  if (!is.null(idx)) {
    if (all(idx %in% idx_this)) {
      return(1)
    } else {
      return(page + 1)
    }
  } else {
    return(page + 1)
  }
  
}

weiboTopicGet <- function(db_path, db_table, queryVal, 
                          page_start = 1, time_sleep = 3, time_out = 30, 
                          log_path = '/home/slj_temp/log/weiboTopicGet.log', 
                          cookie_path = '/home/slj_temp/weibo/cookie', 
                          user_agent_path = '/home/slj_temp/weibo/user-agent') {
  library(dplyr)
  library(RSQLite)
  library(luzlogr)
  
  log_path_dir <- str_replace(log_path, paste0('/', basename(log_path)), '')
  if (!dir.exists(log_path_dir)) {
    dir.create(log_path_dir)
  }
  openlog(log_path, append = TRUE, sink = FALSE)
  on.exit(closelog(sessionInfo = FALSE), add = TRUE)
  
  tryCatch({
    
    con <- dbConnect(RSQLite::SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    idx_this <- dbGetQuery(
      con, sprintf('select distinct id_weibo from %s;', db_table)
    )$id_weibo
    cookie <- readLines(cookie_path)
    u_agent <- readLines(user_agent_path)
    page <- page_start
    null_or_dup <- 0
    
    while (TRUE) {
      weibo <- weiboPageGet(queryVal, page, time_sleep, time_out, cookie, u_agent)
      
      if (is.null(weibo) | all(weibo$id_weibo %in% idx_this)) {
        printlog(sprintf('page: %s, null or dupliated page', page))
        null_or_dup <- null_or_dup + 1
        page <- page + 1
        if (null_or_dup > 20) {
          printlog('reach max null or duplicated page limit, maybe there is no more weibo')
          page <- 1
          purrr::walk(seq_len(10), function(x) {
            Sys.sleep(60)
            printlog(sprintf('%s min left to crawl from page 1 again', 
                             as.character(10 - x)))
          })
          next # stop is not expected, wait for a moment and crawl again
        }
        next # null page can't be parsed and inserted, and duplicated page no need
      }
      null_or_dup <- 0
      
      weibo <- mutate(weibo, page = page)
      weibo <- filter(weibo, !id_weibo %in% idx_this)
      weibo <- weiboTimeConvert(weibo)
      idx_this <- unique(c(idx_this, weibo$id_weibo))
      
      dbWriteTable(con, db_table, weibo, append = TRUE)
      printlog(sprintf('P: %s, N: %s, ts: %s, %s', 
                       page, 
                       length(idx_this), 
                       str_replace(weibo$created_at[1], '00:00:00', ''), 
                       str_sub(weibo$text[1], end = 10)))
      # page <- pageChange(idx_this, weibo$id, page)
      page <- page + 1
      
    }
    
  }, error = function(e) {
    printlog(sprintf('error: %s', e$message))
  })
}
