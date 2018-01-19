
newsParse <- function(html_content, url_id) {
  
  title <- html_text(html_node(html_content, '.title'))
  ts <- html_text(html_node(html_content, '.time'))
  amount <- html_text(html_node(html_content, '.amount'))
  source <- html_text(html_node(html_content, '.source'))
  zan <- html_text(html_node(html_content, '.article-zan span'))
  content <- html_text(html_node(html_content, '.article'))
  
  data_frame(title = title, 
             ts = ts, 
             amount = amount, 
             source = source, 
             zan = zan, 
             content = content, 
             url_id = url_id)
}

newsGet <- function(news, base_url, url_id_start, timeout, log_path, 
                    url_id_limit = NA) {
  suppressPackageStartupMessages({
    library(luzlogr)
    library(RSQLite)
    library(R.utils)
    library(methods)
    library(rvest)
    library(dplyr)
    library(stringr)
  })
  
  # openlog(file.path(log_path, 'newsGet.log'), append = TRUE)
  on.exit(closelog(sessionInfo = FALSE))
  url_id <- as.integer(url_id_start)
  
  tryCatch({
    
    con <- dbConnect(RSQLite::SQLite(), news)
    on.exit(dbDisconnect(con))
    
    while (TRUE) {
      tryCatch({
        if (!is.na(url_id_limit) & url_id > url_id_limit) {
          printlog(sprintf('url_id reach limit: %s', url_id_limit))
          break
        }
        
        url <- paste0(base_url, url_id)
        withTimeout(html_content <- read_html(url), timeout = timeout)
        Sys.sleep(abs(rnorm(1, 0.5, 0.5)))
        
        content_test <- str_trim(html_text(html_node(html_content, 'body')))
        if (content_test == '该新闻已下线') {
          printlog(sprintf('news offline, url_id: %s', url_id))
          url_id <- as.integer(url_id + 1)
          next
        } else if (content_test == '没有该新闻') {
          printlog(sprintf('no more news, url_id: %s', url_id))
          url_id <- as.integer(url_id + 1)
          next # comment this after url_id bigger than 8e5
          # break # we should uncomment this after url_id bigger than 8e5
        }
        temp <- newsParse(html_content, url_id)
        printlog(sprintf('page %s got, ts: %s %s', url_id, temp$ts[1], temp$title[1]))
        
        if (!is.na(temp$ts[1]) & 
            difftime(Sys.time(), strptime(temp$ts[1], '%Y-%m-%d %H:%M'), units = 'hours') < 12) {
          printlog(sprintf('reach the 6-hours-gap limit'))
          break
        }
        
        dbWriteTable(con, 'news', temp, append = TRUE)
        url_id <- as.integer(url_id + 1)
        
      }, error = function(e) {
        printlog(sprintf('url_id: %s, error: %s', url_id, e$message))
        dbWriteTable(con, 'news', data_frame(
          content = e$message, 
          url_id = as.integer(url_id - 1)
        ), append = TRUE)
        url_id <<- as.integer(url_id + 1)
      })
    }
    printlog(sprintf('url_id ends at: %s', url_id))
    
  }, error = function(e) {
    printlog(sprintf('url_id: %s, error: %s', url_id, e$message))
  })
}

newsCron <- function(news, timeout = 30, log_path = '/home/slj_temp/log', 
                     url_id_limit = NA) {
  suppressPackageStartupMessages({
    library(luzlogr)
    library(RSQLite)
    library(methods)
  })
  
  if (!dir.exists(log_path)) {
    dir.create(log_path)
  }
  openlog(file.path(log_path, 'newsGet.log'), append = TRUE)
  on.exit(closelog(sessionInfo = FALSE))
  
  base_url <- 'http://app.peopleapp.com/Api/600/DetailApi/shareArticle?type=0&article_id='
  
  tryCatch({
    
    con <- dbConnect(RSQLite::SQLite(), news)
    on.exit(dbDisconnect(con))
    url_id_max <- dbGetQuery(con, 'select max(url_id) as n from news')$n
    
    if (is.na(url_id_max)) {
      url_id_start <- 1
    } else {
      url_id_start <- as.integer(url_id_max) + 1
    }
    printlog(sprintf('url_id_start: %s', url_id_start))
    
    newsGet(news, base_url, url_id_start, timeout, log_path, url_id_limit)
    
  }, error = function(e) {
    printlog(e$message)
  })
  
}
