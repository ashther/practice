library(httr)
library(httpuv)
library(rvest)
library(stringr)
library(jiebaR)
library(luzlogr)
library(R.utils)

log_path <- file.path('/home/slj_temp', 'log')
base_url <- 'https://movie.douban.com/chart'
used_top_movie_path <- '/home/slj_temp/fanfou/used_top_movie.rds'

if (!dir.exists(log_path)) {
  dir.create(log_path)
}
openlog(file.path(log_path, 'fanfou.log'), append = TRUE)

tryCatch({
  top_movie <- withTimeout(read_html(base_url), timeout = 60)
  top_movie <- top_movie %>% 
    html_nodes('#listCont2 li div.name > a') %>% 
    html_attr('href')
  
  if (!file.exists(used_top_movie_path)) {
    url <- sample(top_movie, 1)
    saveRDS(url, used_top_movie_path)
  } else {
    used_top_movie <- readRDS(used_top_movie_path)
    url <- sample(setdiff(top_movie, used_top_movie), 1)
    saveRDS(union(used_top_movie, url), used_top_movie_path)
  }
  printlog(url)
  
  push_movie <- read_html(url)
  title <- push_movie %>% 
    html_node('span[property="v:itemreviewed"]') %>% 
    html_text() %>% 
    paste0('《', ., '》')
  printlog(title)
  
  director <- push_movie %>% 
    html_nodes('div#info span:nth-child(1) span') %>% 
    html_text()
  director <- paste0(director[1], ':', 
                     str_trim(unlist(str_split(director[2], '/'))[1]))
  printlog(director)
  
  actor <- push_movie %>% 
    html_nodes('div#info span.actor span') %>% 
    html_text()
  actor <- paste0(
    actor[1], ':', 
    paste0(str_trim(unlist(str_split(actor[2], '/'))[1:2]), collapse = ','))
  printlog(actor)
  
  genre <- push_movie %>% 
    html_nodes('div#info span[property="v:genre"]') %>% 
    html_text() %>% 
    paste0(collapse = '/')
  printlog(genre)
  
  key <- worker('keywords', topn = 20)
  tag <- worker('tag')
  key_words <- push_movie %>% 
    html_nodes('span[property="v:summary"]') %>% 
    html_text() %>% 
    keywords(key) %>% 
    tagging(tag) %>% 
    `[`(!names(.) %in% c('nr', 'nrt', 'nr1', 'nr2', 'nrj', 'nrf')) %>% 
    `[`(1:ifelse(length(.) >= 5, 5, length(.))) %>% 
    paste0(collapse = ',') %>% 
    paste0('关键字：', .)
  printlog(key_words)
  
  img <- read_html(paste0(url, 'photos?type=R'))
  img <- html_node(img, 'ul.poster-col3.clearfix li:nth-child(1) div.cover a img') %>% 
    html_attr('src') %>% 
    str_replace('\\.jpg$', '.webp')
  
  img_webp_file <- tempfile(tmpdir = '/home/slj_temp/fanfou/', fileext = '.webp')
  img_jpeg_file <- tempfile(tmpdir = '/home/slj_temp/fanfou/', fileext = '.jpeg')
  download.file(img, img_webp_file)
  webp::read_webp(img_webp_file) %>% 
    jpeg::writeJPEG(img_jpeg_file)
  file.remove(img_webp_file)
  
  fanfou_endpoint <- oauth_endpoint(
    request = 'http://fanfou.com/oauth/request_token', 
    authorize = 'http://fanfou.com/oauth/authorize', 
    access = 'http://fanfou.com/oauth/access_token'
  )
  
  fanfou_app <- oauth_app(
    appname = 'fanfou', 
    key = '', 
    secret = ''
  )
  
  fanfou_token <- oauth1.0_token(fanfou_endpoint, fanfou_app, 
                                 cache = '/home/slj_temp/fanfou/.httr-oauth')
  
  # POST('http://api.fanfou.com/statuses/update.json', 
  #      body = list(status = 'test another text'), 
  #      config = config(token = fanfou_token))
  
  POST('http://api.fanfou.com/photos/upload.json', 
       body = list(photo = upload_file(img_jpeg_file), 
                   status = paste(title, director, actor, genre, key_words, url, 
                                  sep = ' ')), 
       config = config(token = fanfou_token), 
       encode = 'multipart')
  file.remove(img_jpeg_file)
  closelog(sessionInfo = FALSE)
}, error = function(e) {
  printlog(e$message)
  closelog(sessionInfo = FALSE)
})
