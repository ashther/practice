#!/usr/bin/Rscript

library(XML)
library(magrittr)
library(RCurl)
library(RPushbullet)

url_jd <- 'https://vip.jd.com/medal/coupon-60-1.html'
title <- '京东自营'
result <- NULL

tryCatch({
  content <- url_jd %>% 
    getURL() %>% 
    iconv(from = 'utf-8', to = 'gbk') %>% 
    htmlParse(asText = TRUE)
  
  ids <- xpathSApply(content, '//div[@class="shop-name ellipsis"]', xmlValue) %>% 
    grepl(title, .) & !(
      xpathSApply(content, '//div[@class="coupon-list"]/div', xmlGetAttr, 'class') %>% 
        grepl('status-empty', .)
    )
  
  if (any(ids)) {
    result <- xpathSApply(content, '//div[@class="name ellipsis"]',xmlValue) %>%
      extract(ids) %>%
      gsub('\\s', '', ., fixed = FALSE) %>%
      paste0(collapse = '\n')
  }
}, error = function(e){
  result <- e
})

if (!is.null(result)) {
  pbPost(type = 'link', title = title , body = result, url = url_jd,
         apikey = 'o.fW2venSCqeIjYIxbSkeQfgLJ8NjEyn19')
}

load(file = '/home/ashther/jd_coupon/new_day_check.rda')
if (Sys.Date() > new_day_check) {
  new_day_check <- Sys.Date()
  save(new_day_check, file = '/home/ashther/jd_coupon/new_day_check.rda')
  pbPost(type = 'note', title = 'hi morning' , body = 'what a wonderful day it will be', 
         apikey = 'o.fW2venSCqeIjYIxbSkeQfgLJ8NjEyn19')
}

