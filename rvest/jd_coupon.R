#!/usr/bin/Rscript

library(XML)
library(magrittr)
library(RCurl)
library(RPushbullet)

url_jd <- 'https://vip.jd.com/medal/coupon-60-1.html'
title <- '中南博集天卷'
result <- NULL

tryCatch({
  content <- url_jd %>% 
    getURL() %>% 
    htmlParse(encoding = 'utf-8', asText = TRUE)
  
  ids <- xpathSApply(content, '//div[@class="shop-name ellipsis"]', xmlValue) %>% 
    grepl(title, .) & !(
      xpathSApply(content, '//div[@class="coupon-list"]/div', xmlGetAttr, 'class') %>% 
        grepl('status-empty', .)
    )
  
  if (length(ids) > 0) {
    result <- xpathSApply(content, '//div[@class="name ellipsis"]',xmlValue) %>% 
      extract(ids) %>% 
      gsub('\\s', '', ., fixed = FALSE) %>% 
      paste0(collapse = '\n')
  }
}, error = function(e){
  result <- e$message
})

if (!is.null(result)) {
  pbPost(type = 'link', title = title , body = result, url = url_jd,  
         apikey = '')
}

