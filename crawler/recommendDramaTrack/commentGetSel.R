#!/usr/local/bin/Rscript

loginSelenium <- function(remDr) {
  remDr$navigate('https://accounts.douban.com/login')
  # remDr$screenshot(TRUE)
  # captcha <- readline('captcha is: ')
  remDr$findElement('css selector', 'input#email')$sendKeysToElement(list(''))
  remDr$findElement('css selector', 'input#password')$sendKeysToElement(list(''))
  # remDr$findElement('css selector', 'input#captcha_field')$sendKeysToElement(list(captcha))
  remDr$findElement('css selector', 'input[type="checkbox"]')$clickElement()
  remDr$findElement('css selector', '.btn-submit')$clickElement()
  return(remDr$getCurrentUrl()[[1]])
} 

suppressPackageStartupMessages({
  library(rvest)
  library(httr)
  library(stringr)
  library(dplyr)
  library(purrr)
  # library(writexl)
  library(luzlogr)
  library(RSelenium)
  library(methods)
})
openlog(here('commentGet.log'), append = TRUE, sink = TRUE)

comments <- tibble()

u_agent <- 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36'
# result_path <- '/home/wj/douban_comment/'
base_url <- 'https://movie.douban.com/subject/'
# celebrity_id <- 1274825
# proxy <- '59.40.51.156'
# port <- 8010
args <- commandArgs(TRUE)
subject_id <- args[1]
proxy <- as.character(args[2])
port <- as.integer(args[3])
printlog(sprintf('subject is: %s', subject_id))
printlog(sprintf('proxy is: %s', proxy))
printlog(sprintf('port is: %s', port))

pjs <- phantom('/home/slj_temp/bianju/phantomjs-2.1.1-linux-x86_64/bin/phantomjs', port = 5555)
ecap <- list(phantomjs.page.settings.userAgent = u_agent, 
             phantomjs.binary.path = '/home/slj_temp/bianju/phantomjs-2.1.1-linux-x86_64/bin/phantomjs')
remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = ecap, port = 5555)
invisible(remDr$open())
loginSelenium(remDr)

nxt_page <- ''
# i <- 1
# nxt_page <- sprintf('?start=%s&limit=20&sort=new_score&status=P&percent_type=', i)
while (TRUE) {
  tryCatch({
    nxt_page <- paste0(base_url, subject_id, '/comments', nxt_page)
    if (is.na(proxy) & is.na(port)) {
      remDr$navigate(nxt_page)
      html_content <- read_html(remDr$getPageSource()[[1]])
      Sys.sleep(5)
    } else {
      html_content <- content(GET(nxt_page,
                                  # add_headers(u_agent),
                                  use_proxy(proxy, port)))
      Sys.sleep(5)
    }
    
    comments_number <- as.integer(unlist(
      str_extract_all(
        html_text(html_node(html_content, 'li.is-active span')), 
        '[:digit:]+'
      )
    ))
    
    temp <- html_nodes(html_content, 'div.comment-item')
    comments_this_page <- map_df(temp, function(x) {
      tryCatch({
        cid <- html_attr(x, 'data-cid')
        user_url <- html_attr(html_node(x, 'div.comment h3 span.comment-info a'), 'href')
        user_name <- html_text(html_node(x, 'div.comment h3 span.comment-info a'))
        star <- html_attr(html_node(x, 'div.comment h3 span.comment-info span[class$="rating"]'), 'title')
        ts <- html_attr(html_node(x, 'div.comment h3 span.comment-info span.comment-time'), 'title')
        comment_text <- str_trim(html_text(html_node(x, 'div.comment p')))
        vote <- html_text(html_node(x, 'div.comment h3 span.comment-vote span.votes'))
        
        tibble(cid = cid, user_url = user_url, user_name = user_name, star = star,
               comment_text = comment_text, vote = vote, ts = ts)
        
      }, error = function(e) {
        printlog(e$message)
        return(tibble())
      })
    })
    
    comments <- bind_rows(comments, comments_this_page) %>% 
      filter(!duplicated(cid))
    printlog(sprintf('comments reading... %s/%s', nrow(comments), comments_number))
    
  }, error = function(e) {
    printlog(sprintf('page: %s, error: %s', nxt_page, e$message))
  })
  
  nxt_page <- tryCatch(
    html_attr(html_node(html_content, 'div#paginator a.next'), 'href'),
    error = function(e) {
      printlog(sprintf('page: %s, error: %s', nxt_page, e$message))
      return(NA)
    }
  )
  if (is.na(nxt_page)) {
    break
  }
  # i <- i + 20
  # if (i > as.integer(comments_number)) {
  #   break
  # }
  # nxt_page <- sprintf('?start=%s&limit=20&sort=new_score&status=P&percent_type=', i)
}

invisible(write_xlsx(comments, paste0(result_path, subject_id, '.xlsx')))
printlog(sprintf('%s comments have been wrote to file', nrow(comments)))

closelog(sessionInfo = FALSE)
