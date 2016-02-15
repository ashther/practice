
library(RCurl)
library(rvest)
library(RSelenium)

# my_http_header <- c(
#     'User-Agent' = 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.154 Safari/537.36 LBBROWSER', 
#     'Accept-Language' = 'zh-CN,zh;q=0.8', 
#     'Connection' = 'keep-alive'
# )
# 
# d <- debugGatherer()
# 
# chandle <- getCurlHandle(httpheader = my_http_header, followlocation = TRUE, 
#                          debugfunction = d$update, verbose = TRUE, 
#                          ssl.cipher.list = 'RC4-SHA', 
#                          cookiefile = 'cookie.txt')

# try RSelenium
umeng_url <- 'http://www.umeng.com/apps/e282000ec3e85e7641e20365/reports/active_user'
RSelenium::startServer()

pjs <- phantom()
Sys.sleep(5)
ecap <- list(phantomjs.page.settings.userAgent = 
                 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.154 Safari/537.36 LBBROWSER')
remDr <- remoteDriver(browserName = 'phantomjs', extraCapabilities = ecap)
remDr$open()

remDr$navigate(umeng_url)

test_login <- remDr$findElement('id', 'userId')
test_login$sendKeysToElement(list('xayzxxkj@126.com'))
test_pass <- remDr$findElement('id', 'passWord')
test_pass$sendKeysToElement(list('abc123'))
test_cap <- remDr$findElement('id', 'captcha')

remDr$screenshot(display = TRUE) # to see what captcha is
test_cap$sendKeysToElement(list('XQWE')) # need to change captcha

button <- remDr$findElement('id', 'login-submit')
button$clickElement()
remDr$screenshot(display = TRUE) # to confirm login

remDr$navigate(umeng_url)
content <- remDr$getPageSource()[[1]]

remDr$close
pjs$stop()
remDr$closeServer()

test <- content %>% 
    read_html() %>% 
    html_nodes('td.active_user_summary') %>% 
    html_text() %>% 
    iconv(from = 'utf-8', to = 'gbk')
