
library(RCurl)
my_http_header <- c(
    'User-Agent' = 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.154 Safari/537.36 LBBROWSER', 
    'Accept-Language' = 'zh-CN,zh;q=0.8', 
    'Connection' = 'keep-alive'
)

d <- debugGatherer()

chandle <- getCurlHandle(httpheader = my_http_header, followlocation = TRUE, 
                         debugfunction = d$update, verbose = TRUE, 
                         ssl.cipher.list = 'RC4-SHA', 
                         cookiefile = 'cookie.txt')

# ssl.cipher.list = 'RC4-SHA'

test <- getURL('http://www.umeng.com/apps/e282000ec3e85e7641e20365/reports/active_user', 
               curl = chandle) %>% 
    read_html()


