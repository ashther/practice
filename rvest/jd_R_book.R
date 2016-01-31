
library(rvest)
library(magrittr)
library(stringr)

result <- data.frame()
pb <- txtProgressBar(max = 10, style = 3)
for (i in 1:10) {
    url <- paste0('http://search.jd.com/Search?keyword=R&book=y', 
                  '#keyword=R&enc=utf-8&qrst=1&rt=1&stop=1&book=y&vt=2&page=', 
                  i, 
                  '&click=1')
    
    tryCatch({
        print(url)
        books <- url %>% 
            read_html() %>% 
            html_nodes('li.gl-item') %>%
            iconv(from = 'utf-8', to = 'gbk', sub = '') %>% 
            gsub('\t|\n', '', .)
        
        price <- str_match(books, 'data-price=\"(.*?)"')[, 2]
        name <- str_match(books, 'p-name.*?<em>(.*?)</em>')[, 2]
        m_price <- str_match(books, '<del>￥(.*?)</del>')[, 2]
        publisher <- str_match(books, 'p-bi-store.*?title=\"(.*?)\"')[, 2]
        shop <- str_match(books, 'curr-shop\".*?>(.*?)<')[, 2]
        author <- str_match(books, 'p-bi-name.*?title=\"(.*?)\"')[, 2]
        date <- str_match(books, 'p-bi-date\">(.*?)</span>')[, 2]
        href <- str_match(books, 'class=\"gl-i-wrap.*?href=\"//(.*?)\"')[, 2]
    }, 
    error = function(e){
        price <- e
        name <- NA
        m_price <- NA
        publisher <- NA
        shop <- NA
        author <- NA
        date <- NA
        href <- NA
    })
 
    books_data_frame <- data.frame(list(name = name, price = price, m_price = m_price, 
                            discounts = round(as.numeric(price) / as.numeric(m_price), 2), 
                            author = author, publisher = publisher, date = date, 
                            shop = shop, href = paste0('http://', href)), 
                       stringsAsFactors = FALSE)
    
    result <- rbind(result, books_data_frame)
    setTxtProgressBar(pb, value = i)
    
    Sys.sleep(1)
    closeAllConnections()
}
close(pb)
write.csv(result, 'jd_R_book.csv')


