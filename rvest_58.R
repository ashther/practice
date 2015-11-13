library(magrittr)
library(rvest)
library(stringr)
url_0 <- 'http://xa.58.com/qztech/pn'
df_all <- data.frame()
start <- 1
end <- 10

urlParse <- function(url, nodes){
    result <- url %>% 
        read_html(encoding = 'utf-8') %>% 
        html_nodes(nodes) %>% 
        html_text() %>% 
        iconv('utf-8', 'gbk', sub = '')
}

detailParse <- function(s, con){
    s_temp <- s %>% 
        gsub('\r|\n|\t', ' ', .) %>% 
        strsplit(' ', fixed = TRUE) %>% 
        unlist
    s_temp <- s_temp[s_temp != '']
    ids <- grep(con, s_temp)
    
    if (!identical(ids, integer(0))) {
        result <- s_temp[ids] %>% 
            str_replace(con, '') %>% 
            iconv('utf-8', 'gbk', sub = '')
    } else {
        result <- NA
    }
    return(result)
}

for (i in start:end) {
    url <- paste(url_0, i, sep = '')
    information <- urlParse(url, 'div.xboxcontent')
    
    salary <- lapply(information, function(s)detailParse(s, '期望月薪：')) %>% unlist
    
    df <- data.frame(salary)
    df_all <- rbind(df_all, df)
}
