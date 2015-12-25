library(magrittr)
library(rvest)
library(R.utils)

# 测试数据
urls <- paste('http://baike.baidu.com/item/', 
              c('北京大学', '复旦大学', '南京大学'), sep = '')
nodes <- c("//div[contains(@class, 'lemma-summary')]",
           "//h3[contains(@class, 'para-title')]//following-sibling::div[1]",
           "//h3[contains(@class, 'para-title')]")
cons <- c('师资力量', '学校排名', '院系设置', '学校地址')

# 处理单网页、多标签、多检索字
# 返回为与检索字相关的字符串向量
urlParse <- function(url, nodes, cons){
    result <- vector(length = 1 + length(cons))
    
    content <- read_html(url, encoding = 'utf-8')
    temp <- sapply(nodes, function(node){
        content %>% 
            html_nodes(xpath = node) %>% 
            html_text(trim = TRUE) %>% 
            iconv('utf-8', 'gbk', sub = '')
    })
    
    result[1] <- temp[[1]]
    ids <- sapply(cons, function(con){
        grep(con, temp[[3]], fixed = TRUE)
    }) %>% unlist
    result[2:length(result)] <- temp[[2]][ids[cons]]
    
    return(result)
}

# 处理多网页
# 返回为数据框，行名为网页地址
main <- function(urls, nodes, cons, time_limit){
    result <- matrix(NA, nrow = length(urls), ncol = 1 + length(cons), 
                     dimnames = list(urls, c('intro', cons))) %>% 
        as.data.frame()
    
    pb <- txtProgressBar(max = length(urls), style = 3)
    
    for (url in urls) {
        tryCatch({
            evalWithTimeout(try(result[url, ] <- urlParse(url, nodes, cons), 
                                silent = TRUE), timeout = time_limit)
        }, error = function(e){
            result[url, ] <- e
        })
        
        setTxtProgressBar(pb, which(urls == url))
        gc()
        closeAllConnections()
    }
    
    close(pb)
    return(result)
}

university_data <- main(urls, nodes, cons, time_limit = 2)
write.csv(university_data, 'university_data.csv')













