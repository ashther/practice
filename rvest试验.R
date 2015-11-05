library(magrittr)
library(rvest)
library(stringr)

url <- 'http://search.51job.com/jobsearch/search_result.php?fromJs=1&jobarea=000000%2C00&district=000000&funtype=0000&industrytype=00&issuedate=9&providesalary=99&keyword=%E6%95%B0%E6%8D%AE%E6%8C%96%E6%8E%98&keywordtype=1&curr_page=1&lang=c&stype=2&postchannel=0000&workyear=99&cotype=99&degreefrom=99&jobterm=01&companysize=99&lonlat=0%2C0&radius=-1&ord_field=0&list_type=0&fromType=14&dibiaoid=-1'

pageParse <- function(page_to_parse, nodes){
    return(page_to_parse %>% 
               html_nodes(nodes) %>% 
               html_text() %>% 
               iconv('utf-8', 'gbk', sub = '')) # sub = ''去除非ASCII码
}

page_to_parse <- url %>% read_html(encoding = 'gbk')

title <- pageParse(page_to_parse, 'td.td1 a')
company <- pageParse(page_to_parse, 'td.td2 a')
location <- pageParse(page_to_parse, 'td.td3 span')
time <- pageParse(page_to_parse, 'td.td4 span')
introduction <- pageParse(page_to_parse, 'td.td1234')[seq(2, 100, 2)] %>% 
    lapply(function(x){
        gsub('\r|\n|\t', '', x)
    }) %>% 
    unlist()
detail <- pageParse(page_to_parse, 'tr.tr1 td.td1234') %>% 
    lapply(function(x){
        gsub('\t|\r|\n', '', x)
    }) %>% 
    unlist() %>% 
    lapply(function(x){
        strsplit(x, '|', fixed = TRUE) # fixed = TRUE用于非正则表达式模式
    }) %>% 
    unlist()
degree <- detail[seq(1, 197, 4)] %>% 
    lapply(function(x){
        str_replace(x, '学历要求：', '') %>% #替换中文式普通的gsub无效，只好用stringr里的方法
            iconv('utf-8', 'gbk', sub = '') #这次替换将避免打印时出现乱码
    }) %>% 
    unlist()
experience <- detail[seq(2, 198, 4)] %>% 
    lapply(function(x){
        str_replace(x, '工作经验：', '') %>% 
            iconv('utf-8', 'gbk', sub = '')
    }) %>% 
    unlist()
com_type <- detail[seq(3, 199, 4)] %>% 
    lapply(function(x){
        str_replace(x, '公司性质：', '') %>% 
            iconv('utf-8', 'gbk', sub = '')
    }) %>% 
    unlist()
com_size <- detail[seq(4, 200, 4)] %>% 
    lapply(function(x){
        str_replace(x, '公司规模', '') %>% 
            iconv('utf-8', 'gbk', sub = '')
    }) %>% 
    unlist()

df <- data.frame(title, company, location, time, degree, 
                 experience, com_type, com_size, introduction, 
                 stringsAsFactors = FALSE)
write.csv(df, '51job.csv')
