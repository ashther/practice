library(magrittr)
library(rvest)
library(stringr)

# nodes为字符串向量，各元素为所需字段的标签特征（路径）
urlParse <- function(url, nodes){
    # 读入网页内容，暂时保存在page_content
    page_content <- url %>% read_html(encoding = 'utf-8')
    
    # 对字符串向量nodes的每个元素应用函数，函数的定义如下所示，函数按照每个元素
    # 指出的路径提取文本信息，最终所有路径文本以list形式保存在result中
    result <- lapply(nodes, function(node){
        page_content %>% 
            html_nodes(node) %>% 
            html_text() %>% 
            iconv('utf-8', 'gbk', sub = '')
    })
}

# s是将要被解析的字符串，包含了大量“\r\t\n”符号
# con是用于检索的关键字 比如“期望月薪：”
detailParse <- function(s, con){
    s_temp <- s %>% 
        gsub('\r|\n|\t', ' ', .) %>%  # 首先将s中所有的“\r\n\t”替换成空格
        strsplit(' ', fixed = TRUE) %>%  # 利用空格分割上一步得到的字符串，结果是一个含有大量空值的list
        unlist # unlist之后得到一个含有大量空值的字符串向量，赋给s_temp
    
    s_temp <- s_temp[s_temp != ''] # 利用s_temp != ''创建了逻辑向量，并作为索引用于提取s_temp中的非空值
    result <- character(length(con))
    for (i in 1:length(con)) {
        ids <- grep(con[i], s_temp) # 根据关键字con在s_temp中做检索，得到包含该con的索引，这个地方奇怪的是如果设置fixed=TRUE反而会出错
        
        if (!identical(ids, integer(0))) { # 因为当检索不到con时 ids会等于integer(0)，所以ids不等于integer(0)时执行以下步骤
            result[i] <- s_temp[ids] %>%  # 以ids为索引提取s_temp中的元素，这个元素就是包含con的元素
                str_replace(con[i], '') %>%  # 将该元素中的con部分替换为空，比如“期望月薪：面议”变为“面议”
                iconv('utf-8', 'gbk', sub = '') # 处理编码格式的
        } else {
            result[i] <- NA # 如果ids为integer(0)，说明s_temp中没有含有con的元素，也就是这份简历的该字段缺失，赋值为NA
        }
    }
    return(result)
}

# url <- 'http://xa.58.com/qztech/pn'
# nodes <- c('span.name', 'dd.w70', 'div.xobxcontent')
# con <- c('期望月薪：', '期望职位：')
mainfunc <- function(url, nodes, con, start_page = 1, end_page = 5){
    urls <- paste0(url, start_page:end_page)
    df_all <- data.frame()
    
    for (url in urls) {
        content <- urlParse(url, nodes)
        name <- content[[1]] #需要根据nodes修改
        sex <- content[[2]][-1] #需要根据nodes修改
        
        temp <- content[[3]] %>% #需要根据nodes修改
            lapply(function(s)detailParse(s, con)) %>% 
            unlist
        
        salary <- temp[c(TRUE, FALSE)] #需要根据nodes修改
        title <- temp[c(FALSE, TRUE)] #需要根据nodes修改
        
        df <- data.frame(name, sex, salary, title)
        df_all <- rbind(df_all, df)
    }
    write.csv(df_all, 'df_all.csv')
}
