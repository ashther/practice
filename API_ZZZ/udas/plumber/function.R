
library(RMySQL)
library(dplyr)
library(tidyr)
library(memoise)

# HOME_PATH <- '/home/ashther/udas'
LOG_PATH <- file.path(HOME_PATH, 'logger')
LOG_LEVEL <- futile.logger::INFO
futile.logger::flog.threshold(LOG_LEVEL)
futile.logger::flog.appender(futile.logger::appender.file(LOG_PATH), 
                             name = 'api_udas')
layout_custom <- futile.logger::layout.format(
  '~l [~t] ~m', datetime.fmt = '%Y-%m-%d %H:%M:%OS3'
)
futile.logger::flog.layout(layout_custom, name = 'api_udas')

res_logger <- function(req, res, msg = NULL) {
  if (as.integer(res$status) == 200) {
    futile.logger::flog.info(
      '{"uuid":"%s","status":"%s","msg":"response successfully"}',
      req$cookies$uuid,
      res$status,
      name = 'api_udas'
    )
    plumber::forward()
  } else {
    futile.logger::flog.error(
      '{"uuid":"%s","status":"%s","msg":"%s"}',
      req$cookies$uuid,
      res$status,
      msg, 
      name = 'api_udas'
    )
    plumber::forward()
  }
}

area <- tidyr::unnest(tibble::enframe(list(
  '西北' = c('陕西省', '甘肃省', '青海省', '宁夏回族自治区', '新疆维吾尔自治区'), 
  '华东' = c('山东省', '江苏省', '安徽省', '浙江省', '江西省', '福建省', '上海市'), 
  '华北' = c('北京市', '天津市', '河北省', '山西省', '内蒙古自治区'), 
  '华中' = c('河南省', '湖南省', '湖北省'), 
  '华南' = c('广西壮族自治区', '广东省', '海南省'), 
  '西南' = c('西藏自治区', '四川省', '重庆市', '贵州省', '云南省'), 
  '东北' = c('黑龙江省', '吉林省', '辽宁省'), 
  '港澳台' = c('香港特别行政区', '澳门特别行政区', '台湾省')
)))

# the time-indicate column in df must be character type
yearFill <- function(df, col = NULL) {
  
  if (nrow(df) == 0) {
    return(df)
  }
  
  if (is.null(col)) {
    col <- colnames(df)[1]
  }
  
  year_max <- max(df[[col]])
  year_min <- min(df[[col]])
  full_year <- tibble(as.character(seq(year_min, year_max, 1)))
  colnames(full_year)[1] <- col
  
  left_join(full_year, df, by = col) %>% 
    mutate_all(.funs = replace_na, 0)
}

# original sql mustn't include neither `where` nor `and` in plain text, 
# except keywords
sqlFill <- function(sql) {
  if (!grepl(' WHERE ', sql, ignore.case = TRUE) & 
      grepl(' AND ', sql, ignore.case = TRUE)) {
    sql <- sub(' AND ', ' WHERE ', sql, ignore.case = TRUE)
  }
  sql
}

if(!is.memoised(dbGetQuery)) {
  dbGetQuery <- memoise(DBI::dbGetQuery, ~timeout(30))
}
